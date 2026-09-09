package okay.docs.dynamo

import okay.{!, +, Async, Chunk, Produce, Resource, Stream}
import okay.given
import okay.crypto.given
import okay.blob.SigV4
import okay.codec.{Json, Schema}
import okay.docs.{Cond, PutResult}
import okay.persist.{MemoryStore, Saga}
import okay.pg.PgSql
import okay.sql.{Isolation, Pool, Sql, SqlValue, Typed}

/**
 * The seven persistence-audit lanes TOGETHER (persistence-e2e): a Pool
 * over the pg wire lends connections to transactRetry under a real
 * write skew; a Saga whose steps are DynamoDocs CAS writes crosses the
 * crash window and recovers forward on the far end's idempotency. Live:
 * skips where the dockerized pg (5432) or dynamodb-local (8000) is absent.
 */
class TestPersistenceE2E extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val pgHost = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val pgPort = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)
  val dynamo = sys.env.getOrElse("OKAY_DYNAMO", "http://127.0.0.1:8000")

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))
  def connect(): PgSql ! Async = PgSql.connect(pgHost, pgPort, "okay", "okay", "okay")

  lazy val pgUp: Boolean = try { run(connect()).close(); true } catch { case _: Throwable => false }
  lazy val dynamoUp: Boolean =
    try
      val u = java.net.URI(dynamo); val s = java.net.Socket()
      s.connect(java.net.InetSocketAddress(u.getHost, u.getPort), 300); s.close(); true
    catch case _: Exception => false

  def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }
  def count(db: Sql, table: String): Long ! Async =
    drain(db.query(s"select count(*) from $table")).map(_.head.head match
      case SqlValue.I64(n) => n
      case other => throw AssertionError(s"count: $other"))

  test("a Pool over the pg wire lends to transactRetry: write skew between two borrowed connections, the loser re-runs, the pool is whole after") {
    assume(pgUp, s"no Postgres at $pgHost:$pgPort — the live suite skips")
    val pool = Pool[PgSql](2, 5000L)(() => connect())(_.close())
    run(pool.borrow(db => !.widen[Long, Async, Resource](
      db.update("drop table if exists e2e_ssi").flatMap(_ => db.update("create table e2e_ssi(k int not null)"))))): Unit
    var runs = 0
    // b's first run: read, then a whole transaction on ANOTHER borrowed
    // connection between b's read and b's write — the rw-conflict cycle
    def aWrites(): Unit = run(pool.borrow(a => !.widen[Long, Async, Resource](Resource.run[Long, Async](
      Typed.transact[Long, Async](a, Isolation.Serializable)(_ =>
        !.widen[Long, Async, Resource](count(a, "e2e_ssi").flatMap(_ => a.update("insert into e2e_ssi values (1)")))))))): Unit
    val r = run(pool.borrow(b => !.widen[Typed.Retried[Long], Async, Resource](
      Typed.transactRetry(b, Isolation.Serializable, Typed.Retry(3)) { _ =>
        !.widen[Long, Async, Resource](count(b, "e2e_ssi").flatMap { _ =>
          runs += 1
          if runs == 1 then aWrites()
          b.update("insert into e2e_ssi values (2)")
        })
      })))
    assertEquals(r.attempts, 2, "the first run lost to a, the second landed")
    assertEquals(run(pool.borrow(db => !.widen[Long, Async, Resource](count(db, "e2e_ssi")))), 2L)
    val st = pool.stats
    assertEquals((st.busy, st.waiting, st.closed), (0, 0, false))
    assert(st.created <= 2 && st.idle == st.created, s"$st")
    assert(Json.write(st).contains("\"busy\":0"), Json.write(st))
    pool.close()
  }

  final case class Order(id: String, version: Option[Long], log: Vector[String]) derives Schema
  final case class Doc(state: String) derives Schema

  test("a Saga whose steps are DynamoDocs CAS writes: the crash window recovers forward because the far end is idempotent, and the status renders") {
    assume(dynamoUp, s"no dynamodb-local at $dynamo — the live suite skips")
    val docs = DynamoDocs[Doc](dynamo, "us-east-1", SigV4.Creds("local", "local"), s"e2e_${System.nanoTime()}", Map.empty)
    docs.ensure()
    val topic = MemoryStore().topic("sagas")
    var halt = true
    /** a CAS step: Applied advances the state; Stale means it already
     * happened (the crash window's re-run) and is the SAME answer */
    def cas(name: String, next: String)(cond: Order => Cond): Saga.Step[Order] = Saga.Step[Order](name,
      forward = o => docs.put(o.id, Doc(next), cond(o)).map {
        case PutResult.Applied(v) => o.copy(version = Some(v), log = o.log :+ name)
        case PutResult.Stale(cur) => o.copy(version = cur, log = o.log :+ s"$name (already)")
      }.flatMap(o2 => if name == "ship" && halt then throw Saga.Halt() else okay.pure(o2)),
      compensate = o => docs.put(o.id, Doc(s"un-$next"), Cond.Always).map(_ => o.copy(log = o.log :+ s"undo $name")))
    def saga() = Saga[Order](topic, "ord-1")(
      cas("reserve", "reserved")(_ => Cond.IfAbsent),
      cas("charge", "charged")(o => o.version.fold(Cond.Always)(Cond.IfVersion(_))),
      cas("ship", "shipped")(o => o.version.fold(Cond.Always)(Cond.IfVersion(_))))
    val _ = intercept[Saga.Halt](run(saga().run(Order("ord-1", None, Vector.empty))))
    assertEquals(run(docs.get("ord-1")).map(_.value), Some(Doc("shipped")), "the effect happened before the process died")
    val standing = saga().status
    assertEquals((standing.phase, standing.done, standing.pending), ("running", Vector("reserve", "charge"), Some("ship")))
    halt = false
    val out = run(saga().recover())
    out match
      case Saga.Outcome.Finished(o) => assertEquals(o.log, Vector("reserve", "charge", "ship (already)"))
      case other => fail(s"expected Finished, got $other")
    assertEquals(run(docs.get("ord-1")).map(_.value), Some(Doc("shipped")))
    assert(Json.write(saga().status).contains("\"phase\":\"finished\""), Json.write(saga().status))
  }
