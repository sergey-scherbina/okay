package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.sql.SqlValue

/**
 * COPY through the wire (specs/sql.md's box) and the load-id
 * posture on the free engine: the retry after a crash between
 * journal and commit lands ONCE, because the registry row and the
 * data commit together. Live; skips where Postgres is absent.
 */
class TestCopy extends munit.FunSuite {

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def connect(): PgSql = okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))

  lazy val available: Boolean =
    try { connect().close(); true }
    catch { case _: Throwable => false }

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def withDb[A](f: PgSql => A): A =
    val db = connect()
    try f(db)
    finally db.close()

  override def beforeAll(): Unit =
    if !available then return
    withDb { db =>
      run(db.update("drop table if exists bulk"))
      run(db.update("create table bulk(id bigint not null, label text, amount double precision)"))
      run(db.update("drop table if exists okay_loads"))
      run(Load.ensure(db))
      ()
    }

  def countBulk(db: PgSql): Long =
    import okay.!.*
    def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] =
      def go(rest: Chunk[Vector[SqlValue]] ! (Produce + Async), acc: Vector[Vector[SqlValue]]): Vector[Vector[SqlValue]] =
        (rest.resume: @unchecked) match
          case Pure(_) => acc
          case Effect(e) => okay.<|>[Async, Produce](e) match
            case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc
            case Right(c) => acc ++ c.asInstanceOf[Chunk[Vector[SqlValue]]]
          case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
            case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
            case Right(c) => go(k(c), acc ++ c.asInstanceOf[Chunk[Vector[SqlValue]]])
      go(p, Vector.empty)
    drain(db.query("select count(*) from bulk")).head.head match
      case SqlValue.I64(n) => n
      case other => fail(s"expected a count, got $other")

  test("copyIn streams a thousand rows in one command; special characters survive the text format") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      run(db.update("truncate bulk"))
      val rows = (1 to 1000).iterator.map(i =>
        PgSql.copyRow(Vector(SqlValue.I64(i), SqlValue.Text(s"row-$i"), SqlValue.F64(i / 2.0))))
      assertEquals(run(db.copyIn("copy bulk (id, label, amount) from stdin", rows)), 1000L)
      assertEquals(countBulk(db), 1000L)

      // the escapes: tab, newline, backslash, NULL — round-trip
      val tricky = Vector(
        Vector(SqlValue.I64(2001), SqlValue.Text("tab\there"), SqlValue.Null),
        Vector(SqlValue.I64(2002), SqlValue.Text("line\nbreak"), SqlValue.F64(1.0)),
        Vector(SqlValue.I64(2003), SqlValue.Text("back\\slash"), SqlValue.F64(2.0)),
      )
      assertEquals(run(db.copyIn("copy bulk (id, label, amount) from stdin",
        tricky.iterator.map(PgSql.copyRow))), 3L)
      final case class Row(label: Option[String])
      given okay.codec.Schema[Row] = okay.codec.Schema.derived
      import okay.sql.Typed
      val back = {
        import okay.!.*
        def drain[A](p: Chunk[A] ! (Produce + Async)): Vector[A] =
          def go(rest: Chunk[A] ! (Produce + Async), acc: Vector[A]): Vector[A] =
            (rest.resume: @unchecked) match
              case Pure(_) => acc
              case Effect(e) => okay.<|>[Async, Produce](e) match
                case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc
                case Right(c) => acc ++ c.asInstanceOf[Chunk[A]]
              case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
                case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
                case Right(c) => go(k(c), acc ++ c.asInstanceOf[Chunk[A]])
          go(p, Vector.empty)
        drain(Typed.rows[Row](db, "select label from bulk where id >= 2001 order by id"))
      }
      assertEquals(back.collect { case Right(r) => r.label },
        Vector(Some("tab\there"), Some("line\nbreak"), Some("back\\slash")))
    }
  }

  test("the load id dedups: the same load twice lands once, and says so") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      run(db.update("truncate bulk"))
      run(db.update("delete from okay_loads"))
      val rows = (1 to 50).toVector.map(i =>
        Vector[SqlValue](SqlValue.I64(i), SqlValue.Text(s"r$i"), SqlValue.F64(1.0)))
      assertEquals(run(Load.load(db, "batch-2026-09-01", "bulk",
        Vector("id", "label", "amount"), rows)), Load.Result.Loaded(50L))
      // the retry — the ack was lost, the caller loads again
      assertEquals(run(Load.load(db, "batch-2026-09-01", "bulk",
        Vector("id", "label", "amount"), rows)), Load.Result.AlreadyLoaded)
      assertEquals(countBulk(db), 50L)
    }
  }

  test("a crash between COPY and commit rolls back the CLAIM too: the retry lands, once overall") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db => run(db.update("truncate bulk")); run(db.update("delete from okay_loads")); () }
    val rows = (1 to 20).toVector.map(i =>
      Vector[SqlValue](SqlValue.I64(i), SqlValue.Text(s"r$i"), SqlValue.F64(1.0)))

    // the crash: claim + COPY, then the connection DIES uncommitted
    val dying = connect()
    run(dying.begin(okay.sql.Isolation.ReadCommitted))
    assertEquals(run(dying.update(
      "insert into okay_loads(load_id) values ($1) on conflict do nothing",
      Vector(SqlValue.Text("batch-crash")))), 1L)
    assertEquals(run(dying.copyIn("copy bulk (id, label, amount) from stdin",
      rows.iterator.map(PgSql.copyRow))), 20L)
    dying.close() // no commit: the server rolls back claim AND data together

    withDb { db =>
      assertEquals(countBulk(db), 0L, "uncommitted COPY data survived the crash")
      // the retry on a fresh connection: the claim rolled back with
      // the data, so the load runs — and lands exactly once overall
      assertEquals(run(Load.load(db, "batch-crash", "bulk",
        Vector("id", "label", "amount"), rows)), Load.Result.Loaded(20L))
      assertEquals(countBulk(db), 20L)
    }
  }
}
