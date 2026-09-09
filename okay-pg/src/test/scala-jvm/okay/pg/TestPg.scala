package okay.pg

import okay.{!, %, +, Async, Chunk, Handler, Produce, Resource, Stream, Throws, effect}
import okay.given
import okay.crypto.given
import okay.codec.Schema
import okay.sql.{Granted, Isolation, Sql, SqlType, SqlValue, Typed}
import okay.sql.given

/**
 * The wire against a REAL Postgres (the live-suite pattern: skips
 * where the endpoint is absent — a local server or the okay-pg
 * docker container, okay/okay/okay on 5432). SCRAM is proven by
 * connecting at all: the container authenticates scram-sha-256
 * and nothing else.
 */
class TestPg extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  def connect(): PgSql = okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))

  lazy val available: Boolean =
    try { connect().close(); true }
    catch { case _: Throwable => false }

  override def beforeAll(): Unit =
    if !available then return
    val db = connect()
    try
      run(db.update("drop table if exists customer")): Unit
      run(db.update("""create table customer(
        id bigint primary key not null,
        user_name varchar(64) not null,
        age int,
        balance double precision not null,
        active boolean not null,
        avatar bytea)""")): Unit
      run(db.update("insert into customer values " +
        "(1, 'ann', 25, 10.5, true, '\\x0102')," +
        "(2, 'bob', null, -3.25, false, null)")): Unit
      run(db.update("drop table if exists big")): Unit
      run(db.update("create table big(n int not null, label varchar(32) not null)")): Unit
      run(db.update("insert into big select g, 'row-' || g from generate_series(1, 500) g")): Unit
      ()
    finally db.close()

  def withDb[A](f: Sql => A): A =
    val db = connect()
    try f(db)
    finally db.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), acc: List[Chunk[A]]): List[Chunk[A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc.reverse
          case Right(c) => (c.asInstanceOf[Chunk[A]] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), c.asInstanceOf[Chunk[A]] :: acc)
    go(s, Nil)

  final case class Customer(id: Long, userName: String, age: Option[Int],
                            balance: Double, active: Boolean,
                            avatar: Option[Array[Byte]])
  given Schema[Customer] = Schema.derived

  test("startup + SCRAM-SHA-256 lands a working session (and a bad password refuses)") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val one = collectChunks(db.query("select 1")).flatten
      assertEquals(one, List(Vector(SqlValue.I32(1))))
    }
    intercept[PgError](okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "wrong-password", "okay"))))
  }

  test("the typed layer runs over the wire: rows by label, verify with catalog nullability") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      // pg_attribute answers nullability, so a clean verify needs no
      // Option-everything concession here — unlike the Parquet road
      assertEquals(run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = collectChunks(Typed.rows[Customer](db, "select * from customer order by id")).flatten
      assertEquals(rs.length, 2)
      val ann = rs.head.toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      assertEquals(rs(1).toOption.get.age, None)

      val drifts = run(Typed.verify[Customer](db,
        "select id, age, balance, active, avatar from customer"))
      assertEquals(drifts.map(_.column), Vector("user_name"))
    }
  }

  test("portal streaming: 500 rows arrive at fetch-size chunks — the protocol IS the fetch-size story") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      final case class Row(n: Int, label: String)
      given Schema[Row] = Schema.derived
      val chunks = collectChunks(Typed.rows[Row](db, "select * from big order by n"))
      assertEquals(chunks.map(_.length), List(64, 64, 64, 64, 64, 64, 64, 52))
      assertEquals(chunks.flatten.collect { case Right(r) => r.n }.take(3), List(1, 2, 3))
      assert(chunks.flatten.forall(_.isRight))
    }
  }

  test("params bind, updates count, an error names itself and the session survives") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
      given Schema[NewRow] = Schema.derived
      assertEquals(run(Typed.update(db,
        "insert into customer(id, user_name, balance, active) values ($1, $2, $3, $4)")(
        NewRow(10, "dee", 5.0, true))), 1L)
      val e = intercept[PgError](run(db.update("select syntax error from")))
      assert(e.getMessage.nonEmpty)
      // the connection reached quiet and keeps working
      assertEquals(run(db.update("delete from customer where id = 10")), 1L)
    }
  }

  test("transact over the wire: granted isolation read back; an abort rolls back") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val g = !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](
        Typed.transact[Granted, Async](db, Isolation.RepeatableRead)(g => okay.pure(g)))))
      assertEquals(g.granted, Isolation.RepeatableRead)
      assert(!g.downgraded)

      type G = Throws % String
      val prog = Typed.transact[Long, G](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource + G](
          db.update("insert into customer(id, user_name, balance, active) values (30, 'tx', 1, true)"))
          .flatMap(_ => effect[Resource + Async + G, Long](Throws("no")))
      }
      val out = !.run(Async.run[Either[String, Long], Nothing](
        Resource.run[Either[String, Long], Async](
          okay.runEither[Long, Resource + Async, String](prog))))
      assertEquals(out, Left("no"))
      val n = collectChunks(db.query("select count(*) from customer where id = 30")).flatten
      assertEquals(n.head.head, SqlValue.I64(0), "the insert survived the abort")
    }
  }

  test("a handled error inside a region: pg's COMMIT answers ROLLBACK, and the region must not report success (sql-commit-tag)") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      // the body inserts, then RECOVERS from a failed statement (the
      // nested blocking run is what any program that catches the
      // PgError does); pg is now in the aborted state, and its COMMIT
      // answers the tag ROLLBACK with no error at all
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource](
          db.update("insert into customer(id, user_name, balance, active) values (31, 'tag', 1, true)"))
          .flatMap(_ => !.widen[Long, Async, Resource](okay.async {
            try run(db.update("select syntax error from"))
            catch { case _: PgError => 0L }
          }))
      }
      val e = intercept[PgError](!.run(Async.run[Long, Nothing](Resource.run[Long, Async](prog))))
      assert(e.getMessage.contains("ROLLBACK"), e.getMessage)
      val n = collectChunks(db.query("select count(*) from customer where id = 31")).flatten
      assertEquals(n.head.head, SqlValue.I64(0), "the aborted transaction's insert is not there")
      // and the connection is usable afterwards, outside any transaction
      assertEquals(run(db.update("delete from customer where id = 31")), 0L)
    }
  }

  test("write skew under Serializable: the loser gets 40001 through the wire, and transactRetry re-runs it (sql-serialization-retry)") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    val a = connect(); val b = connect()
    try
      run(a.update("drop table if exists ssi")): Unit
      run(a.update("create table ssi(k int not null)")): Unit
      def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
        val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
        S.uncons(p).flatMap {
          case None => okay.pure(Vector.empty)
          case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
        }
      def count(db: Sql): Long ! Async =
        drain(db.query("select count(*) from ssi")).map(_.head.head match
          case SqlValue.I64(n) => n
          case other => throw AssertionError(s"count: $other"))
      // a's whole transaction, run to completion INSIDE b's first run,
      // between b's read and b's write — the classic rw-conflict cycle
      def aWrites(): Unit =
        !.run(Async.run[Long, Nothing](Resource.run[Long, Async](
          Typed.transact[Long, Async](a, Isolation.Serializable) { _ =>
            !.widen[Long, Async, Resource](count(a).flatMap(_ => a.update("insert into ssi values (1)")))
          }))): Unit
      var runs = 0
      val r = run(Typed.transactRetry(b, Isolation.Serializable, Typed.Retry(3)) { _ =>
        !.widen[Long, Async, Resource](count(b).flatMap { _ =>
          runs += 1
          if runs == 1 then aWrites()
          b.update("insert into ssi values (2)")
        })
      })
      assertEquals(r.attempts, 2, "the first run lost to a, the second landed")
      assertEquals(run(count(b)), 2L)
      // and the loser's failure, seen raw, is the SQLSTATE the retry keys on
      run(b.update("delete from ssi")): Unit
      runs = 0
      val e = intercept[PgError](run(Typed.transactRetry(b, Isolation.Serializable) { _ =>
        !.widen[Long, Async, Resource](count(b).flatMap { _ =>
          runs += 1
          if runs == 1 then aWrites()
          b.update("insert into ssi values (2)")
        })
      }))
      assertEquals(b.sqlState(e), Some("40001"), e.getMessage)
    finally { a.close(); b.close() }
  }

  final case class Stamp(id: Int, at: java.time.Instant, plain: java.time.Instant, d: java.time.LocalDate,
                         t: java.time.LocalTime, ref: java.util.UUID, doc: String, ats: Vector[java.time.Instant])
  given Schema[Stamp] = Schema.derived

  test("sql-temporal-types over the wire: timestamptz/timestamp/date/time/uuid/jsonb and a timestamptz[] read typed, bind back exact, verify clean") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      run(db.update("drop table if exists stamps")): Unit
      run(db.update("create table stamps(id int not null, at timestamptz not null, plain timestamp not null, " +
        "d date not null, t time not null, ref uuid not null, doc jsonb not null, ats timestamptz[] not null)")): Unit
      // the session zone is whatever the server has; the offset on the wire is applied, not assumed
      run(db.update("set time zone 'Europe/Kyiv'")): Unit
      run(db.update("insert into stamps values (1, '2026-09-02 06:00:00+00', '2026-09-02 06:00:00', '2026-09-02', " +
        "'06:00:00.5', '6ba7b810-9dad-11d1-80b4-00c04fd430c8', '{\"k\": [1, 2]}', " +
        "array['2026-09-02 06:00:00+00', '1969-12-31 23:59:59.999999+00']::timestamptz[])")): Unit
      val six = java.time.Instant.parse("2026-09-02T06:00:00Z")
      val one = Stamp(1, six, six, java.time.LocalDate.of(2026, 9, 2), java.time.LocalTime.of(6, 0, 0, 500000000),
        java.util.UUID.fromString("6ba7b810-9dad-11d1-80b4-00c04fd430c8"), "{\"k\": [1, 2]}",
        Vector(six, java.time.Instant.parse("1969-12-31T23:59:59.999999Z")))
      val sql = "select id, at, plain, d, t, ref, doc, ats from stamps order by id"
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector(SqlType.I32) ++
        Vector(SqlType.Timestamp, SqlType.Timestamp, SqlType.Date, SqlType.Time, SqlType.Uuid, SqlType.Json, SqlType.Arr(SqlType.Timestamp)))
      assertEquals(run(Typed.verify[Stamp](db, sql)), Vector.empty)
      assertEquals(collectChunks(Typed.rows[Stamp](db, sql)).flatten, List(Right(one)))
      val two = one.copy(id = 2, at = six.plusNanos(1000), plain = java.time.Instant.parse("1969-12-31T23:59:59.999999Z"),
        d = java.time.LocalDate.of(1899, 12, 31), t = java.time.LocalTime.of(23, 59, 59, 999999000),
        ref = java.util.UUID.randomUUID(), doc = "{\"z\": true}", ats = Vector.empty)
      assertEquals(run(Typed.update(db, "insert into stamps values ($1, $2, $3, $4, $5, $6, $7, $8)")(two)), 1L)
      assertEquals(collectChunks(Typed.rows[Stamp](db, sql)).flatten, List(Right(one), Right(two)))
      run(db.update("set time zone 'UTC'")): Unit
    }
  }

  test("nested transact refuses loudly on the wire too") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      val prog = Typed.transact[Granted, Async](db) { _ =>
        Typed.transact[Granted, Async](db)(g2 => okay.pure(g2))
      }
      val e = intercept[IllegalStateException](
        !.run(Async.run[Granted, Nothing](Resource.run[Granted, Async](prog))))
      assert(e.getMessage.contains("nested"))
    }
  }

  test("batch: one parse, many binds, summed counts") {
    assume(available, s"no Postgres at $host:$port — the live suite skips")
    withDb { db =>
      run(db.update("drop table if exists batched")): Unit
      run(db.update("create table batched(n int not null)")): Unit
      val rows = okay.ChunkBuf.of((1 to 10).map(i => Vector[SqlValue](SqlValue.I32(i))))
      assertEquals(run(db.batch("insert into batched values ($1)", rows)), 10L)
      val n = collectChunks(db.query("select count(*) from batched")).flatten
      assertEquals(n.head.head, SqlValue.I64(10))
    }
  }
}
