package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Resource, Stream}
import okay.given
import okay.codec.Schema
import okay.sql.{Granted, Isolation, Sql, SqlType, SqlValue, Typed}
import okay.sql.given
import java.sql.{DriverManager, SQLException}

/**
 * The JDBC road to Postgres (jdbc-tails): the probes the audit lanes
 * ran through the wire driver, now through pgjdbc — the driver most
 * deployments actually carry. Live: skips where the dockerized pg
 * (okay/okay/okay on 5432) is absent.
 */
class TestPgJdbc extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)
  val url = s"jdbc:postgresql://$host:$port/okay"

  lazy val available: Boolean =
    try { DriverManager.getConnection(url, "okay", "okay").close(); true }
    catch { case _: Throwable => false }

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(url, "okay", "okay")
    try f(JdbcSql(conn))
    finally conn.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def drain[A](p: Chunk[A] ! (Produce + Async)): Vector[A] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }

  def count(db: Sql, sql: String): Long =
    run(drain(db.query(sql, Vector.empty))).head.head match
      case SqlValue.I64(n) => n
      case other => fail(s"count: $other")

  test("a handled error inside a region: pgjdbc's COMMIT on the aborted transaction does not report success either") {
    assume(available, s"no Postgres at $url — the live suite skips")
    withDb { db =>
      run(db.update("drop table if exists jdbc_tag", Vector.empty)): Unit
      run(db.update("create table jdbc_tag(n int not null)", Vector.empty)): Unit
      val prog = Typed.transact[Long, Async](db, Isolation.ReadCommitted) { _ =>
        !.widen[Long, Async, Resource](db.update("insert into jdbc_tag values (1)", Vector.empty))
          .flatMap(_ => !.widen[Long, Async, Resource](okay.async {
            try run(db.update("select syntax error from", Vector.empty))
            catch { case _: SQLException => 0L }
          }))
      }
      val committed = try { run(Resource.run[Long, Async](prog)): Unit; true } catch { case _: SQLException => false }
      val rows = count(db, "select count(*) from jdbc_tag")
      assert(!committed || rows == 1L, "commit reported success and the row is absent: the rollback that does not roll back")
      assertEquals(rows, 0L)
    }
  }

  final case class Stamp(id: Int, at: java.time.Instant, plain: java.time.Instant, d: java.time.LocalDate,
                         t: java.time.LocalTime, ref: java.util.UUID, doc: String)
  given Schema[Stamp] = Schema.derived

  test("temporal types through pgjdbc: binds by the declared parameter type, jsonb through ?::jsonb, exact round trip under a non-UTC session zone") {
    assume(available, s"no Postgres at $url — the live suite skips")
    withDb { db =>
      run(db.update("drop table if exists jdbc_stamps", Vector.empty)): Unit
      run(db.update("create table jdbc_stamps(id int not null, at timestamptz not null, plain timestamp not null, " +
        "d date not null, t time not null, ref uuid not null, doc jsonb not null)", Vector.empty)): Unit
      run(db.update("set time zone 'Europe/Kyiv'", Vector.empty)): Unit
      val six = java.time.Instant.parse("2026-09-02T06:00:00Z")
      val one = Stamp(1, six.plusNanos(1000), java.time.Instant.parse("1969-12-31T23:59:59.999999Z"),
        java.time.LocalDate.of(1899, 12, 31), java.time.LocalTime.of(23, 59, 59, 999999000),
        java.util.UUID.fromString("6ba7b810-9dad-11d1-80b4-00c04fd430c8"), "{\"k\": [1, 2]}")
      assertEquals(run(Typed.update(db, "insert into jdbc_stamps values (?, ?, ?, ?, ?, ?, ?::jsonb)")(one)), 1L)
      val sql = "select id, at, plain, d, t, ref, doc from jdbc_stamps"
      assertEquals(run(db.describe(sql)).map(_.tpe), Vector(SqlType.I32, SqlType.Timestamp, SqlType.Timestamp,
        SqlType.Date, SqlType.Time, SqlType.Uuid, SqlType.Json))
      assertEquals(run(Typed.verify[Stamp](db, sql)), Vector.empty)
      val rows = run(drain(Typed.rows[Stamp](db, sql)))
      rows.head match
        case Right(s) =>
          assertEquals(s.copy(doc = ""), one.copy(doc = ""))
          assert(s.doc.replace(" ", "") == "{\"k\":[1,2]}", s.doc)
        case Left(bad) => fail(s"row did not decode: $bad")
      run(db.update("set time zone 'UTC'", Vector.empty)): Unit
    }
  }

  test("write skew through pgjdbc, raw: the loser's failure names 40001 and the connection is usable after the brake") {
    assume(available, s"no Postgres at $url — the live suite skips")
    val a = DriverManager.getConnection(url, "okay", "okay"); val b = DriverManager.getConnection(url, "okay", "okay")
    try
      val da = JdbcSql(a); val dbb = JdbcSql(b)
      run(da.update("drop table if exists jdbc_ssi0", Vector.empty)): Unit
      run(da.update("create table jdbc_ssi0(k int not null)", Vector.empty)): Unit
      val e = intercept[SQLException](run(Resource.run[Long, Async](Typed.transact[Long, Async](dbb, Isolation.Serializable) { _ =>
        !.widen[Long, Async, Resource](okay.async(count(dbb, "select count(*) from jdbc_ssi0")).flatMap { _ =>
          run(Resource.run[Long, Async](Typed.transact[Long, Async](da, Isolation.Serializable) { _ =>
            !.widen[Long, Async, Resource](okay.async(count(da, "select count(*) from jdbc_ssi0"))
              .flatMap(_ => da.update("insert into jdbc_ssi0 values (1)", Vector.empty)))
          })): Unit
          dbb.update("insert into jdbc_ssi0 values (2)", Vector.empty)
        })
      })))
      assertEquals(dbb.sqlState(e), Some("40001"), e.getMessage)
      assertEquals(run(Resource.run[Granted, Async](Typed.transact[Granted, Async](dbb)(g => okay.pure(g)))).granted, Isolation.ReadCommitted)
    finally { a.close(); b.close() }
  }

  test("write skew under Serializable through pgjdbc: the loser's SQLException carries 40001, and transactRetry lands it") {
    assume(available, s"no Postgres at $url — the live suite skips")
    val a = DriverManager.getConnection(url, "okay", "okay"); val b = DriverManager.getConnection(url, "okay", "okay")
    try
      val da = JdbcSql(a); val dbb = JdbcSql(b)
      run(da.update("drop table if exists jdbc_ssi", Vector.empty)): Unit
      run(da.update("create table jdbc_ssi(k int not null)", Vector.empty)): Unit
      def aWrites(): Unit = run(Resource.run[Long, Async](Typed.transact[Long, Async](da, Isolation.Serializable) { _ =>
        !.widen[Long, Async, Resource](okay.async(count(da, "select count(*) from jdbc_ssi"))
          .flatMap(_ => da.update("insert into jdbc_ssi values (1)", Vector.empty)))
      })): Unit
      var runs = 0
      val r = run(Typed.transactRetry(dbb, Isolation.Serializable, Typed.Retry(3)) { _ =>
        !.widen[Long, Async, Resource](okay.async(count(dbb, "select count(*) from jdbc_ssi")).flatMap { _ =>
          runs += 1
          if runs == 1 then aWrites()
          dbb.update("insert into jdbc_ssi values (2)", Vector.empty)
        })
      })
      assertEquals(r.attempts, 2)
      assertEquals(count(da, "select count(*) from jdbc_ssi"), 2L)
    finally { a.close(); b.close() }
  }
}
