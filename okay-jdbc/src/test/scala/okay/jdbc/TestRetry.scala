package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Resource, Stream}
import okay.given
import okay.sql.{Granted, Isolation, Sql, SqlValue, Typed}
import java.sql.{DriverManager, SQLException}

/**
 * specs/sql.md "Serialization failures are retried": the region runs
 * again on 40001/40P01 and on nothing else. The engine is H2, which
 * never raises 40001 on its own — so a decorator plays the engine
 * that chose this transaction to lose, N times, and counts what the
 * region did about it.
 */
class TestRetry extends munit.FunSuite {

  val url = "jdbc:h2:mem:retry;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit =
    val c = DriverManager.getConnection(url, "sa", "")
    try c.createStatement().execute("create table hits(n int not null)"): Unit
    finally c.close()

  /** fails the first `failures` updates with the given SQLSTATE */
  final class Losing(inner: Sql, var failures: Int, state: String) extends Sql:
    var begins = 0
    var cancels = 0
    def describe(sql: String) = inner.describe(sql)
    def query(sql: String, params: Vector[SqlValue]) = inner.query(sql, params)
    def update(sql: String, params: Vector[SqlValue]): Long ! Async =
      if failures > 0 then
        failures -= 1
        throw SQLException("could not serialize access (the engine chose this transaction to lose)", state)
      else inner.update(sql, params)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]) = inner.batch(sql, rows)
    def begin(isolation: Isolation, readOnly: Boolean) = { begins += 1; inner.begin(isolation, readOnly) }
    def commit() = inner.commit()
    def rollback() = inner.rollback()
    def cancel(): Unit = { cancels += 1; inner.cancel() }
    override def sqlState(t: Throwable) = inner.sqlState(t)

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def withDb[A](failures: Int, state: String)(f: Losing => A): A =
    val conn = DriverManager.getConnection(url, "sa", "")
    try
      run(JdbcSql(conn).update("delete from hits", Vector.empty)): Unit
      f(Losing(JdbcSql(conn), failures, state))
    finally conn.close()

  def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }

  def hits(db: Sql): Long =
    run(drain(db.query("select count(*) from hits", Vector.empty)).map(_.head.head)) match
      case SqlValue.I64(n) => n
      case other => fail(s"count: $other")

  def insert(db: Sql): Granted => Long ! (Resource + Async) =
    _ => !.widen[Long, Async, Resource](db.update("insert into hits values (1)", Vector.empty))

  test("N losses then success: Retry(N+1) answers the value with attempts = N+1, each loss rolled back") {
    withDb(2, "40001") { db =>
      val r = run(Typed.transactRetry(db, Isolation.Serializable, Typed.Retry(3))(insert(db)))
      assertEquals(r, Typed.Retried(1L, 3))
      assertEquals(db.begins, 3)
      // the brake runs at EVERY scope exit — a no-op after the commit —
      // so three runs are three brakes; that only one row exists is
      // what proves the two lost runs rolled back
      assertEquals(db.cancels, 3)
      assertEquals(hits(db), 1L, "committed exactly once")
    }
  }

  test("Retry(N) against N losses propagates the N-th failure, with its SQLSTATE") {
    withDb(2, "40001") { db =>
      val e = intercept[SQLException](run(Typed.transactRetry(db, Isolation.Serializable, Typed.Retry(2))(insert(db))))
      assertEquals(e.getSQLState, "40001")
      assertEquals(db.begins, 2)
      assertEquals(hits(db), 0L)
    }
  }

  test("a deadlock (40P01) is retried too; a unique violation (23505) is not — it propagates on the first run") {
    withDb(1, "40P01") { db =>
      assertEquals(run(Typed.transactRetry(db, Isolation.ReadCommitted, Typed.Retry(2))(insert(db))).attempts, 2)
    }
    withDb(1, "23505") { db =>
      val e = intercept[SQLException](run(Typed.transactRetry(db, Isolation.ReadCommitted, Typed.Retry(5))(insert(db))))
      assertEquals(e.getSQLState, "23505")
      assertEquals(db.begins, 1, "not a conflict: no second run")
      assertEquals(db.cancels, 1)
    }
  }

  test("the default Retry.none runs once, and the backoff is consulted with the run number") {
    withDb(1, "40001") { db =>
      val _ = intercept[SQLException](run(Typed.transactRetry(db)(insert(db))))
      assertEquals(db.begins, 1)
    }
    withDb(2, "40001") { db =>
      var asked = List.empty[Int]
      val r = run(Typed.transactRetry(db, retry = Typed.Retry(3, n => { asked ::= n; 1L }))(insert(db)))
      assertEquals(r.attempts, 3)
      assertEquals(asked.reverse, List(1, 2))
    }
  }
}
