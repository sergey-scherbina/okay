package okay2.jdbc

import java.sql.{DriverManager, SQLException}
import okay2.{!, +, Resource}
import okay2.async.Async
import okay2.platform._
import okay2.sql.{Col, Granted, Isolation, Sql, SqlValue, Typed}
import okay2.stream.{Chunk, Source}

/** "Serialization failures are retried" (okay-jdbc's TestRetry): the
 * region runs again on 40001/40P01 and on nothing else. H2 never raises
 * 40001 on its own, so a decorator plays the engine that chose this
 * transaction to lose, N times, and counts what the region did. */
class TestRetry extends munit.FunSuite {

  val url = "jdbc:h2:mem:retry;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit = {
    val c = DriverManager.getConnection(url, "sa", "")
    try c.createStatement().execute("create table hits(n int not null)"): Unit
    finally c.close()
  }

  /** fails the first `failures` updates with the given SQLSTATE */
  final class Losing(inner: Sql, var failures: Int, state: String) extends Sql {
    var begins = 0
    var cancels = 0
    def describe(sql: String): Vector[Col] ! Async = inner.describe(sql)
    def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] = inner.query(sql, params)
    def update(sql: String, params: Vector[SqlValue]): Long ! Async =
      if (failures > 0) {
        failures -= 1
        throw new SQLException("could not serialize access (the engine chose this transaction to lose)", state)
      } else inner.update(sql, params)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = inner.batch(sql, rows)
    def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = { begins += 1; inner.begin(isolation, readOnly) }
    def commit(): Unit ! Async = inner.commit()
    def rollback(): Unit ! Async = inner.rollback()
    def cancel(): Unit = { cancels += 1; inner.cancel() }
    override def sqlState(t: Throwable): Option[String] = inner.sqlState(t)
  }

  def withDb[A](failures: Int, state: String)(f: Losing => A): A = {
    val conn = DriverManager.getConnection(url, "sa", "")
    try {
      Run(new JdbcSql(conn).update("delete from hits", Vector.empty)): Unit
      f(new Losing(new JdbcSql(conn), failures, state))
    } finally conn.close()
  }

  def hits(db: Sql): Long =
    Run(Source.concat(db.query("select count(*) from hits", Vector.empty)).map(_.head.head)) match {
      case SqlValue.I64(n) => n
      case other => fail(s"count: $other")
    }

  def insert(db: Sql): Granted => Long ! (Resource + Async) =
    _ => db.update("insert into hits values (1)", Vector.empty)

  test("N losses then success: Retry(N+1) answers the value with attempts = N+1, each loss rolled back") {
    withDb(2, "40001") { db =>
      val r = Run(Typed.transactRetry(db, Isolation.Serializable, Typed.Retry(3))(insert(db)))
      assertEquals(r, Typed.Retried(1L, 3))
      assertEquals(db.begins, 3)
      // the brake runs at EVERY scope exit (a no-op after the commit)
      assertEquals(db.cancels, 3)
      assertEquals(hits(db), 1L, "committed exactly once")
    }
  }

  test("Retry(N) against N losses propagates the N-th failure, with its SQLSTATE") {
    withDb(2, "40001") { db =>
      val e = intercept[SQLException](Run(Typed.transactRetry(db, Isolation.Serializable, Typed.Retry(2))(insert(db))))
      assertEquals(e.getSQLState, "40001")
      assertEquals(db.begins, 2)
      assertEquals(hits(db), 0L)
    }
  }

  test("a deadlock (40P01) is retried too; a unique violation (23505) is not") {
    withDb(1, "40P01") { db =>
      assertEquals(Run(Typed.transactRetry(db, Isolation.ReadCommitted, Typed.Retry(2))(insert(db))).attempts, 2)
    }
    withDb(1, "23505") { db =>
      val e = intercept[SQLException](Run(Typed.transactRetry(db, Isolation.ReadCommitted, Typed.Retry(5))(insert(db))))
      assertEquals(e.getSQLState, "23505")
      assertEquals(db.begins, 1, "not a conflict: no second run")
      assertEquals(db.cancels, 1)
    }
  }

  test("the default Retry.none runs once, and the backoff is consulted with the run number") {
    withDb(1, "40001") { db =>
      val _ = intercept[SQLException](Run(Typed.transactRetry(db)(insert(db))))
      assertEquals(db.begins, 1)
    }
    withDb(2, "40001") { db =>
      var asked = List.empty[Int]
      val r = Run(Typed.transactRetry(db, retry = Typed.Retry(3, n => { asked ::= n; 1L }))(insert(db)))
      assertEquals(r.attempts, 3)
      assertEquals(asked.reverse, List(1, 2))
    }
  }
}
