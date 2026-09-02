package okay.jdbc

import okay.{!, Async}
import okay.given
import okay.sql.Sql
import java.sql.DriverManager

/**
 * WithKey at batch granularity, on the free engine: a load retried
 * across a simulated crash lands ONCE because the history's unique
 * key recognizes the id; a failing COPY rolls its claim back with
 * it; and the OLAP posture refuses row DML by name.
 */
class TestBulkLoad extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  def duck(): (java.sql.Connection, Sql) =
    Class.forName("org.duckdb.DuckDBDriver")
    val c = DriverManager.getConnection("jdbc:duckdb:")
    val st = c.createStatement()
    st.execute("create table facts(id int, amount double)")
    st.close()
    (c, JdbcSql(c))

  def csv(rows: String*): String =
    val f = java.nio.file.Files.createTempFile("okay-bulk", ".csv")
    java.nio.file.Files.write(f, ("id,amount\n" + rows.mkString("\n")).getBytes)
    f.toString

  def count(c: java.sql.Connection): Int =
    val rs = c.createStatement().executeQuery("select count(*) from facts")
    rs.next(); rs.getInt(1)

  test("a load with a load id lands once — the retry after a crash finds the key") {
    val (c, db) = duck()
    val file = csv("1,10.5", "2,20.0", "3,3.25")
    val copy = s"copy facts from '$file' (header)"
    assertEquals(run(BulkLoad.load(db, "load-2026-09-01-a", copy)),
      BulkLoad.Outcome.Loaded(3))
    // the crash was after commit; the retry re-runs the SAME call
    assertEquals(run(BulkLoad.load(db, "load-2026-09-01-a", copy)),
      BulkLoad.Outcome.AlreadyLoaded)
    assertEquals(count(c), 3)
    // a NEW id is a new batch
    assertEquals(run(BulkLoad.load(db, "load-2026-09-01-b", copy)),
      BulkLoad.Outcome.Loaded(3))
    assertEquals(count(c), 6)
  }

  test("a failing COPY rolls its claim back — the fixed retry starts clean, never half-loaded") {
    val (c, db) = duck()
    intercept[Exception](run(BulkLoad.load(db, "load-x",
      "copy facts from '/no/such/file.csv' (header)"))): Unit
    assertEquals(count(c), 0)
    // the claim died with the transaction: the SAME id now loads
    val file = csv("7,7.0")
    assertEquals(run(BulkLoad.load(db, "load-x", s"copy facts from '$file' (header)")),
      BulkLoad.Outcome.Loaded(1))
    assertEquals(count(c), 1)
  }

  test("the OLAP posture refuses row DML by name; reads and COPY pass") {
    val (_, db) = duck()
    val olap = BulkLoad.olap(db)
    val e = intercept[UnsupportedOperationException](
      run(olap.update("insert into facts values (1, 1.0)")))
    assert(e.getMessage.contains("stage a file"), e.getMessage)
    intercept[UnsupportedOperationException](
      run(olap.update("update facts set amount = 0"))): Unit
    intercept[UnsupportedOperationException](
      run(olap.batch("insert into facts values (?, ?)", okay.Chunks.emptyChunk))): Unit
    // the right doors stay open
    val file = csv("9,9.9")
    assertEquals(run(BulkLoad.load(db, "load-olap", s"copy facts from '$file' (header)")),
      BulkLoad.Outcome.Loaded(1))
    assertEquals(run(olap.describe("select * from facts")).length, 2)
  }
}
