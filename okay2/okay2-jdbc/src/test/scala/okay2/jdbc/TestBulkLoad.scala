package okay2.jdbc

import java.sql.DriverManager
import okay2.sql.{Sql, SqlValue}
import okay2.stream.Chunks

/**
 * WithKey at batch granularity, on the free engine (okay-jdbc's
 * TestBulkLoad): a load retried across a simulated crash lands ONCE
 * because the history's unique key recognizes the id; a failing COPY
 * rolls its claim back with it; and the OLAP posture refuses row DML by
 * name.
 */
class TestBulkLoad extends munit.FunSuite {

  def duck(): (java.sql.Connection, Sql) = {
    Class.forName("org.duckdb.DuckDBDriver")
    val c = DriverManager.getConnection("jdbc:duckdb:")
    val st = c.createStatement()
    st.execute("create table facts(id int, amount double)")
    st.close()
    (c, new JdbcSql(c))
  }

  def csv(rows: String*): String = {
    val f = java.nio.file.Files.createTempFile("okay2-bulk", ".csv")
    java.nio.file.Files.write(f, ("id,amount\n" + rows.mkString("\n")).getBytes("UTF-8"))
    f.toString
  }

  def count(c: java.sql.Connection): Int = {
    val rs = c.createStatement().executeQuery("select count(*) from facts")
    rs.next(); rs.getInt(1)
  }

  test("a load with a load id lands once — the retry after a crash finds the key") {
    val (c, db) = duck()
    val file = csv("1,10.5", "2,20.0", "3,3.25")
    val copy = s"copy facts from '$file' (header)"
    assertEquals(Run(BulkLoad.load(db, "load-2026-09-01-a", copy)), BulkLoad.Outcome.Loaded(3): BulkLoad.Outcome)
    // the crash was after commit; the retry re-runs the SAME call
    assertEquals(Run(BulkLoad.load(db, "load-2026-09-01-a", copy)), BulkLoad.Outcome.AlreadyLoaded: BulkLoad.Outcome)
    assertEquals(count(c), 3)
    // a NEW id is a new batch
    assertEquals(Run(BulkLoad.load(db, "load-2026-09-01-b", copy)), BulkLoad.Outcome.Loaded(3): BulkLoad.Outcome)
    assertEquals(count(c), 6)
    c.close()
  }

  test("a failing COPY rolls its claim back — the fixed retry starts clean, never half-loaded") {
    val (c, db) = duck()
    intercept[Exception](Run(BulkLoad.load(db, "load-x", "copy facts from '/no/such/file.csv' (header)"))): Unit
    assertEquals(count(c), 0)
    // the claim died with the transaction: the SAME id now loads
    val file = csv("7,7.0")
    assertEquals(Run(BulkLoad.load(db, "load-x", s"copy facts from '$file' (header)")),
      BulkLoad.Outcome.Loaded(1): BulkLoad.Outcome)
    assertEquals(count(c), 1)
    c.close()
  }

  test("the OLAP posture refuses row DML by name; reads and COPY pass") {
    val (c, db) = duck()
    val olap = BulkLoad.olap(db)
    val e = intercept[UnsupportedOperationException](Run(olap.update("insert into facts values (1, 1.0)")))
    assert(e.getMessage.contains("stage a file"), e.getMessage)
    intercept[UnsupportedOperationException](Run(olap.update("update facts set amount = 0"))): Unit
    intercept[UnsupportedOperationException](Run(olap.batch("insert into facts values (?, ?)", Chunks.emptyChunk[Vector[SqlValue]]))): Unit
    // the right doors stay open
    val file = csv("9,9.9")
    assertEquals(Run(BulkLoad.load(db, "load-olap", s"copy facts from '$file' (header)")),
      BulkLoad.Outcome.Loaded(1): BulkLoad.Outcome)
    assertEquals(Run(olap.describe("select * from facts")).length, 2)
    c.close()
  }
}
