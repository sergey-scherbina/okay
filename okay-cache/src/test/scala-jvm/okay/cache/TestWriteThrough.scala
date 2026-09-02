package okay.cache

import okay.{!, +, Async, Produce, Stream}
import okay.given
import okay.jdbc.JdbcSql
import okay.sql.{Sql, SqlValue, Typed}
import okay.codec.Schema
import java.sql.DriverManager

/**
 * The last open cache.md box, argued three ways over a REAL database
 * through the Sql seam: the ordering is asserted, the WRONG ordering's
 * resurrection bug is demonstrated (that is why the ordering is the
 * rule), and the honest commit-to-invalidate window is shown rather
 * than denied.
 */
class TestWriteThrough extends munit.FunSuite {

  final case class Row(price: Long)
  given Schema[Row] = Schema.derived

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  private var n = 0
  def fixture(): (Sql, Cache[String, Long]) =
    n += 1
    Class.forName("org.h2.Driver")
    val conn = DriverManager.getConnection(s"jdbc:h2:mem:wt$n;DB_CLOSE_DELAY=-1")
    val st = conn.createStatement()
    st.execute("create table price(id varchar(16) primary key, price bigint not null)")
    st.execute("insert into price values ('okay', 100)")
    st.close()
    (JdbcSql(conn), Cache.memory[String, Long](Regime.Invalidated, 64))

  def load(db: Sql)(k: String): Long ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(db.query("select price from price where id = ?",
        Vector(SqlValue.Text(k)))).map {
      case Some((c, _)) if c.nonEmpty => c(0) match
        case Vector(SqlValue.I64(p)) => p
        case other => fail(other.toString)
      case _ => fail("no row")
    }

  def commitUpdate(db: Sql, price: Long): Long ! Async =
    Typed.update[Row](db, "update price set price = ? where id = 'okay'")(Row(price))

  test("write-through: commit, then invalidate — the next read loads the new truth") {
    val (db, cache) = fixture()
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)
    val _ = run(WriteThrough.write(cache, "okay")(commitUpdate(db, 150)))
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 150L)
  }

  test("the ordering is ASSERTED: invalidate runs after the commit, not before") {
    val (db, cache) = fixture()
    val events = Vector.newBuilder[String]
    val probing = new Cache[String, Long]:
      def get(k: String) = cache.get(k)
      def put(k: String, v: Long) = cache.put(k, v)
      def invalidate(k: String) = { events += "invalidate"; cache.invalidate(k) }
      def getOrLoad(k: String)(l: String => Long ! Async) = cache.getOrLoad(k)(l)
      def stats = cache.stats
    val _ = run(WriteThrough.write(probing, "okay")(
      commitUpdate(db, 150).map { n => events += "commit"; n }))
    assertEquals(events.result(), Vector("commit", "invalidate"))
  }

  test("the WRONG ordering resurrects the old truth — the bug the rule prevents, demonstrated") {
    val (db, cache) = fixture()
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)
    // invalidate FIRST, and let a concurrent reader slip in before
    // the commit: it re-loads the PRE-commit value into the cache
    run(cache.invalidate("okay"))
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)   // the racing reader
    val _ = run(commitUpdate(db, 150))
    // the commit landed, but the cache serves the resurrected 100 —
    // indefinitely, until something else invalidates
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)
    assertEquals(run(load(db)("okay")), 150L)                    // the database knows better
  }

  test("the honest window: after commit, before invalidate, the old value is served — stated, not denied") {
    val (db, cache) = fixture()
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)
    val _ = run(commitUpdate(db, 150))
    // the window: committed truth is 150, the cache still says 100
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 100L)
    run(cache.invalidate("okay"))
    assertEquals(run(cache.getOrLoad("okay")(load(db))), 150L)
  }
}
