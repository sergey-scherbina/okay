package okay2.jdbc

import java.sql.DriverManager
import java.util.concurrent.atomic.AtomicInteger
import okay2.persist.{Policy, Store, StoreSuite}

/**
 * The stage-3 acceptance (okay-jdbc's TestSqlStore, specs/persist.md):
 * the SAME contract suite every persist engine passes, over a SQL table
 * via the seam — a consumer developed against memory meets no surprises
 * in a database.
 */
class TestSqlStore extends StoreSuite {
  private val n = new AtomicInteger(0)
  private var conns = List.empty[java.sql.Connection]

  def mkStore(): Store = {
    val conn = DriverManager.getConnection(s"jdbc:h2:mem:sqlstore${n.incrementAndGet()};DB_CLOSE_DELAY=-1", "sa", "")
    conns ::= conn
    SqlStore(new JdbcSql(conn))
  }

  override def afterAll(): Unit = conns.foreach(_.close())

  // per-record granularity, the memory engine's numbers
  def tinyRetention: Policy = Policy(retainBytes = 340)
}
