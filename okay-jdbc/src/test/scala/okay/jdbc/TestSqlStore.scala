package okay.jdbc

import okay.persist.{Policy, Store, StoreSuite}
import java.sql.DriverManager
import java.util.concurrent.atomic.AtomicInteger

/**
 * The stage-3 acceptance that matters (specs/persist.md): the SAME
 * contract suite every persist engine passes, over a SQL table via
 * the seam — a consumer developed against memory or files meets no
 * surprises in a database.
 */
class TestSqlStore extends StoreSuite:
  private val n = AtomicInteger(0)
  private var conns = List.empty[java.sql.Connection]

  def mkStore(): Store =
    val conn = DriverManager.getConnection(
      s"jdbc:h2:mem:sqlstore${n.incrementAndGet()};DB_CLOSE_DELAY=-1", "sa", "")
    conns ::= conn
    SqlStore(JdbcSql(conn))

  override def afterAll(): Unit = conns.foreach(_.close())

  // per-record granularity, the memory engine's numbers
  def tinyRetention: Policy = Policy(retainBytes = 340)
