package okay2.jdbc

import java.sql.DriverManager
import java.util.concurrent.atomic.AtomicInteger
import okay2.{!, +, Resource}
import okay2.async.Async
import okay2.platform._
import okay2.sql.{Isolation, Pool, Sql, SqlValue}
import okay2.stream.Source

/** the pool on H2 (okay-jdbc's TestPool): the connection count is
 * bounded, a borrower's leftover transaction never reaches the next
 * one, an exhausted pool says so within its timeout, close disposes */
class TestPool extends munit.FunSuite {

  val url = "jdbc:h2:mem:pool;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit = {
    val c = DriverManager.getConnection(url, "sa", "")
    try c.createStatement().execute("create table marks(n int not null)"): Unit
    finally c.close()
  }

  def count(db: Sql): Long ! Async =
    Source.concat(db.query("select count(*) from marks", Vector.empty)).map(_.head.head match {
      case SqlValue.I64(n) => n
      case other => throw new AssertionError(s"count: $other")
    })

  final class Opened {
    val opened = new AtomicInteger(0)
    val live = new AtomicInteger(0)
    val peak = new AtomicInteger(0)
    def open(): JdbcSql ! Async = Async {
      opened.incrementAndGet()
      val n = live.incrementAndGet()
      peak.updateAndGet(p => math.max(p, n))
      new JdbcSql(DriverManager.getConnection(url, "sa", ""))
    }
    def close(db: JdbcSql): Unit = { live.decrementAndGet(); db.close() }
  }

  test("the pool bounds the connections: eight borrowers, size two, never more than two open, all served") {
    val o = new Opened
    val pool = Pool[JdbcSql](2, 5000L)(() => o.open())(o.close)
    val fibers = (1 to 8).map { i =>
      Async.spawn(pool.borrow[Unit] { db =>
        db.update("insert into marks values (?)", Vector(SqlValue.I32(i))).flatMap(_ => Async.sleep(20L))
      })
    }
    fibers.foreach(_.join())
    assertEquals(o.peak.get, 2, "size is the ceiling")
    assert(o.opened.get <= 2, s"opened ${o.opened.get}: connections are reused, not reopened")
    val c = DriverManager.getConnection(url, "sa", "")
    try assertEquals(Run(count(new JdbcSql(c))), 8L)
    finally c.close()
    assertEquals(pool.stats.busy, 0)
    assertEquals(pool.stats.idle, o.opened.get)
    pool.close()
    assertEquals(o.live.get, 0, "close disposes the idle connections")
    assert(pool.stats.closed)
  }

  test("a borrower's open transaction is rolled back by the brake on return; the next borrower sees no leftover") {
    val o = new Opened
    val pool = Pool[JdbcSql](1, 1000L)(() => o.open())(o.close)
    Run(pool.borrow[Long](db => db.begin(Isolation.ReadCommitted).flatMap(_ => db.update("insert into marks values (99)", Vector.empty)))): Unit
    val seen = Run(pool.borrow[Long](db =>
      Source.concat(db.query("select count(*) from marks where n = 99", Vector.empty)).map(_.head.head match {
        case SqlValue.I64(n) => n
        case other => throw new AssertionError(s"$other")
      })))
    assertEquals(seen, 0L, "the raw begin's insert never committed")
    assertEquals(o.opened.get, 1)
    pool.close()
  }

  test("exhausted: size one, held; a second borrow fails with Exhausted after its timeout, and succeeds once the first returns") {
    val o = new Opened
    val pool = Pool[JdbcSql](1, 150L)(() => o.open())(o.close)
    val gate = Async.spawn(pool.borrow[Unit](_ => Async.sleep(600L)))
    Thread.sleep(50L)
    val t0 = System.nanoTime()
    val e = intercept[Pool.Exhausted](Run(pool.borrow[Long](db => count(db))))
    val waited = (System.nanoTime() - t0) / 1000000L
    assertEquals(e.size, 1)
    assert(waited >= 100L && waited < 550L, s"waited $waited ms")
    val deadline = System.nanoTime() + 1000000000L
    while (pool.stats.waiting != 0 && System.nanoTime() < deadline) Thread.sleep(5L)
    assertEquals(pool.stats.waiting, 0, s"the timed-out waiter left the queue: ${pool.stats}")
    gate.join()
    assert(Run(pool.borrow[Long](db => count(db))) >= 0L)
    assertEquals(o.opened.get, 1, "the one connection served both")
    pool.close()
  }

  test("pinned: a Resource scope holds one connection across several statements and returns it at the scope's end") {
    val o = new Opened
    val pool = Pool[JdbcSql](1, 1000L)(() => o.open())(o.close)
    val n = Run(Resource.run[Long, Async](pool.pinned.flatMap[Resource + Async, Long] { db =>
      db.update("insert into marks values (7)", Vector.empty).flatMap(_ => count(db))
    }))
    assert(n >= 1L)
    assertEquals(pool.stats.busy, 0)
    assertEquals(pool.stats.idle, 1)
    val e = intercept[Pool.Closed] { pool.close(); Run(pool.borrow[Long](db => count(db))) }
    assertEquals(e.getMessage, "pool closed")
  }
}
