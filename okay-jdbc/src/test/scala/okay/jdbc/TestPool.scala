package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Resource, Stream}
import okay.given
import okay.sql.{Isolation, Pool, Sql, SqlValue}
import java.sql.DriverManager
import java.util.concurrent.atomic.AtomicInteger

/** specs/sql.md "The pool", on H2: the connection count is bounded,
 * a borrower's leftover transaction never reaches the next one, an
 * exhausted pool says so within its timeout, and close disposes. */
class TestPool extends munit.FunSuite {

  val url = "jdbc:h2:mem:pool;DB_CLOSE_DELAY=-1"

  override def beforeAll(): Unit =
    val c = DriverManager.getConnection(url, "sa", "")
    try c.createStatement().execute("create table marks(n int not null)"): Unit
    finally c.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }

  def count(db: Sql): Long ! Async =
    drain(db.query("select count(*) from marks", Vector.empty)).map(_.head.head match
      case SqlValue.I64(n) => n
      case other => throw AssertionError(s"count: $other"))

  final class Opened:
    val opened = AtomicInteger(0)
    val live = AtomicInteger(0)
    val peak = AtomicInteger(0)
    def open(): JdbcSql ! Async = okay.async {
      opened.incrementAndGet()
      val n = live.incrementAndGet()
      peak.updateAndGet(p => math.max(p, n))
      JdbcSql(DriverManager.getConnection(url, "sa", ""))
    }
    def close(db: JdbcSql): Unit = { live.decrementAndGet(); db.close() }

  test("the pool bounds the connections: eight borrowers, size two, never more than two open, all served") {
    val o = Opened()
    val pool = Pool[JdbcSql](2, 5000L)(() => o.open())(o.close)
    val fibers = (1 to 8).map { i =>
      Async.spawn(pool.borrow { db =>
        !.widen[Long, Async, Resource](db.update("insert into marks values (?)", Vector(SqlValue.I32(i))))
          .flatMap(_ => !.widen[Unit, Async, Resource](Async.sleep(20L)))
      })
    }
    fibers.foreach(_.join())
    assertEquals(o.peak.get, 2, "size is the ceiling")
    assert(o.opened.get <= 2, s"opened ${o.opened.get}: connections are reused, not reopened")
    val c = DriverManager.getConnection(url, "sa", "")
    try assertEquals(run(count(JdbcSql(c))), 8L)
    finally c.close()
    assertEquals(pool.stats.busy, 0)
    assertEquals(pool.stats.idle, o.opened.get)
    pool.close()
    assertEquals(o.live.get, 0, "close disposes the idle connections")
    run(pool.stats.closed match { case true => okay.pure(()); case false => throw AssertionError("closed") })
  }

  test("a borrower's open transaction is rolled back by the brake on return; the next borrower sees autocommit and no leftover") {
    val o = Opened()
    val pool = Pool[JdbcSql](1, 1000L)(() => o.open())(o.close)
    run(pool.borrow { db =>
      !.widen[Long, Async, Resource](db.begin(Isolation.ReadCommitted).flatMap(_ =>
        db.update("insert into marks values (99)", Vector.empty)))
    }): Unit
    val seen = run(pool.borrow(db => !.widen[Long, Async, Resource](
      drain(db.query("select count(*) from marks where n = 99", Vector.empty)).map(_.head.head match
        case SqlValue.I64(n) => n
        case other => throw AssertionError(s"$other")))))
    assertEquals(seen, 0L, "the raw begin's insert never committed")
    assertEquals(o.opened.get, 1)
    pool.close()
  }

  test("exhausted: size one, held; a second borrow fails with Exhausted after its timeout, and succeeds once the first returns") {
    val o = Opened()
    val pool = Pool[JdbcSql](1, 150L)(() => o.open())(o.close)
    val gate = okay.Async.spawn(pool.borrow(_ => !.widen[Unit, Async, Resource](Async.sleep(600L))))
    Thread.sleep(50L)
    val t0 = System.nanoTime()
    val e = intercept[Pool.Exhausted](run(pool.borrow(db => !.widen[Long, Async, Resource](count(db)))))
    val waited = (System.nanoTime() - t0) / 1000000L
    assertEquals(e.size, 1)
    assert(waited >= 100L && waited < 550L, s"waited $waited ms")
    // the loser's cancel runs on the racing fiber: give it a moment
    val deadline = System.nanoTime() + 1000000000L
    while pool.stats.waiting != 0 && System.nanoTime() < deadline do Thread.sleep(5L)
    assertEquals(pool.stats.waiting, 0, s"the timed-out waiter left the queue: ${pool.stats}")
    gate.join()
    // the table is shared with the suite's other tests: the claim is the borrow itself
    assert(run(pool.borrow(db => !.widen[Long, Async, Resource](count(db)))) >= 0L)
    assertEquals(o.opened.get, 1, "the one connection served both")
    pool.close()
  }

  test("pinned: a Resource scope holds one connection across several statements and returns it at the scope's end") {
    val o = Opened()
    val pool = Pool[JdbcSql](1, 1000L)(() => o.open())(o.close)
    val n = run(Resource.run[Long, Async](pool.pinned.flatMap { db =>
      !.widen[Long, Async, Resource](db.update("insert into marks values (7)", Vector.empty))
        .flatMap(_ => !.widen[Long, Async, Resource](count(db)))
    }))
    assert(n >= 1L)
    assertEquals(pool.stats.busy, 0)
    assertEquals(pool.stats.idle, 1)
    val e = intercept[Pool.Closed]({ pool.close(); run(pool.borrow(db => !.widen[Long, Async, Resource](count(db)))) })
    assertEquals(e.getMessage, "pool closed")
  }
}
