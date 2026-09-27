package okay.foreign

/** the pool's own counters and the events a meter hears
 * (foreign-pool-metrics) — no interpreter, a fake with an `alive` flag */
class TestPoolCounters extends munit.FunSuite:
  final class Fake(val n: Int):
    @volatile var alive = true

  test("opens, a death, its replacement heard as a restart on the thread that caused it; borrowed and held") {
    var opens = 0
    val pool = Pool[Fake]("fake", 2, () => { opens += 1; Fake(opens) }, _.alive, _ => ())
    val heard = java.util.concurrent.ConcurrentLinkedQueue[(String, Boolean, Thread)]()
    Pool.listen { case Pool.Heard.Opened(name, restart) => if name == "fake" then heard.add((name, restart, Thread.currentThread)): Unit }
    val a = pool.lease()
    val b = pool.lease()
    assertEquals((pool.opened, pool.live, pool.borrowed, pool.restarts), (2, 2, 2, 0))
    assert(Pool.held._1 >= 2 && Pool.held._2 >= 2, Pool.held.toString)
    b.release(dead = false)
    assertEquals(pool.borrowed, 1)
    a.release(dead = true)
    assertEquals((pool.live, pool.borrowed), (1, 0))
    val c = pool.lease()   // the idle one, alive: no open
    val d = pool.lease()   // a fresh one, replacing the dead
    assertEquals((pool.opened, pool.restarts), (3, 1))
    // an idle one found dead when borrowed counts as a death too
    c.e.alive = false
    c.release(dead = false)
    val e = pool.lease()
    assertEquals((pool.opened, pool.restarts), (4, 2))
    d.release(dead = false); e.release(dead = false)
    import scala.jdk.CollectionConverters.*
    val events = heard.asScala.toVector
    assertEquals(events.map(_._2), Vector(false, false, true, true))
    assert(events.forall(_._3 eq Thread.currentThread), "an open heard on another thread than the one that caused it")
  }
