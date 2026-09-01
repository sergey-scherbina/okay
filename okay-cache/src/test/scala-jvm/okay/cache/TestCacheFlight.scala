package okay.cache

import okay.{!, Async}
import okay.given
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Single-flight under real concurrency (specs/cache.md): N callers
 * missing one key run the loader ONCE and all get its value; a
 * second key loads independently — no global lock.
 */
class TestCacheFlight extends munit.FunSuite {

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  test("N concurrent misses on one key: one load, N answers; another key independent") {
    val c = Cache.memory[String, String](Regime.Invalidated, 100)
    val loads = AtomicInteger(0)
    val gate = CountDownLatch(1)

    def slow(k: String): String ! Async = okay.async {
      loads.incrementAndGet()
      gate.await(2, TimeUnit.SECONDS)
      s"v-$k"
    }

    val n = 8
    val results = new Array[String](n)
    val done = CountDownLatch(n)
    val threads = (0 until n).map { i =>
      val t = new Thread(() => {
        results(i) = run(c.getOrLoad("hot")(slow))
        done.countDown()
      })
      t.start(); t
    }
    // while the hot key's single load is parked on the gate, an
    // INDEPENDENT key loads freely — no global lock
    val other = new Thread(() => { run(c.getOrLoad("cold")(k => okay.async(s"v-$k"))): Unit })
    other.start(); other.join(2000)
    assertEquals(run(c.get("cold")), Some("v-cold"))

    gate.countDown()
    assert(done.await(5, TimeUnit.SECONDS), "callers never completed")
    threads.foreach(_.join(1000))
    assertEquals(results.toList, List.fill(n)("v-hot"))
    assertEquals(loads.get(), 1, "the hot key dogpiled")
    assertEquals(c.stats.loads, 2L) // hot once + cold once
  }

  test("a failing flight fails every waiter, then the key recovers") {
    val c = Cache.memory[String, String](Regime.Invalidated, 100)
    val gate = CountDownLatch(1)
    def failing(k: String): String ! Async = okay.async {
      gate.await(2, TimeUnit.SECONDS)
      throw RuntimeException("boom")
    }
    val n = 4
    val failures = AtomicInteger(0)
    val done = CountDownLatch(n)
    (0 until n).foreach { _ =>
      new Thread(() => {
        try { run(c.getOrLoad("k")(failing)): Unit }
        catch { case _: RuntimeException => failures.incrementAndGet(): Unit }
        finally done.countDown()
      }).start()
    }
    Thread.sleep(100) // let them pile onto one flight
    gate.countDown()
    assert(done.await(5, TimeUnit.SECONDS), "waiters hung on the failed flight")
    assertEquals(failures.get(), n)
    assertEquals(run(c.getOrLoad("k")(k => okay.async("fine"))), "fine")
  }
}
