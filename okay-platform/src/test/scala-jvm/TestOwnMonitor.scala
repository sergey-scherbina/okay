package okay

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/**
 * own-scheduler-monitor (specs/schedulers.md, "The monitor"): local
 * work nobody was told about. A fiber forked FROM a worker lands on its
 * deque with no signal; before the monitor, a few long fibers forked
 * that way ran on ONE thread, and blocking ones reached a handful of
 * threads on `adaptive`. The assertions are on threads and concurrency
 * with wide margins, never on time.
 */
class TestOwnMonitor extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private def busy(nanos: Long): Unit =
    val end = System.nanoTime() + nanos
    while System.nanoTime() < end do Thread.onSpinWait()

  /** fork `n` fibers running `body` from INSIDE a fiber, join them all */
  private def burst(n: Int)(body: () => Unit)(using Scheduler): Unit =
    Async.spawn {
      async((0 until n).map(_ => Async.spawn(async(body())))).flatMap { fs =>
        fs.foldLeft(pure[Async, Unit](()))((acc, f) => acc.flatMap(_ => f.joinAsync))
      }
    }.join()

  test("own: a burst of long fibers forked inside a fiber runs on more than one thread") {
    val sch = Schedulers.own.workers(4).build
    try
      given Scheduler = sch
      var fewest = Int.MaxValue
      for _ <- 1 to 10 do
        Thread.sleep(20) // every worker parked: only the monitor can spread the burst
        val ids = ConcurrentHashMap.newKeySet[Long]()
        burst(8) { () => { val _ = ids.add(Thread.currentThread().threadId()); busy(500000L) } }
        fewest = math.min(fewest, ids.size)
      assert(fewest >= 2, s"a run used $fewest thread(s) for eight 0.5 ms fibers on four workers")
    finally sch.close()
  }

  /** peak number of `calls` blocking at once when `n` fibers forked inside
   * a fiber each block `calls` times for 1 ms */
  private def blockingPeak(n: Int, calls: Int)(using Scheduler): Int =
    val active = AtomicInteger()
    val peak = AtomicInteger()
    burst(n) { () =>
      var c = 0
      while c < calls do
        val now = active.incrementAndGet()
        val _ = peak.accumulateAndGet(now, math.max)
        Thread.sleep(1)
        val _ = active.decrementAndGet()
        c += 1
    }
    peak.get

  test("own: fibers that block inside a worker wake the parked workers") {
    val sch = Schedulers.own.workers(4).build
    try
      given Scheduler = sch
      Thread.sleep(20)
      val peak = blockingPeak(32, 4)
      assert(peak >= 3, s"peak $peak concurrent blocking calls on four workers")
    finally sch.close()
  }

  test("adaptive: fibers that block inside a worker reach the overflow workers too") {
    val sch = Schedulers.adaptive.workers(4).build // overflow defaults to 4: eight threads at most
    try
      given Scheduler = sch
      Thread.sleep(20)
      val peak = blockingPeak(32, 4)
      assert(peak >= 6, s"peak $peak concurrent blocking calls on four workers plus four overflow")
    finally sch.close()
  }
}
