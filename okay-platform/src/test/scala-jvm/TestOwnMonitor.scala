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
class TestOwnMonitor extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
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
        // 5 ms a fiber, not 0.5 (own-monitor-burst-load-flake, 2026-09-27):
        // eight 0.5 ms fibers are 4 ms of work, inside the monitor's own
        // 5 ms tick, so one run in ten where the tick came late finished
        // on the forking worker and the MINIMUM failed. Measured under 20
        // CPU burners: 7 of 60 law runs failed at 0.5 ms, 0 of 60 at 5 ms.
        // The law is unchanged: the monitor spreads a burst.
        burst(8) { () => { val _ = ids.add(Thread.currentThread().threadId()); busy(5000000L) } }
        note(s"a run used ${ids.size} thread(s)")
        fewest = math.min(fewest, ids.size)
      assert(fewest >= 2, s"a run used $fewest thread(s) for eight 5 ms fibers on four workers")
    finally sch.close()
  }

  /** adaptive-outside-long-fibers-serial: the Wrocław shape. Eight long
   * fibers forked from OUTSIDE (this thread, not a worker) onto parked
   * workers, then joined: the fewest threads any round used, and the
   * greatest number of them running at once in that round. */
  private def outsideBurst(sch: Schedulers.Running): (Int, Int) =
    given Scheduler = sch
    var fewest = Int.MaxValue
    var leastOverlap = Int.MaxValue
    for _ <- 1 to 5 do
      Thread.sleep(20) // every worker parked, as after a program's own setup
      val ids = ConcurrentHashMap.newKeySet[Long]()
      val running = AtomicInteger()
      val peak = AtomicInteger()
      val fs = (0 until 8).map { _ =>
        Async.spawn(async {
          val _ = ids.add(Thread.currentThread().threadId())
          val _ = peak.accumulateAndGet(running.incrementAndGet(), math.max)
          busy(20000000L)
          val _ = running.decrementAndGet()
        })
      }
      fs.foreach(_.join())
      fewest = math.min(fewest, ids.size)
      leastOverlap = math.min(leastOverlap, peak.get)
    (fewest, leastOverlap)

  test("own: eight long fibers forked from outside run on more than one thread, at once") {
    val sch = Schedulers.own.workers(4).build
    try
      val (threads, overlap) = outsideBurst(sch)
      assert(threads >= 2 && overlap >= 2, s"a round used $threads thread(s), $overlap at once, on four workers")
    finally sch.close()
  }

  test("adaptive: eight long fibers forked from outside run on more than one thread, at once") {
    val sch = Schedulers.adaptive.workers(4).build
    try
      val (threads, overlap) = outsideBurst(sch)
      assert(threads >= 2 && overlap >= 2, s"a round used $threads thread(s), $overlap at once, on four workers")
    finally sch.close()
  }

  /** adaptive-chunked-merge-cost (specs/adaptive-chunked-merge-cost.md):
   * two fibers forked from OUTSIDE, the second after the first's worker
   * is awake and busy, each spinning until the other has started (at
   * most `patience`). Whether they MET says whether the second got a
   * worker of its own while the first still ran. The monitor is off,
   * so nothing but the fork itself can wake a second worker. */
  private def outsidePairMeets(sch: Scheduler, long: Boolean, patience: Long): Boolean =
    val started = AtomicInteger()
    val met = AtomicInteger()
    def body(): Unit =
      val _ = started.incrementAndGet()
      val end = System.nanoTime() + patience
      while started.get < 2 && System.nanoTime() < end do Thread.onSpinWait()
      if started.get >= 2 then { val _ = met.incrementAndGet() }
    def go(): Fiber[Unit] = if long then sch.forkLong(() => async(body())) else sch.fork(() => async(body()))
    Thread.sleep(20) // every worker parked
    val a = go()
    Thread.sleep(10) // a's worker is awake and inside `body`
    val b = go()
    a.join(); b.join()
    note(s"long=$long started=${started.get} met=${met.get}")
    met.get == 2

  test("own: a long fiber forked from outside beside a busy one gets a worker at once (forkLong)") {
    val sch = Schedulers.own.workers(4).unmonitored.build
    try assert(outsidePairMeets(sch, long = true, patience = 2_000_000_000L), "forkLong: the second fiber waited behind the first")
    finally sch.close()
  }

  test("own: the same pair forked with plain fork does not meet (the case forkLong exists for)") {
    val sch = Schedulers.own.workers(4).unmonitored.build
    try assert(!outsidePairMeets(sch, long = false, patience = 300_000_000L), "fork alone woke a second worker: the law above proves nothing")
    finally sch.close()
  }

  test("forkLong is fork on a scheduler that does not override it") {
    var forked = 0
    val plain = new Scheduler:
      def fork[A](prog: () => A ! Async): Fiber[A] =
        forked += 1
        Schedulers.loom.fork(prog)
    assertEquals(plain.forkLong(() => pure(7)).join(), 7)
    assertEquals(forked, 1)
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
