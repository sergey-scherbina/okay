package okay


import okay.freer.*


import okay.std.*
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

/**
 * own-managed-blocking (specs/schedulers.md, "Managed blocking"): a
 * fiber that blocks through the library's door on a worker SAYS SO, and
 * the scheduler answers at once rather than on the monitor's or the
 * stuck-check's next tick. Every scheduler here is `unmonitored`, so the
 * door is the only thing that can help within the deadline; the
 * deadlines are seconds, the help it measures is microseconds.
 */
class TestManagedBlocking extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, "min")

  /** a fiber that blocks in `CanBlock.block` until `release` is called */
  private final class Gate:
    private val k = AtomicReference[(Unit => Unit) | Null](null)
    val entered = CountDownLatch(1)
    def await(): Unit =
      summon[CanBlock].block[Unit] { cb => k.set(cb); entered.countDown(); () => () }
    def release(): Unit =
      while k.get == null do Thread.onSpinWait()
      k.get.nn(())

  test("own: a sibling forked before a fiber blocks runs while it is blocked") {
    val sch = Schedulers.own.workers(2).unmonitored.build
    val gate = Gate()
    val sibling = CountDownLatch(1)
    try
      given Scheduler = sch
      Thread.sleep(20) // both workers parked
      val f = Async.spawn(async {
        val _ = Async.spawn(async(sibling.countDown())) // this worker's own deque, no signal
        gate.await()
      })
      val ran = sibling.await(3, TimeUnit.SECONDS)
      gate.release()
      f.join()
      assert(ran, "the sibling did not run while the fiber that forked it was blocked")
    finally sch.close()
  }

  test("watched: with no parked worker the door starts an overflow worker at once") {
    val sch = Schedulers.own.workers(1).unmonitored
      .watched(scala.concurrent.duration.Duration(10, "s"), overflow = 1).build
    val gate = Gate()
    val sibling = CountDownLatch(1)
    try
      given Scheduler = sch
      val f = Async.spawn(async {
        val _ = Async.spawn(async(sibling.countDown()))
        gate.await()
      })
      val ran = sibling.await(3, TimeUnit.SECONDS)
      gate.release()
      f.join()
      assert(ran, "the sibling waited for the stuck-check (10 s) instead of the door's overflow worker")
    finally sch.close()
  }

  test("own: a blocked worker is not counted awake — an outside fork wakes a parked one") {
    val sch = Schedulers.own.workers(2).unmonitored.build
    val gate = Gate()
    val outside = CountDownLatch(1)
    try
      given Scheduler = sch
      Thread.sleep(20)
      val f = Async.spawn(async(gate.await()))
      assert(gate.entered.await(3, TimeUnit.SECONDS))
      Thread.sleep(20) // the other worker has spun out and parked
      val _ = Async.spawn(async(outside.countDown()))
      val ran = outside.await(3, TimeUnit.SECONDS)
      gate.release()
      f.join()
      assert(ran, "the outside fork woke nobody: the blocked worker counted as awake")
    finally sch.close()
  }

  // ── the bound (scheduler-default-decision) ──────────────────────────
  // `blocked` fibers park in the library's door, then ONE more fiber is
  // forked that would release them all. Every fork is from outside, so
  // they queue in submission order and the releaser is taken last: it
  // gets a thread only if the scheduler has one beyond the blocked ones.

  /** forks `blocked` door-blocked fibers and then their releaser; true when
   * the releaser ran within `within` ms. Always unwedges before returning
   * (from this thread, outside the scheduler) and joins everything. */
  private def releaserRuns(blocked: Int, within: Long)(using Scheduler): Boolean =
    val gates = Vector.fill(blocked)(Gate())
    val fs = gates.map(g => Async.spawn(async(g.await())))
    gates.foreach(g => assert(g.entered.await(10, TimeUnit.SECONDS), "a blocker never started"))
    val once = java.util.concurrent.atomic.AtomicBoolean(false)
    def releaseAll(): Unit = if once.compareAndSet(false, true) then gates.foreach(_.release())
    val ran = CountDownLatch(1)
    val r = Async.spawn(async { ran.countDown(); releaseAll() })
    val inTime = ran.await(within, TimeUnit.MILLISECONDS)
    if !inTime then releaseAll()
    fs.foreach(_.join()); r.join()
    inTime

  test("bound: on loom, n + overflow + 1 fibers blocked in the door all finish") {
    given Scheduler = Schedulers.loom
    assert(releaserRuns(blocked = 2 + 2 + 1, within = 3000))
  }

  // adaptive-blocking-io (2026-09-28): at the bound, work that has not
  // started is handed to a virtual thread instead of waiting, so the
  // releaser runs. What still wedges is a scheduler with nothing to spill
  // to: plain `own` (no overflow) keeps its thread count as its contract.
  test("bound: on adaptive, n + overflow + 1 blocked fibers finish — the releaser spills to a virtual thread") {
    // unmonitored: the stuck-check (20 ms) is the one that finds the queued
    // releaser here; the monitor's own path is the same helper
    val a = Schedulers.own.workers(2).unmonitored.watched(scala.concurrent.duration.Duration(20, "ms"), overflow = 2).build
    try
      given Scheduler = a
      assert(releaserRuns(blocked = 2 + 2 + 1, within = 3000), "the releaser waited behind n + overflow blocked fibers")
    finally a.close()
    val m = Schedulers.own.workers(2).watched(scala.concurrent.duration.Duration(10, "s"), overflow = 2).build
    try
      given Scheduler = m
      assert(releaserRuns(blocked = 2 + 2 + 1, within = 3000), "the monitor did not spill the queued releaser")
    finally m.close()
  }

  test("bound: plain own (nothing to spill to) still wedges at n") {
    val o = Schedulers.own.workers(2).build
    try
      given Scheduler = o
      assert(!releaserRuns(blocked = 2, within = 300), "own grew past its workers")
    finally o.close()
  }

  // the five-way TCP shape (adaptive-blocking-io): fibers blocked in a RAW
  // call, which passes no door, more of them than `n + overflow`. Only the
  // monitor sees them; at the bound the ones still waiting spill to
  // virtual threads, so all of them are in their call at once.
  test("bound: more raw-blocking fibers than n + overflow all block at once on adaptive") {
    val a = Schedulers.own.workers(2).watched(overflow = 2).build
    val inCall = java.util.concurrent.atomic.AtomicInteger()
    val most = java.util.concurrent.atomic.AtomicInteger()
    try
      given Scheduler = a
      Async.spawn(async {
        Vector.fill(8)(Async.spawn(async {
          val now = inCall.incrementAndGet()
          most.accumulateAndGet(now, math.max): Unit
          Thread.sleep(300)
          inCall.decrementAndGet(): Unit
        }))
      }).join().foreach(_.join())
      assertEquals(most.get, 8, "raw-blocking fibers waited for a worker past the bound")
    finally a.close()
  }
}
