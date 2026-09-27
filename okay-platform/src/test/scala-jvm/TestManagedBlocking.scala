package okay

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
}
