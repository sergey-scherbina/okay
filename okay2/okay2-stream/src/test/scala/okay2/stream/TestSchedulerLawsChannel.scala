package okay2.stream

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.Duration
import okay2.async._
import okay2.platform._

/**
 * The two scheduler laws whose blocking device is a CHANNEL — the Scala 3
 * core's TestSchedulerLawsChannel (okay-stream), ported beside okay2's
 * channels over the same `SchedulerFamily` okay2-platform's
 * TestSchedulerLaws runs, so a scheduler added there reaches these too.
 */
class TestSchedulerLawsChannel extends SchedulerFamily {
  override val munitTimeout = Duration(3, "min")

  /**
   * CANCEL WINS ON A BLOCKING SEND TOO (the Scala 3 core's
   * park-interrupt-order), with the race FORCED. A blocking send parks in
   * `CanBlock.blockAccepted`; its registration SPINS until the test has
   * cancelled the fiber AND delivered the acceptance, so the fiber returns
   * into the window every time and must read its interrupt before the
   * filled slot.
   *
   * WHY NOT THE SCALA 3 SHAPE, measured: the Scala 3 law sends by
   * `ch.send` — `Async.await`, parked in `block`, never in
   * `blockAccepted` — and sleeps instead of forcing, and its fiber's
   * answer after `cancel()` is the cancellation whatever the park did. A
   * mutant of okay2's `block` that reads `filled` before the interrupt
   * passed it twice (TestSchedulerLaws caught that one); this shape is
   * the one that reaches the blocking send's own park.
   */
  each("cancel wins on a blocking send too: an acceptance after cancel is not the answer") { sch =>
    implicit val S: Scheduler = sch
    val cb = implicitly[CanBlock]
    val k = new java.util.concurrent.atomic.AtomicReference[Accepted](null)
    val go = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = Async.spawn(Async(cb.blockAccepted { acc =>
      k.set(acc)
      while (!go.get) Thread.onSpinWait()   // a spin, not a park: an interrupt does not end it
      () => ()
    }))
    while (k.get() == null) Thread.onSpinWait()
    f.cancel()
    k.get()(true)                            // accepted AFTER the cancel
    go.set(true)
    val answers = new ConcurrentLinkedQueue[Either[Throwable, Boolean]]()
    f.onComplete(r => { val _ = answers.offer(r) })
    Thread.sleep(50)
    assert(!answers.contains(Right(true)), "an acceptance that arrived after the cancel became the fiber's answer")
    assert(answers.size <= 1, s"answered ${answers.size} times")
  }

  test("adaptive — a fiber that BLOCKS inside a worker does not stop the program") {
    // one worker, and the first fiber blocks on a channel the second
    // fills: without the stuck-check this is a deadlock, which is why
    // `own` is not the default and `adaptive` exists
    val running = Schedulers.adaptive.workers(1).watched(Duration(50, "ms")).build
    try {
      implicit val S: Scheduler = running
      val ch = Channel[Int](4)
      val reader = Async.spawn(Async {
        val filler = Async.spawn(Async { val _ = ch.sendBlocking(9); 0 })
        val got = ch.receiveBlocking()
        val _ = filler.join()
        got
      })
      assertEquals(reader.joinEither().fold(e => throw e, identity), Some(9))
    } finally running.close()
  }
}
