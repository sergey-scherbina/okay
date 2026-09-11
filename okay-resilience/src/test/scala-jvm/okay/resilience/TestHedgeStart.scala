package okay.resilience

import okay.*
import okay.given
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

/**
 * The two windows `Hedge.start` opens by publishing AFTER it acts
 * (hedge-start-races). It forks an attempt and only then adds the
 * fiber to the list `settle` cancels; it arms the hedge timer later
 * still. A settle inside either window leaves something behind — an
 * attempt nobody cancels, which for a hedged request is a duplicate
 * that outlives the answer, or a timer nobody disarms.
 *
 * Driven, not timed: the timer is fired by hand and the scheduler
 * lets the first attempt answer in the middle of the second one's
 * fork, which is the window exactly.
 */
class TestHedgeStart extends munit.FunSuite {

  /** a timer the test fires */
  final class ManualTimer extends Timer:
    private val armed = AtomicReference(Vector.empty[() => Unit])
    def after(millis: Long)(k: () => Unit): () => Unit =
      armed.updateAndGet(_ :+ k)
      () => { armed.updateAndGet(_.filterNot(_ eq k)); () }
    def fireAll(): Int =
      val ks = armed.getAndSet(Vector.empty)
      ks.foreach(_())
      ks.size
    def pending: Int = armed.get.size

  /** counts forks and records WHICH fork was cancelled */
  final class Watched(inner: Scheduler, beforeFork: Int => Unit) extends Scheduler:
    val forks = AtomicInteger(0)
    val cancelled = AtomicReference(Set.empty[Int])
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val n = forks.incrementAndGet()
      beforeFork(n)
      val f = inner.fork(prog)
      new Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit = f.onComplete(k)
        def cancel(): Unit =
          cancelled.updateAndGet(_ + n): Unit
          f.cancel()

  /** a bounded wait for a state change — a deadline, not a duration
   * anything is asserted about */
  private def until(what: String)(p: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + 5000
    while !p && System.currentTimeMillis() < deadline do Thread.`yield`()
    assert(p, s"timed out waiting for $what")

  test("an attempt forked while the answer arrives leaves neither a running attempt nor an armed timer") {
    val timer = ManualTimer()
    val release = AtomicBoolean(false)
    val answered = AtomicBoolean(false)
    lazy val sched: Watched = Watched(summon[Scheduler], n =>
      if n == 2 then
        // the window: let the FIRST attempt answer, and do not return
        // until it has, so the second fiber is registered after settle
        release.set(true)
        until("the first attempt to answer")(answered.get))

    val prog: String ! Async =
      okay.async(sched.forks.get).flatMap { n =>
        if n == 1 then Async.await[String] { k =>
          Thread.ofVirtual().start { () =>
            while !release.get do Thread.`yield`()
            k(Right("first"))
          }: Unit
          () => ()
        }
        else Async.await[String](_ => () => ())   // never answers on its own
      }

    val runner = Thread.ofVirtual().start { () =>
      // max 3: the second attempt arms a hedge timer of its own, so
      // the same window covers the fiber AND the timer
      val got = Async.run(Hedge.run(20, max = 3)(prog)(using sched, timer)).runWith
      assertEquals(got, "first")
      answered.set(true)
    }

    until("the hedge timer to be armed")(timer.pending == 1)
    assertEquals(timer.fireAll(), 1)      // fires start() for the second attempt, here, on this thread
    runner.join()
    assertEquals(sched.forks.get, 2)
    assert(sched.cancelled.get.contains(2),
      s"the second attempt outlived the answer: cancelled ${sched.cancelled.get}")
    assertEquals(timer.pending, 0, "the second attempt left its hedge timer armed after the answer")
  }

  /** the ordinary case, which held before this lane too: an attempt
   * that answers on its own disarms the timer it armed */
  test("a fast first attempt leaves no timer armed") {
    val timer = ManualTimer()
    val got = Async.run(Hedge.run(20, max = 3)(okay.async("fast"))(using summon[Scheduler], timer)).runWith
    assertEquals(got, "fast")
    assertEquals(timer.pending, 0, "the hedge left a timer armed after it settled")
  }
}
