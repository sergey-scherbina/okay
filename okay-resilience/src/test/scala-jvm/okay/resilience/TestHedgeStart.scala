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

  /**
   * Which attempt the running fiber IS, decided when it was FORKED
   * (hedge-start-hang-under-load). The program used to ask the shared
   * fork counter when it RAN: under load the first attempt's fiber
   * started after the timer had fired and the second fork had counted
   * itself, so the first attempt read 2, took the branch that never
   * answers, and the test waited for ever for an answer nobody was going
   * to give — measured at load 61 in 1 run of 40, and made certain by
   * starting the first attempt 200ms late.
   */
  private val attempt = ThreadLocal[Int]()

  /** counts forks and records WHICH fork was cancelled */
  final class Watched(inner: Scheduler, beforeFork: Int => Unit) extends Scheduler:
    val forks = AtomicInteger(0)
    val cancelled = AtomicReference(Set.empty[Int])
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val n = forks.incrementAndGet()
      beforeFork(n)
      val f = inner.fork(() => { attempt.set(n); prog() })
      new Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit = f.onComplete(k)
        def cancel(): Unit =
          cancelled.updateAndGet(_ + n): Unit
          f.cancel()

  /**
   * A WAIT FOR A CONDITION, WITH A TRIPWIRE — not a deadline anything
   * is asserted about (hedge-bounds, 2026-09-18).
   *
   * The old shape spun on `Thread.yield()` until a FIVE-SECOND wall
   * clock and then failed. Beside ninety other module runs that is
   * not a statement about hedging, it is one about the scheduler, and
   * it failed four landing gates from four lanes that could not have
   * caused it — the last of them a lane whose whole diff was one line
   * of build.sbt.
   *
   * Two things changed and each has a reason:
   *
   *   - THE BUDGET IS A TRIPWIRE. Sixty seconds is not a claim about
   *     how fast anything is; it is the line past which "slow" has
   *     become "hung", and it is what keeps a genuine deadlock from
   *     hanging the gate for ever. The assertion below is still the
   *     CONDITION; the clock only says when to stop waiting for it.
   *   - IT SLEEPS INSTEAD OF SPINNING. `Thread.yield()` on a loaded
   *     box can hand the core straight back to this thread, which is
   *     the one thread that has nothing to do; a millisecond of sleep
   *     gives it to the fibre being waited on. (The starvation theory
   *     was tested and REFUTED with burners at load 22 — this is not
   *     the cause, it is one less thing competing while we wait.)
   */
  private def until(what: String)(p: => Boolean): Unit =
    val deadline = System.nanoTime() + 60_000_000_000L
    while !p && System.nanoTime() < deadline do Thread.sleep(1)
    assert(p, s"waited 60s for $what — at that point it is hung, not slow")

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
      okay.async(attempt.get).flatMap { n =>
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
