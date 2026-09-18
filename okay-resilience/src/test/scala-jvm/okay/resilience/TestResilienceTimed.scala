package okay.resilience

import okay.*
import okay.given
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

/**
 * The pieces that need a real timer or a real fiber: hedging, a
 * deadline that runs out mid-way, a limiter that parks. Delays are
 * tens of milliseconds and every assertion is about ORDER (who
 * answered, how many started), never about elapsed time.
 */
class TestResilienceTimed extends munit.FunSuite {
  // Live, out of `sbt test`: these tests race a real timer (a 5 ms
  // answer against a 20 ms hedge). On 2026-09-16 the hedge test lost
  // that race in a gate on a box at load 22 and passed 8/8 alone a
  // minute later; the policy for flakiness found in an untagged suite
  // is the tag, not a wider window (integration-test-gate).
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))


  def run[A](prog: A ! Async): A = Async.run(prog).runWith

  /**
   * A timer the TEST fires (hedge-timed-flake). The three assertions
   * below that say "nothing else started" used to say it by sleeping
   * past the hedge delay on the real timer — which asserts the BOX's
   * speed, not the hedge's contract: under load the first attempt
   * takes longer than the delay, the timer fires exactly as designed,
   * and the count is 2 (measured 2026-09-09, nine sbt JVMs).
   *
   * With this one, no wall clock is involved: after the run has
   * settled, `fireAll` runs whatever is still armed, and the contract
   * is that nothing starts, because `start()` is guarded by `done`.
   */
  final class ManualTimer extends Timer:
    private val armed = AtomicReference(Vector.empty[() => Unit])
    def after(millis: Long)(k: () => Unit): () => Unit =
      armed.updateAndGet(_ :+ k)
      () => { armed.updateAndGet(_.filterNot(_ eq k)); () }
    /** run every callback still armed; answers how many there were */
    def fireAll(): Int =
      val ks = armed.getAndSet(Vector.empty)
      ks.foreach(_())
      ks.size

  /** a program that answers after `ms` on the platform timer */
  def after[A](ms: Long)(a: => A): A ! Async =
    Async.sleep(ms).map(_ => a)

  /** never answers; counts its cancellations */
  def never[A](cancelled: AtomicInteger): A ! Async =
    Async.await[A](_ => () => { cancelled.incrementAndGet(); () })

  test("hedge: a slow first attempt is joined by a second; the first success answers and the loser is cancelled") {
    val starts = AtomicInteger(0)
    val cancelled = AtomicInteger(0)
    val got = run(Hedge.run(20)(okay.async(starts.incrementAndGet()).flatMap { n =>
      if n == 1 then never[String](cancelled) else after(5)(s"attempt $n")
    }))
    // WHAT HEDGING PROMISES, AND NOT WHAT THE SCHEDULER DID
    // (hedge-bounds, 2026-09-18). This test uses real time — a 20 ms
    // hedge against a 5 ms answer — so on a loaded box a THIRD
    // attempt can start and win, and `starts == 2` then fails while
    // hedging worked perfectly. The three claims below hold on any
    // machine:
    //   a hedge HAPPENED (the first attempt never answers, so the
    //   answer can only come from a later one)
    assert(got.startsWith("attempt "), got)
    assert(starts.get >= 2, s"no hedge started: starts=${starts.get}")
    //   EVERY LOSER IS CANCELLED — the promise itself, and the one
    //   assertion here that is about hedging rather than about time
    assertEquals(cancelled.get, starts.get - 1,
      s"starts=${starts.get} cancelled=${cancelled.get}: every attempt but the winner must be cancelled")
  }

  test("hedge: a fast first attempt never starts a second, however late the timer fires") {
    val timer = ManualTimer()
    val starts = AtomicInteger(0)
    val got = run(Hedge.run(20)(okay.async { starts.incrementAndGet(); "first" })(using summon[Scheduler], timer))
    assertEquals(got, "first")
    assertEquals(starts.get, 1)
    timer.fireAll(): Unit    // whatever survived the settle fires now
    assertEquals(starts.get, 1)
  }

  test("hedge: a failure is not slowness — the only attempt failing is the answer, nothing else starts") {
    val starts = AtomicInteger(0)
    val timer = ManualTimer()
    val e = intercept[RuntimeException](run(Hedge.run(20)(okay.async[String] {
      starts.incrementAndGet(); throw RuntimeException("no")
    })(using summon[Scheduler], timer)))
    assertEquals(e.getMessage, "no")
    assertEquals(starts.get, 1)
    timer.fireAll(): Unit
    assertEquals(starts.get, 1)
  }

  test("hedge: the second failing while the first is still running leaves the first to answer") {
    val starts = AtomicInteger(0)
    val got = run(Hedge.run(10)(okay.async(starts.incrementAndGet()).flatMap { n =>
      if n == 1 then after(60)("slow but right") else okay.async[String](throw RuntimeException("hedge died"))
    }))
    assertEquals(got, "slow but right")
    assertEquals(starts.get, 2)
  }

  test("hedge: max bounds the attempts in flight") {
    val starts = AtomicInteger(0)
    val cancelled = AtomicInteger(0)
    val got = run(Hedge.run(10, max = 3)(okay.async(starts.incrementAndGet()).flatMap { n =>
      if n < 3 then never[Int](cancelled) else after(5)(n)
    }))
    assertEquals(got, 3)
    assertEquals(starts.get, 3)
    assertEquals(cancelled.get, 2)
  }

  test("deadline: a budget that runs out mid-way cancels the run and answers Exceeded") {
    val cancelled = AtomicInteger(0)
    val e = intercept[Refused.DeadlineExceeded](run(Deadline.enforce(Deadline.in(30))(never[Int](cancelled))))
    assert(e.remainingMillis <= 0)
    assertEquals(cancelled.get, 1)
  }

  test("deadline: a program within its budget answers as itself") {
    assertEquals(run(Deadline.enforce(Deadline.in(500))(after(5)("in time"))), "in time")
  }

  test("limiter: with a wait budget the caller parks for the refill instead of being refused") {
    // the limiter's clock is FROZEN, so the bucket cannot refill behind
    // the test's back however slow the box is: the second call always
    // owes a 20 ms park, and `delayed` says it happened. No wall-clock
    // assertion — resilience-timed-flake: one read 0 ms under five
    // sibling sbts, because the real clock had refilled the bucket
    // between the two calls.
    val l = Limiter("slow", ratePerSecond = 50, burst = 1, maxWaitMillis = 100, clock = () => 0L)
    assertEquals(run(l.admit()(okay.async(1))), 1)
    assertEquals(l.stats.delayed, 0L)
    assertEquals(run(l.admit()(okay.async(2))), 2)   // parks ~20 ms on the platform timer
    assertEquals(l.stats.delayed, 1L)
    assertEquals(l.stats.rejected, 0L)
  }
}
