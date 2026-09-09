package okay.resilience

import okay.*
import okay.given
import okay.http.{Http, Request, Response}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The composite under a plan (specs/resilience.md, stage 2): the
 * seeded adversary in front of a plain far end, the five pieces in
 * front of the adversary, and the pieces' contracts holding across
 * the whole. Every assertion is about what happened and to which
 * ordinal, never about elapsed time.
 */
class TestFaults extends munit.FunSuite {

  def run[A](prog: A ! Async): A = Async.run(prog).runWith

  /** a far end that answers its own call number */
  final class Far extends Http:
    val calls = AtomicInteger(0)
    def send(r: Request): Response ! Async =
      okay.async(calls.incrementAndGet()).map(n => Response(200, Nil, Http.one(n.toString.getBytes("UTF-8"))))

  def outcome(prog: Response ! Async): Either[Throwable, Int] =
    try Right(run(prog).status) catch case t: Throwable => Left(t)

  test("fate is a pure function of seed and ordinal: two runs, one seed, one log") {
    val plan = Faults.Plan(dropRate = 0.2, failRate = 0.3, slowRate = 0.1, slowMillis = 1)
    val a = (1L to 200L).map(Faults.fate(7, plan))
    val b = (1L to 200L).map(Faults.fate(7, plan))
    assertEquals(a, b)
    assert(a.contains(Faults.Fault.Drop) && a.contains(Faults.Fault.Status(503)) && a.contains(Faults.Fault.Slow(1)))
    assert(a.contains(Faults.Fault.None))
    // a fixed fault wins over a drawn one
    assertEquals(Faults.fate(7, plan.copy(dropAt = Set(1)))(1), Faults.Fault.Drop)
    assertEquals(Faults.fate(7, plan.copy(failAt = Map(1L -> 418)))(1), Faults.Fault.Status(418))
  }

  test("breaker: opens on the drops the plan places, the far end is not asked while it is open") {
    val far = Far()
    val wire = Faults.http(1, Faults.Plan(dropAt = Set(2, 3)))(far)
    val breaker = Breaker("wire", failures = 2, openMillis = 60_000)
    val client = Resilient.http(wire, breaker = Some(breaker))
    val outcomes = (1 to 5).map(_ => outcome(client.send(Request.get("http://x/"))))
    assertEquals(outcomes(0), Right(200))
    assert(outcomes(1).left.exists(_.isInstanceOf[Faults.Dropped]))
    assert(outcomes(2).left.exists(_.isInstanceOf[Faults.Dropped]))
    assert(outcomes(3).left.exists(_.isInstanceOf[Refused.BreakerOpen]))
    assert(outcomes(4).left.exists(_.isInstanceOf[Refused.BreakerOpen]))
    assertEquals(far.calls.get, 1)                 // drops never reached it, open never tried
    assertEquals(wire.log.map(_._2), Vector(Faults.Fault.None, Faults.Fault.Drop, Faults.Fault.Drop))
    assertEquals(breaker.stats.rejected, 2L)
  }

  test("hedge: hides the delay the plan places on the first attempt — the second answers") {
    val far = Far()
    val wire = Faults.http(1, Faults.Plan(slowAt = Map(1L -> 5_000L)))(far)
    val client = Resilient.http(wire, hedge = Some((20L, 2)))
    val body = run(client.send(Request.get("http://x/")).flatMap(Http.text))
    assertEquals(body, "1")                        // the far end's first answer — the hedge's, ordinal 2 on the wire
    assertEquals(wire.log.map(_._2), Vector(Faults.Fault.Slow(5_000), Faults.Fault.None))
    assertEquals(far.calls.get, 1)                 // the slowed attempt was cancelled before it reached the far end
  }

  test("deadline: bounds the whole — a slowed call is cut and refused, nothing waits the plan out") {
    val far = Far()
    val wire = Faults.http(1, Faults.Plan(slowAt = Map(1L -> 5_000L)))(far)
    val client = Resilient.http(wire, budgetMillis = Some(40))
    val e = intercept[Refused.DeadlineExceeded](run(client.send(Request.get("http://x/"))))
    assert(e.remainingMillis <= 0)
    assertEquals(far.calls.get, 0)
  }

  test("the composite under a drawn plan: every call is accounted for, and the run replays by seed") {
    def session(seed: Long): (Vector[Either[String, Int]], Faults.Stats, Breaker.Stats, Limiter.Stats) =
      val far = Far()
      val wire = Faults.http(seed, Faults.Plan(dropRate = 0.15, failRate = 0.25, slowRate = 0.1, slowMillis = 2))(far)
      val breaker = Breaker("c", failures = 3, openMillis = 60_000)
      val bulkhead = Bulkhead("c", permits = 4, queue = 4)
      val limiter = Limiter("c", ratePerSecond = 1_000, burst = 30)
      val client = Resilient.http(wire, budgetMillis = Some(1_000), breaker = Some(breaker),
        bulkhead = Some(bulkhead), limiter = Some((limiter, _ => "k")))
      val outs = (1 to 40).toVector.map { _ =>
        outcome(client.send(Request.get("http://x/"))).left.map(_.getClass.getSimpleName)
      }
      (outs, wire.stats, breaker.stats, limiter.stats)

    val (outs, wire, breaker, limiter) = session(11)
    assertEquals(outs.size, 40)
    // the breaker saw every call the wire saw, plus the ones it refused
    assertEquals(breaker.calls + breaker.rejected, 40L)
    assertEquals(breaker.calls, wire.calls)
    // what the wire dropped or failed is what the breaker counted as failures
    assertEquals(breaker.failures, wire.dropped + wire.failed)
    // the first refusal, if any, follows the third consecutive failure and is final
    val firstOpen = outs.indexWhere(_ == Left("BreakerOpen"))
    if firstOpen >= 0 then
      assert(outs.drop(firstOpen).forall(_ == Left("BreakerOpen")), outs.toString)
      assertEquals(breaker.state, Breaker.State.Open)
    assertEquals(limiter.rejected, 0L)             // the burst covers a serial run
    assertEquals(session(11)._1, outs)             // the same seed, the same story
    assertNotEquals(session(12)._1, outs)          // a different seed, a different one
  }
}
