package okay.resilience

/**
 * Does an adaptive concurrency limit earn its place? Measured, and
 * the answer is NO — for a reason that belongs to the idea rather
 * than to this implementation (adaptive-concurrency,
 * specs/resilience.md's last open box).
 *
 * The box was deferred twice, the second time because no measurement
 * existed. This is the measurement, and these tests are kept so the
 * refutation stays honest: they assert the FINDING, so a controller
 * that actually beats a constant will FAIL here and force whoever
 * wrote it to re-read the numbers instead of quietly landing.
 *
 * Deterministic by construction — a virtual clock and a modelled
 * downstream — so none of it moves with the machine.
 */
class TestAdaptive extends munit.FunSuite:

  /** the downstream: 20 concurrent calls at 50 ms, every extra one in
    * flight costs everybody 10 ms more */
  val base = 50L
  val queue = 10L

  def steady(capacity: Int, arrivals: Int = 2000)(limit: Limit): Workload.Result =
    Workload.run(limit, arrivals, everyMillis = 2, base, queue, _ => capacity)

  /** the case a fixed number cannot follow: half the downstream goes
    * away in the middle of the run (a deploy, an instance lost, a
    * neighbour taking the shared database) */
  def halving(from: Int, to: Int, at: Long)(limit: Limit): Workload.Result =
    Workload.run(limit, arrivals = 2000, everyMillis = 2, base, queue,
      t => if t < at then from else to)

  test("the model behaves like a queue: more permits buys throughput, then only latency") {
    val byPermits = Vector(5, 10, 20, 40, 80).map(n => n -> steady(20)(Fixed(n))).toMap
    byPermits.toVector.sortBy(_._1).foreach((n, r) => println(f"  fixed $n%3d  ${r.show}"))
    // under the knee, permits buy throughput
    assert(byPermits(10).throughputPerSecond > byPermits(5).throughputPerSecond,
      "10 permits should serve more than 5 against a capacity of 20")
    // over it, they buy latency and nothing else — which is the whole
    // reason a concurrency limit exists
    assert(byPermits(80).p99 > byPermits(20).p99 * 2,
      s"80 permits should cost tail latency: p99 ${byPermits(80).p99} vs ${byPermits(20).p99}")
    assert(byPermits(80).throughputPerSecond <= byPermits(20).throughputPerSecond * 1.15,
      "past capacity, throughput must not keep rising with permits")
  }

  test("REFUTED: on steady capacity the gradient loses to a well-chosen constant") {
    val best = Vector(10, 20, 30).map(n => steady(20)(Fixed(n))).maxBy(_.throughputPerSecond)
    val adaptive = steady(20)(Gradient(start = 10))
    println(f"  best fixed ${best.show}")
    println(f"  gradient   ${adaptive.show}")
    val ratio = adaptive.throughputPerSecond / best.throughputPerSecond
    println(f"  ratio $ratio%4.2f")
    // the bar this lane set BEFORE the numbers was 0.8 of the best
    // fixed. It reads 0.55. The assertion is written the way it came
    // out, so a controller that fixes this FAILS here and is read.
    assert(ratio < 0.8,
      f"the gradient now reaches $ratio%4.2f of the best fixed — the refutation in " +
      "specs/resilience.md may no longer hold; re-read it before moving this bar")
  }

  test("REFUTED, and not for want of time: twenty times the run changes nothing") {
    val ratios = Vector(2000, 10000, 40000).map { arrivals =>
      val g = steady(20, arrivals)(Gradient(start = 10))
      val f = steady(20, arrivals)(Fixed(20))
      val ratio = g.throughputPerSecond / f.throughputPerSecond
      println(f"  arrivals $arrivals%6d  gradient ${g.throughputPerSecond}%6.1f/s  fixed20 ${f.throughputPerSecond}%6.1f/s  ratio $ratio%4.2f  limit ${g.finalPermits}%3d")
      ratio
    }
    // this is what makes it a refutation rather than a slow start: the
    // controller does not converge, it ORBITS — the only way it learns
    // the limit is too high is by exceeding it, and exceeding it costs
    // the latency that makes it cut again
    assert(ratios.max - ratios.min < 0.1,
      s"the ratio moved with run length ($ratios) — then it is convergence speed, not oscillation, and the refutation needs re-reading")
    assert(ratios.forall(_ < 0.8), s"all three well under the bar: $ratios")
  }

  test("the one thing it DOES buy: when capacity halves it follows, and a stale constant cannot") {
    val tunedForBefore = halving(40, 10, 2000)(Fixed(40))
    val tunedForAfter = halving(40, 10, 2000)(Fixed(10))
    val adaptive = halving(40, 10, 2000)(Gradient(start = 20))
    println(f"  fixed 40 (tuned before) ${tunedForBefore.show}")
    println(f"  fixed 10 (tuned after)  ${tunedForAfter.show}")
    println(f"  gradient                ${adaptive.show}")
    // the honest half of the result, kept so the refutation is not
    // read as "adaptation is worthless"
    assert(adaptive.p99 < tunedForBefore.p99,
      s"gradient p99 ${adaptive.p99} vs the stale constant's ${tunedForBefore.p99}")
    assert(adaptive.throughputPerSecond > tunedForAfter.throughputPerSecond,
      "and it serves more than the conservative constant")
  }
