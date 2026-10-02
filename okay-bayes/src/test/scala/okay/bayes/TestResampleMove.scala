package okay.bayes

import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 7b: resample-move SMC — a static parameter's particles stop being only prior draws */
class TestResampleMove extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // enough flips that the weights keep collapsing: resampling then keeps only a handful of prior draws
  val flips: Vector[Boolean] = Vector.tabulate(400)(i => i % 3 != 0)
  val (a, b) = (2.0, 2.0)
  val k = flips.count(identity)
  val model = for
    p <- sample("p", Beta(a, b))
    _ <- observeEach(flips)(_ => Bernoulli(p), identity)
  yield p
  val (mean, sd) = { val (x, y) = (a + k, b + flips.length - k); (x / (x + y), math.sqrt(x * y / ((x + y) * (x + y) * (x + y + 1)))) }
  val logZ = logBeta(a + k, b + flips.length - k) - logBeta(a, b)

  test("Beta–Bernoulli one flip at a time: distinct particle values without and with the move, and the move's posterior and evidence exact") {
    val plain = smc(model, particles = 1000)
    val moving = smc(model, particles = 1000, move = Some(Kernel.site("p").times(3)))
    val distinct = (ps: Particles[Double]) => ps.values.distinct.length
    def moments(ps: Particles[Double]) =
      val m = ps.expect(identity)
      (m, math.sqrt(ps.expect(x => (x - m) * (x - m))))
    val ((m0, s0), (m1, s1)) = (moments(plain), moments(moving))
    report(f"resample-move, 400 flips, 1000 particles: distinct values of p ${distinct(plain)} without the move, ${distinct(moving)} with it; E[p] $m0%.4f / $m1%.4f, sd $s0%.4f / $s1%.4f (exact $mean%.4f ± $sd%.4f); log p(y) ${plain.logEvidence}%.4f / ${moving.logEvidence}%.4f (exact $logZ%.4f); ${moving.resamplings} resamplings")
    // measured: 259 of 1000 distinct without the move, 968 with it — the move restores nearly every particle
    assert(distinct(moving) > 900 && distinct(moving) > 3 * distinct(plain), "the move refreshes what resampling collapsed")
    assert(math.abs(m1 - mean) < 0.01)
    assert(math.abs(s1 / sd - 1) < 0.1)
    assert(math.abs(moving.logEvidence - logZ) < 0.1)
  }

  test("a NUTS block as the move: the same posterior") {
    val moving = smc(model, particles = 500, move = Some(Kernel.nuts("p")))
    val m = moving.expect(identity)
    report(f"resample-move by nuts(p): E[p] $m%.4f (exact $mean%.4f), ${moving.values.distinct.length} distinct of 500")
    assert(math.abs(m - mean) < 0.01)
  }
