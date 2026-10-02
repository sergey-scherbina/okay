package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed

/** specs/okay-bayes.md stage 2d: Thompson sampling (Bayesian Methods for Hackers ch.6) against exact probabilities and the Lai–Robbins bound */
class TestBandit extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("Thompson pulls each arm with the probability that it is the best") {
    val b = Bandit(Vector(3, 5, 2), Vector(10, 12, 9))
    // P(arm i best) = ∫ f_i(x) Π_{j ≠ i} F_j(x) dx, by the trapezoid on a fine grid
    val n = 20000
    val xs = (0 to n).map(_.toDouble / n)
    val dens = Vector.tabulate(b.arms)(i => xs.map(x => math.exp(b.posterior(i).logPdf(x))))
    val cdfs = dens.map(_.scanLeft(0.0)(_ + _ / n).tail)
    val best = Vector.tabulate(b.arms)(i => xs.indices.map(k => dens(i)(k) * (0 until b.arms).filter(_ != i).map(cdfs(_)(k)).product / n).sum)
    val rng = Random(6)
    val draws = 200000
    val counts = Vector.fill(draws)(b.choose(rng)).groupMapReduce(identity)(_ => 1)(_ + _)
    for i <- 0 until b.arms do
      val got = counts.getOrElse(i, 0).toDouble / draws
      report(f"P(arm $i is best): Thompson's choice $got%.4f, exact ${best(i)}%.4f")
      assert(math.abs(got - best(i)) < 4 * math.sqrt(best(i) * (1 - best(i)) / draws) + 1e-3, s"arm $i: $got vs ${best(i)}")
  }

  // the book's arms
  val p = Vector(0.85, 0.60, 0.75)
  def kl(a: Double, b: Double): Double = a * math.log(a / b) + (1 - a) * math.log((1 - a) / (1 - b))
  /** Lai & Robbins (1985): no consistent strategy's regret grows slower than Σ Δᵢ / KL(pᵢ, p*) · log T */
  def laiRobbins(t: Int): Double = p.filter(_ < p.max).map(q => (p.max - q) / kl(q, p.max)).sum * math.log(t)

  /** the expected regret of a run: Σ over pulls of the gap to the best arm */
  def regret(chosen: Vector[Int], upTo: Int): Double = chosen.iterator.take(upTo).map(a => p.max - p(a)).sum

  test("regret grows like log T, near the Lai–Robbins bound; uniform random play grows linearly") {
    val runs = 30
    val world = Random(1933)
    val plays = Vector.tabulate(runs)(r => Bandit.play(Bandit(3), 10000, Random(r))(a => world.nextDouble() < p(a)))
    val r1k = plays.map((_, c) => regret(c, 1000)).sum / runs
    val r10k = plays.map((_, c) => regret(c, 10000)).sum / runs
    val random = 10000 * (p.max - p.sum / p.length)
    val share = plays.map((_, c) => c.count(_ == 0).toDouble / c.length).sum / runs
    report(f"Thompson on (0.85, 0.60, 0.75), $runs runs: regret $r1k%.1f at T = 1000, $r10k%.1f at T = 10 000 (Lai–Robbins ${laiRobbins(1000)}%.1f, ${laiRobbins(10000)}%.1f); random play ${random}%.0f; best arm's share $share%.3f")
    assert(r10k < 3 * laiRobbins(10000), s"regret $r10k against the bound ${laiRobbins(10000)}")
    assert(r10k / r1k < 3, s"ten times the pulls should not cost ten times the regret: $r1k -> $r10k")
    assert(r10k < random / 10, "far below uniform play")
    assert(share > 0.95, s"the best arm's share of pulls $share")
  }

  test("a bandit is a value: observe answers a new one, the old is unchanged, and the posterior is the conjugate update") {
    val b0 = Bandit(2)
    val b1 = b0.observe(1, true).observe(1, false).observe(1, true)
    assertEquals(b0, Bandit(Vector(0, 0), Vector(0, 0)))
    assertEquals(b1.posterior(1), Distribution.Beta(3, 2))
    assertEqualsDouble(b1.means(1), 0.6, 1e-12)
    assertEqualsDouble(b1.means(0), 0.5, 1e-12)
  }
