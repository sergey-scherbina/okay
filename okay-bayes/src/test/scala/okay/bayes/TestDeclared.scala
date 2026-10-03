package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Distribution.*
import Declared.{sample, both, sampleN}

/** specs/okay-bayes.md stage 8: parameters as the free selective — the structure known before anything runs */
class TestDeclared extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  /** a distribution that refuses to be drawn from: listing sites must not draw */
  object Untouchable extends Distribution[Double]:
    def logPdf(a: Double): Double = 0.0
    def sample(rng: Random): Double = throw IllegalStateException("drawn")
    def propose(a: Double, scale: Double, rng: Random): Double = a
    def coerce(v: Any): Option[Double] = None
    def numeric(a: Double): Double = a

  // the coin of TestBayes, as a declaration: which site exists is decided by a draw, and both are written down
  val coin = sample("coin", Bernoulli(0.5))
    .ifS(sample("x", Normal(0, 1)).map(x => (true, x)))(sample("y", Uniform(-1, 1)).map(y => (false, y)))

  test("the sites are listed without a single draw, the branches marked") {
    val p = both(sample("a", Untouchable), both(sample("k", Poisson(3)), sampleN("z", Normal(0, 1), 3)))
    val ss = Declared.sites(p)
    assertEquals(ss.map(s => (s.name, s.conditional)), Vector(("a", false), ("k", false), ("z[0]", false), ("z[1]", false), ("z[2]", false)))
    assertEquals(ss.find(_.name == "k").map(_.dist), Some(Poisson(3)))
    assertEquals(Declared.sites(coin).map(s => (s.name, s.conditional)), Vector(("coin", false), ("x", true), ("y", true)))
    report(s"declared sites, nothing run: ${Declared.sites(coin).map(s => s.name + (if s.conditional then " (under a branch)" else "")).mkString(", ")}")
  }

  test("the coin declared with ifS: P(coin) by metropolis is the closed form, and Declared.nuts refuses it before running") {
    val heads = math.exp(-0.09 / 2.5) / math.sqrt(2 * math.Pi * 1.25)
    val tails = (0.9192433407662289 - 0.004661188023718732) / 2
    val exact = heads / (heads + tails)
    val post = Bayes.metropolis(Declared.model(coin)((_, v) => Normal(v, 0.5).logPdf(0.3)), samples = 60000, burn = 5000)
    val p = post.draws.count(_._1).toDouble / post.draws.length
    report(f"declared coin: P(coin) $p%.4f, exact $exact%.4f")
    assert(math.abs(p - exact) < 0.03)
    val e = intercept[IllegalArgumentException](Declared.nuts(coin, samples = 10, burn = 0)(_ => 0.0))
    assert(e.getMessage.contains("'x' is under a branch"), e.getMessage)
  }

  test("eight schools NON-CENTERED by Declared.nuts: θ = μ + τ·z, against the exact posterior") {
    val y = Vector(28.0, 8, -3, 7, -1, 1, 18, 12)
    val sigma = Vector(15.0, 10, 16, 11, 9, 11, 10, 18)
    val tau = 5.0
    val params = both(sample("mu", Normal(0, 5)), sampleN("z", Normal(0, 1), 8))
    val post = Declared.nuts(params, samples = 1500, burn = 500, chains = 2) { (mu, z) =>
      z.indices.map(j => Normal(mu + tau * z(j), sigma(j)).logPdf(y(j))).sum
    }
    // the exact posterior, as TestVector derives it
    val prec = 1 / 25.0 + sigma.map(s => 1 / (tau * tau + s * s)).sum
    val (muMean, muVar) = (y.indices.map(j => y(j) / (tau * tau + sigma(j) * sigma(j))).sum / prec, 1 / prec)
    def theta(j: Int): (Double, Double) =
      val pj = 1 / (sigma(j) * sigma(j)) + 1 / (tau * tau)
      val w = 1 / (tau * tau * pj)
      ((y(j) / (sigma(j) * sigma(j)) + muMean / (tau * tau)) / pj, math.sqrt(1 / pj + w * w * muVar))
    val mus = post.draws.map(_._1)
    val thetas = Vector.tabulate(8)(j => post.draws.map((mu, z) => mu + tau * z(j)))
    report(f"eight schools non-centered by Declared.nuts: μ ${Summary.mean(mus)}%.2f (exact $muMean%.2f), θ ${thetas.map(t => f"${Summary.mean(t)}%.1f").mkString(" ")} (exact ${(0 until 8).map(j => f"${theta(j)._1}%.1f").mkString(" ")})")
    assert(math.abs(Summary.mean(mus) - muMean) < 4 * math.sqrt(muVar) / math.sqrt(Summary.ess(mus)))
    for j <- 0 until 8 do
      val (m, s) = theta(j)
      assert(math.abs(Summary.mean(thetas(j)) - m) < 4 * s / math.sqrt(Summary.ess(thetas(j))), s"θ$j")
      assert(math.abs(Summary.sd(thetas(j)) / s - 1) < 0.1, s"sd θ$j")
  }

  test("a repeated site name is refused at declaration") {
    val e = intercept[IllegalArgumentException](Declared.sites(both(sample("a", Normal(0, 1)), sample("a", Normal(0, 1)))))
    assert(e.getMessage.contains("'a' is declared twice"), e.getMessage)
  }
