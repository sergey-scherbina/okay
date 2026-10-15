package okay.bayes

import scala.util.Random
import okay.!
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 1: Metropolis–Hastings against posteriors known in closed form */
class TestBayes extends Diagnosed:

  /** a chain's mean within MC error (4 standard errors at its own ESS) of the truth, its sd within 10% */
  def agrees(xs: Vector[Double], mean: Double, sd: Double, what: String): Unit =
    val m = Summary.mean(xs)
    val s = Summary.sd(xs)
    val ess = Summary.ess(xs)
    val line = f"$what: mean $m%.4f (exact $mean%.4f), sd $s%.4f (exact $sd%.4f), ESS $ess%.0f of ${xs.length}"
    note(line); println(s"  okay-bayes | $line")
    assert(math.abs(m - mean) < 4 * sd / math.sqrt(ess), s"$what: mean $m, exact $mean, ESS $ess")
    assert(math.abs(s - sd) < 0.1 * sd, s"$what: sd $s, exact $sd")

  test("Beta–Bernoulli: 21 heads in 30 under Beta(2, 2) is Beta(23, 11)") {
    val flips = Vector.tabulate(30)(i => i < 21)
    val model = for
      p <- sample("p", Beta(2, 2))
      _ <- observeAll(flips)(_ => Bernoulli(p), identity)
    yield p
    val post = metropolis(model, samples = 20000, burn = 2000)
    agrees(post.draws, 23.0 / 34, math.sqrt(23.0 * 11 / (34 * 34 * 35)), "Beta–Bernoulli")
    assertEquals(post.site("p"), post.draws, "the program's own value is the site's draws, typed")
  }

  test("Gamma–Poisson: counts under Gamma(2, 1) give Gamma(2 + Σk, 1 + n)") {
    val counts = Vector(3, 5, 2, 4, 6, 3, 4, 5, 2, 4)
    val model = for
      l <- sample("lambda", Gamma(2, 1))
      _ <- observeAll(counts)(_ => Poisson(l), identity)
    yield l
    val a = 2.0 + counts.sum
    val b = 1.0 + counts.length
    agrees(metropolis(model, samples = 20000, burn = 2000).draws, a / b, math.sqrt(a) / b, "Gamma–Poisson")
  }

  test("Normal–Normal, known σ: the posterior of the mean in closed form; two chains agree (R-hat ≈ 1)") {
    val rng = Random(3)
    val xs = Vector.fill(50)(1.7 + 2 * rng.nextGaussian())
    val model = for
      mu <- sample("mu", Normal(0, 10))
      _ <- observeAll(xs)(_ => Normal(mu, 2), identity)
    yield mu
    val precision = 1.0 / 100 + xs.length / 4.0
    val post = metropolis(model, samples = 20000, burn = 2000, chains = 2)
    agrees(post.draws, (xs.sum / 4) / precision, math.sqrt(1 / precision), "Normal–Normal")
    val r = post.rhat("mu")
    note(f"R-hat $r%.4f")
    assert(r < 1.01, s"R-hat $r")
  }

  test("a model whose STRUCTURE depends on a draw: the trans-dimensional correction gets the coin right") {
    // coin → which of two sites exists; P(coin | y = 0.3) in closed form:
    // heads: N(0.3; 0, √1.25); tails: ∫ U(-1,1)(y) N(0.3; y, 0.5) dy = (Φ(1.4) − Φ(−2.6)) / 2
    val model = for
      coin <- sample("coin", Bernoulli(0.5))
      x <- if coin then sample("x", Normal(0, 1)) else sample("y", Uniform(-1, 1))
      _ <- observe(Normal(x, 0.5), 0.3)
    yield coin
    val heads = math.exp(-0.09 / 2.5) / math.sqrt(2 * math.Pi * 1.25)
    val tails = (0.9192433407662289 - 0.004661188023718732) / 2
    val exact = heads / (heads + tails)
    val post = metropolis(model, samples = 60000, burn = 5000)
    val p = post.draws.count(identity).toDouble / post.draws.length
    note(f"P(coin) = $p%.4f, exact $exact%.4f"); println(f"  okay-bayes | P(coin) = $p%.4f, exact $exact%.4f")
    assert(math.abs(p - exact) < 0.03, s"P(coin) $p, exact $exact")
  }

  test("prior and likelihood weighting: a forward draw scores its trace; weights normalize") {
    val model = for
      p <- sample("p", Beta(2, 2))
      _ <- observe(Bernoulli(p), true)
    yield p
    val run = prior(model, Random(5))
    assertEqualsDouble(run.logPrior, Beta(2, 2).logPdf(run.value), 1e-12)
    assertEqualsDouble(run.logLik, math.log(run.value), 1e-12)
    val w = weighted(model, 20000, Random(6))
    assertEqualsDouble(w.map(_._2).sum, 1.0, 1e-9)
    val m = w.map((p, wt) => p * wt).sum
    assert(math.abs(m - 3.0 / 5) < 0.01, s"weighted mean $m, exact Beta(3, 2) mean 0.6")
  }

  test("summaries: HDI and quantiles of a known sample; ESS falls for a correlated chain; R-hat sees disagreeing chains") {
    val xs = Vector.tabulate(1001)(i => i.toDouble / 1000)
    assertEqualsDouble(Summary.quantile(xs, 0.5), 0.5, 1e-12)
    val (lo, hi) = Summary.hdi(xs, 0.9)
    assertEqualsDouble(hi - lo, 0.9, 0.002)
    val rng = Random(9)
    val white = Vector.fill(5000)(rng.nextGaussian())
    var x = 0.0
    val ar = Vector.fill(5000) { x = 0.95 * x + rng.nextGaussian(); x }
    assert(Summary.ess(white) > 3500, s"white noise ESS ${Summary.ess(white)}")
    assert(Summary.ess(ar) < 500, s"AR(0.95) ESS ${Summary.ess(ar)}")
    assert(Summary.rhat(Seq(white.map(_ + 3), white)) > 1.5, "two chains centred apart")
  }
