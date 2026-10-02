package okay.bayes

import scala.io.Source
import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** Bayesian Methods for Hackers, ch.2 — the A/B test and the Challenger O-ring model, on the book's data */
object Ch2:
  /** (temperature °F, damaged) for the 23 flights with a known outcome — the book drops "NA" and the accident row */
  val flights: Vector[(Double, Boolean)] =
    Source.fromInputStream(getClass.getResourceAsStream("/bmh/challenger_data.csv")).getLines().drop(1)
      .map(_.split(',')).collect { case Array(_, t, d) if d == "0" || d == "1" => (t.toDouble, d == "1") }.toVector

  /** the book's logistic: p(t) = 1 / (1 + e^(βt + α)) */
  def p(t: Double, alpha: Double, beta: Double): Double = 1.0 / (1.0 + math.exp(beta * t + alpha))

  /** the book's priors: Normal(0, τ = 0.001), i.e. sd = 1/√0.001 ≈ 31.6 */
  val sd: Double = 1 / math.sqrt(0.001)

  val challenger = for
    beta <- sample("beta", Normal(0, sd))
    alpha <- sample("alpha", Normal(0, sd))
    _ <- observeAll(flights)(f => Bernoulli(p(f._1, alpha, beta)), _._2)
  yield (alpha, beta, p(31, alpha, beta))

  /** the posterior by brute force: a fine 2-D grid over (α, β), the log posterior exponentiated
   * against its maximum; answers E[α], E[β], E[p(31°F)], sd(β) and the weight on the grid's edge */
  lazy val grid: (Double, Double, Double, Double, Double) =
    val (na, nb) = (2400, 2400)
    val (a0, a1, b0, b1) = (-90.0, 10.0, -0.15, 1.35)
    val logs = Array.ofDim[Double](na, nb)
    var top = Double.NegativeInfinity
    for i <- 0 until na; j <- 0 until nb do
      val a = a0 + (a1 - a0) * i / (na - 1)
      val b = b0 + (b1 - b0) * j / (nb - 1)
      var l = Normal(0, sd).logPdf(a) + Normal(0, sd).logPdf(b)
      for (t, d) <- flights do l += Bernoulli(p(t, a, b)).logPdf(d)
      logs(i)(j) = l
      if l > top then top = l
    var (z, ea, eb, eb2, ep, edge) = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    for i <- 0 until na; j <- 0 until nb do
      val a = a0 + (a1 - a0) * i / (na - 1)
      val b = b0 + (b1 - b0) * j / (nb - 1)
      val w = math.exp(logs(i)(j) - top)
      z += w; ea += w * a; eb += w * b; eb2 += w * b * b; ep += w * p(31, a, b)
      if i == 0 || j == 0 || i == na - 1 || j == nb - 1 then edge += w
    val mb = eb / z
    (ea / z, mb, ep / z, math.sqrt(eb2 / z - mb * mb), edge / z)

class TestHackersCh2 extends Diagnosed:
  import Ch2.*
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  test("the A/B test: two Uniform priors, two Bernoulli samples — the posteriors of pA, pB and P(pA > pB) are exact Betas") {
    // the book's simulation: site A converts at 5% of 1500 visitors, site B at 4% of 750
    val rng = Random(2015)
    val a = Vector.fill(1500)(rng.nextDouble() < 0.05)
    val b = Vector.fill(750)(rng.nextDouble() < 0.04)
    val model = for
      pa <- sample("p_A", Uniform(0, 1))
      pb <- sample("p_B", Uniform(0, 1))
      _ <- observeAll(a)(_ => Bernoulli(pa), identity)
      _ <- observeAll(b)(_ => Bernoulli(pb), identity)
    yield (pa, pb, pa - pb)
    val (ka, kb) = (a.count(identity), b.count(identity))
    val (postA, postB) = (Beta(1.0 + ka, 1.0 + a.length - ka), Beta(1.0 + kb, 1.0 + b.length - kb))
    // P(pA > pB) exactly: ∫ f_A(x) F_B(x) dx, by the trapezoid on a fine grid
    val xs = (0 to 20000).map(_ / 20000.0)
    val fb = xs.map(x => math.exp(postB.logPdf(x)))
    val cdfB = fb.scanLeft(0.0)(_ + _ / 20000.0).tail
    val exactWins = xs.indices.map(i => math.exp(postA.logPdf(xs(i))) * cdfB(i) / 20000.0).sum
    val post = metropolis(model, samples = 40000, burn = 5000)
    val wins = post.draws.count(_._3 > 0).toDouble / post.draws.length
    val meanA = Summary.mean(post.site("p_A"))
    println(f"  okay-bayes | A/B: pA ${meanA}%.5f (exact ${(1.0 + ka) / (2.0 + a.length)}%.5f), P(pA > pB) $wins%.4f (exact $exactWins%.4f)")
    assert(math.abs(meanA - (1.0 + ka) / (2.0 + a.length)) < 0.002)
    assert(math.abs(wins - exactWins) < 0.03, s"P(pA > pB) $wins vs exact $exactWins")
  }

  test("Challenger: the logistic posterior by Metropolis–Hastings against a 2-D grid — and what single-site MH costs here") {
    assertEquals(flights.length, 23)
    val (ea, eb, ep, sdb, edge) = grid
    println(f"  okay-bayes | Challenger grid: E[α] = $ea%.3f, E[β] = $eb%.4f, sd(β) = $sdb%.4f, E[p(31°F)] = $ep%.4f, edge weight $edge%.1e")
    assert(edge < 1e-6, s"the grid holds the posterior: edge weight $edge")
    // the book's own run: 120 000 samples after 100 000 burn-in, thinned by 2
    val post = metropolis(challenger, samples = 60000, burn = 100000, thin = 2, chains = 2)
    val b = post.site("beta")
    val a = post.site("alpha")
    val p31 = post.draws.map(_._3)
    val essB = Summary.ess(b)
    println(f"  okay-bayes | Challenger MH:   E[α] = ${Summary.mean(a)}%.3f, E[β] = ${Summary.mean(b)}%.4f, sd(β) = ${Summary.sd(b)}%.4f, " +
      f"E[p(31°F)] = ${Summary.mean(p31)}%.4f, ESS(β) $essB%.0f of ${b.length}, R-hat(β) ${post.rhat("beta")}%.4f, acceptance ${post.acceptance}")
    assert(math.abs(Summary.mean(b) - eb) < 4 * sdb / math.sqrt(essB), s"E[β] ${Summary.mean(b)} vs grid $eb at ESS $essB")
    assert(math.abs(Summary.mean(p31) - ep) < 0.02, s"E[p(31)] ${Summary.mean(p31)} vs grid $ep")
    assert(post.rhat("beta") < 1.05, s"R-hat ${post.rhat("beta")}")
  }

  test("Challenger by ADAPTIVE Metropolis: one joint step along the α–β ridge — the same posterior at many times the ESS") {
    val (_, eb, ep, sdb, _) = grid
    val post = adaptive(challenger, samples = 60000, burn = 20000, chains = 2)
    val b = post.site("beta")
    val essB = Summary.ess(b)
    val p31 = post.draws.map(_._3)
    println(f"  okay-bayes | Challenger AM:   E[α] = ${Summary.mean(post.site("alpha"))}%.3f, E[β] = ${Summary.mean(b)}%.4f, sd(β) = ${Summary.sd(b)}%.4f, " +
      f"E[p(31°F)] = ${Summary.mean(p31)}%.4f, ESS(β) $essB%.0f of ${b.length}, R-hat(β) ${post.rhat("beta")}%.4f, acceptance ${post.acceptance}")
    assert(essB > 2000, s"the joint step mixes along the ridge: ESS(β) $essB")
    assert(math.abs(Summary.mean(b) - eb) < 4 * sdb / math.sqrt(essB), s"E[β] ${Summary.mean(b)} vs grid $eb at ESS $essB")
    assert(math.abs(Summary.sd(b) - sdb) < 0.1 * sdb, s"sd(β) ${Summary.sd(b)} vs grid $sdb")
    assert(math.abs(Summary.mean(p31) - ep) < 0.005, s"E[p(31)] ${Summary.mean(p31)} vs grid $ep")
  }

