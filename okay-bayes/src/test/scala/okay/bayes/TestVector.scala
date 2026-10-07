package okay.bayes

import okay.freer.!
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md §6: a vector as n named scalars — eight schools with τ fixed, against its exact posterior */
class TestVector extends Diagnosed:
  // two samplers on a nine-parameter model: about a second quiet, and 79 s in a loaded whole build
  // (ci-runner 2026-10-02) — a budget for CPU, as the module's other sampling suites carry
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // eight schools (Rubin 1981; Gelman et al., BDA §5.5): coaching effects and their standard errors
  val y = Vector(28.0, 8, -3, 7, -1, 1, 18, 12)
  val sigma = Vector(15.0, 10, 16, 11, 9, 11, 10, 18)
  val tau = 5.0

  val schools = for
    mu <- sample("mu", Normal(0, 5))
    theta <- sampleN("theta", Normal(mu, tau), 8)
    _ <- observeAll(theta.indices)(j => Normal(theta(j), sigma(j)), y)
  yield mu

  val schoolsAd = for
    mu <- Smooth.param("mu", Smooth.Normal(0, 5))
    theta <- Smooth.paramN("theta", Smooth.Normal(mu, tau), 8)
    _ <- Smooth.observeAll(theta.indices)(j => Smooth.Normal(theta(j), sigma(j)), j => Real.const(y(j)))
  yield mu.value

  /** jointly Gaussian with τ fixed: yⱼ ~ N(μ, τ² + σⱼ²) gives μ; θⱼ given μ is a precision-weighted mean, linear in μ */
  val (muMean, muVar) =
    val prec = 1 / 25.0 + sigma.map(s => 1 / (tau * tau + s * s)).sum
    (y.indices.map(j => y(j) / (tau * tau + sigma(j) * sigma(j))).sum / prec, 1 / prec)
  def theta(j: Int): (Double, Double) =
    val prec = 1 / (sigma(j) * sigma(j)) + 1 / (tau * tau)
    val w = 1 / (tau * tau * prec)
    ((y(j) / (sigma(j) * sigma(j)) + muMean / (tau * tau)) / prec, math.sqrt(1 / prec + w * w * muVar))

  def agrees(xs: Vector[Double], mean: Double, sd: Double, what: String): Unit =
    val (m, s, ess) = (Summary.mean(xs), Summary.sd(xs), Summary.ess(xs))
    assert(math.abs(m - mean) < 4 * sd / math.sqrt(ess), f"$what: mean $m%.3f, exact $mean%.3f, ESS $ess%.0f")
    assert(math.abs(s - sd) < 0.1 * sd, f"$what: sd $s%.3f, exact $sd%.3f")

  def check(post: Posterior[Double], by: String): Unit =
    val thetas = post.vector("theta")
    assertEquals(thetas.length, 8)
    assertEquals(post.site("mu"), post.draws, "the program's value is μ")
    agrees(post.draws, muMean, math.sqrt(muVar), s"$by μ")
    for j <- 0 until 8 do agrees(thetas(j), theta(j)._1, theta(j)._2, s"$by θ$j")
    report(f"eight schools, τ = 5, $by: μ ${Summary.mean(post.draws)}%.2f (exact $muMean%.2f), θ ${thetas.map(t => f"${Summary.mean(t)}%.1f").mkString(" ")} (exact ${(0 until 8).map(j => f"${theta(j)._1}%.1f").mkString(" ")})")

  test("sampleN names the elements name[i], in order, and Posterior.vector reads them back") {
    val r = prior(sampleN("x", Normal(0, 1), 3), scala.util.Random(1))
    assertEquals(r.sites.keySet, Set("x[0]", "x[1]", "x[2]"))
    assertEquals(r.value, Vector(r.sites("x[0]"), r.sites("x[1]"), r.sites("x[2]")))
    assertEquals(Smooth.names(Smooth.paramN("w", Smooth.Normal(0, 1), 12)), Vector.tabulate(12)(i => s"w[$i]"))
  }

  test("eight schools with τ fixed, by adaptive Metropolis, against the exact posterior") {
    check(adaptive(schools, samples = 4000, burn = 1500, chains = 2), "adaptive")
  }

  test("eight schools with τ fixed, by AD NUTS, against the exact posterior") {
    check(Smooth.nuts(schoolsAd, samples = 1500, burn = 500, chains = 2), "AD NUTS")
  }
