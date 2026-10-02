package okay.bayes

import scala.util.Random

/**
 * THE DIRICHLET: a distribution over probability vectors, conjugate to
 * counts — a flat Dirichlet(1, …, 1) prior and counts nᵢ give the
 * posterior Dirichlet(1 + nᵢ). A posterior helper here; as a model SITE it
 * waits on vector sites (specs/okay-bayes.md §6).
 */
final case class Dirichlet(alpha: Vector[Double]):
  require(alpha.nonEmpty && alpha.forall(_ > 0), s"Dirichlet: every alpha must be positive, got $alpha")
  private val total = alpha.sum
  /** normalised Gamma(αᵢ, 1) draws */
  def sample(rng: Random): Vector[Double] =
    val g = alpha.map(Distribution.gamma1(_, rng))
    val s = g.sum
    g.map(_ / s)
  def logPdf(p: Vector[Double]): Double =
    if p.length != alpha.length || p.exists(x => x <= 0 || x >= 1) || math.abs(p.sum - 1) > 1e-9 then Distribution.NegInf
    else Distribution.logGamma(total) - alpha.map(Distribution.logGamma).sum + alpha.zip(p).map((a, x) => (a - 1) * math.log(x)).sum
  def mean: Vector[Double] = alpha.map(_ / total)
  /** Cov(pᵢ, pⱼ) = (δᵢⱼ αᵢ α₀ − αᵢ αⱼ) / (α₀² (α₀ + 1)) */
  def covariance: Vector[Vector[Double]] =
    Vector.tabulate(alpha.length, alpha.length)((i, j) =>
      ((if i == j then alpha(i) * total else 0.0) - alpha(i) * alpha(j)) / (total * total * (total + 1)))

/**
 * A/B TESTING BY REVENUE (*Bayesian Methods for Hackers* ch.7): a visitor
 * buys one of the tiers `values` (one of them 0, nothing bought) with
 * unknown probabilities; `counts` per tier make a Dirichlet posterior, and
 * the expected revenue per visitor Σ vᵢ pᵢ is a posterior draw per
 * Dirichlet draw. A conversion rate alone would call a variant selling more
 * cheap tiers the winner; revenue asks the question the business has.
 */
object AbTest:
  final case class Variant(values: Vector[Double], counts: Vector[Int]):
    require(values.length == counts.length, "AbTest.Variant: one count per value")
    def posterior: Dirichlet = Dirichlet(counts.map(1.0 + _))
    def visitors: Int = counts.sum
    /** the naive estimate: revenue observed per visitor */
    def observed: Double = values.zip(counts).map((v, n) => v * n).sum / visitors
    /** the exact posterior mean and sd of the revenue per visitor */
    def exact: (Double, Double) =
      val d = posterior
      val cov = d.covariance
      (values.zip(d.mean).map(_ * _).sum,
        math.sqrt(values.indices.map(i => values.indices.map(j => values(i) * cov(i)(j) * values(j)).sum).sum))

  /** `n` posterior draws of a variant's expected revenue per visitor */
  def revenue(v: Variant, n: Int, rng: Random): Vector[Double] =
    val d = v.posterior
    Vector.fill(n)(v.values.zip(d.sample(rng)).map(_ * _).sum)

  /** P(b's revenue per visitor exceeds a's), and the posterior draws of the difference b − a */
  def compare(a: Variant, b: Variant, n: Int, rng: Random): (Double, Vector[Double]) =
    val diff = revenue(b, n, rng).zip(revenue(a, n, rng)).map(_ - _)
    (diff.count(_ > 0).toDouble / n, diff)
