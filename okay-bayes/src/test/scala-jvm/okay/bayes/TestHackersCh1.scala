package okay.bayes

import scala.io.Source
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** Bayesian Methods for Hackers, ch.1 "Inferring behaviour from text-message data", on the book's own data
 * (src/test/resources/bmh, MIT) — against its EXACT posterior: Exponential(α) is Gamma(1, α), conjugate
 * to the Poisson, so both rates integrate out and τ's posterior is a finite sum */
object Ch1:
  val counts: Vector[Int] =
    Source.fromInputStream(getClass.getResourceAsStream("/bmh/txtdata.csv")).getLines().map(_.trim.toDouble.toInt).toVector
  val n: Int = counts.length
  val alpha: Double = 1.0 / (counts.sum.toDouble / n)

  /** the book's model, line for line (PyMC: lambda_1, lambda_2 ~ Exponential(alpha); tau ~ DiscreteUniform(0, n - 1)) */
  val texting = for
    l1 <- sample("lambda_1", Exponential(alpha))
    l2 <- sample("lambda_2", Exponential(alpha))
    tau <- sample("tau", DiscreteUniform(0, n - 1))
    _ <- observeAll(counts.zipWithIndex)((_, day) => Poisson(if day < tau then l1 else l2), _._1)
  yield (l1, l2, tau)

  /** log ∫ Π_{days} Poisson(c | λ) · Exponential(λ | α) dλ, up to Π c! (common to every τ) */
  private def segment(sum: Int, days: Int): Double =
    math.log(alpha) + logGamma(sum + 1.0) - (sum + 1.0) * math.log(days + alpha)

  /** the exact posterior of τ, and the exact posterior means of λ1 and λ2 */
  lazy val exact: (Map[Int, Double], Double, Double) =
    val logs = (0 until n).map { tau =>
      val (a, b) = counts.splitAt(tau)
      tau -> (segment(a.sum, a.length) + segment(b.sum, b.length))
    }.toMap
    val top = logs.values.max
    val z = logs.values.map(l => math.exp(l - top)).sum
    val pTau = logs.view.mapValues(l => math.exp(l - top) / z).toMap
    val (a1, a2) = pTau.foldLeft((0.0, 0.0)) { case ((m1, m2), (tau, p)) =>
      val (a, b) = counts.splitAt(tau)
      (m1 + p * (a.sum + 1.0) / (a.length + alpha), m2 + p * (b.sum + 1.0) / (b.length + alpha))
    }
    (pTau, a1, a2)

class TestHackersCh1 extends Diagnosed:
  import Ch1.*
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  test("the book's data: 74 days of text counts") {
    assertEquals(n, 74)
    assertEquals(counts.take(3), Vector(13, 24, 8))
  }

  test("the texting model by Metropolis–Hastings agrees with the exact posterior: τ at 44–45, λ1 ≈ 18, λ2 ≈ 23") {
    val (pTau, m1, m2) = exact
    println(f"  okay-bayes | exact: E[λ1] = $m1%.3f, E[λ2] = $m2%.3f, P(τ = 44) = ${pTau(44)}%.3f, P(τ = 45) = ${pTau(45)}%.3f")
    note(f"exact: E[λ1] = $m1%.3f, E[λ2] = $m2%.3f, P(τ = 44) = ${pTau(44)}%.3f, P(τ = 45) = ${pTau(45)}%.3f")
    val post = metropolis(texting, samples = 30000, burn = 5000, chains = 2)
    val l1 = post.site("lambda_1")
    val l2 = post.site("lambda_2")
    val taus = post.draws.map(_._3)
    val p44 = taus.count(_ == 44).toDouble / taus.length
    val p45 = taus.count(_ == 45).toDouble / taus.length
    println(f"  okay-bayes | MH:    E[λ1] = ${Summary.mean(l1)}%.3f, E[λ2] = ${Summary.mean(l2)}%.3f, P(τ = 44) = $p44%.3f, P(τ = 45) = $p45%.3f, HDI94(λ1) = ${Summary.hdi(l1)}, R-hat(λ1) = ${post.rhat("lambda_1")}%.4f, acceptance ${post.acceptance}")
    note(f"MH:    E[λ1] = ${Summary.mean(l1)}%.3f, E[λ2] = ${Summary.mean(l2)}%.3f, P(τ = 44) = $p44%.3f, P(τ = 45) = $p45%.3f, " +
      f"HDI(λ1) = ${Summary.hdi(l1)}, R-hat(λ1) = ${post.rhat("lambda_1")}%.4f, acceptance ${post.acceptance}")
    assert(math.abs(Summary.mean(l1) - m1) < 0.15, s"E[λ1] ${Summary.mean(l1)} vs exact $m1")
    assert(math.abs(Summary.mean(l2) - m2) < 0.2, s"E[λ2] ${Summary.mean(l2)} vs exact $m2")
    assert(math.abs(p44 + p45 - (pTau(44) + pTau(45))) < 0.04, s"P(τ ∈ {44, 45}) ${p44 + p45} vs exact ${pTau(44) + pTau(45)}")
    assert(post.rhat("lambda_1") < 1.02 && post.rhat("lambda_2") < 1.02)
    assert(m1 > 17 && m1 < 19 && m2 > 22 && m2 < 24, "and the exact numbers are the book's")
  }
