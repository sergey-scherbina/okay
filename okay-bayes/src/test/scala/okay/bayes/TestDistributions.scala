package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Distribution.*

/** specs/okay-bayes.md stage 1: every distribution's density against its closed form, its sampler against its moments */
class TestDistributions extends Diagnosed:

  test("logGamma: integers are log factorials, and the half-integer Γ(1/2) = √π") {
    assertEqualsDouble(logGamma(1), 0.0, 1e-12)
    assertEqualsDouble(logGamma(5), math.log(24), 1e-12)
    assertEqualsDouble(logGamma(101), (1 to 100).map(i => math.log(i.toDouble)).sum, 1e-9)
    assertEqualsDouble(logGamma(0.5), 0.5 * math.log(math.Pi), 1e-12)
    assertEqualsDouble(logGamma(0.1), math.log(9.513507698668732), 1e-10)
  }

  test("densities: closed forms, and -∞ outside the support") {
    assertEqualsDouble(Normal(1, 2).logPdf(1), -math.log(2) - 0.5 * math.log(2 * math.Pi), 1e-12)
    assertEqualsDouble(Exponential(0.5).logPdf(2), math.log(0.5) - 1, 1e-12)
    assertEquals(Exponential(0.5).logPdf(-1), NegInf)
    assertEqualsDouble(Gamma(2, 3).logPdf(1), math.log(9) - 3, 1e-12)              // 3² x e^{-3x} / Γ(2)
    assertEqualsDouble(Beta(2, 2).logPdf(0.5), math.log(1.5), 1e-12)               // 6 x (1-x)
    assertEquals(Beta(2, 2).logPdf(1.0), NegInf)
    assertEqualsDouble(Poisson(3).logPdf(2), math.log(9.0 / 2) - 3, 1e-12)
    assertEquals(Poisson(3).logPdf(-1), NegInf)
    assertEqualsDouble(Binomial(10, 0.3).logPdf(3), math.log(120 * math.pow(0.3, 3) * math.pow(0.7, 7)), 1e-12)
    assertEqualsDouble(Bernoulli(0.2).logPdf(true), math.log(0.2), 1e-15)
    assertEqualsDouble(DiscreteUniform(0, 9).logPdf(9), -math.log(10), 1e-15)
    assertEquals(DiscreteUniform(0, 9).logPdf(10), NegInf)
    assertEqualsDouble(Uniform(-1, 3).logPdf(0), -math.log(4), 1e-15)
  }

  /** a sampler's mean and variance within a few standard errors of the closed form */
  def moments[A](d: Distribution[A], mean: Double, variance: Double, n: Int = 40000)(using rng: Random): Unit =
    val xs = Vector.fill(n)(d.numeric(d.sample(rng)))
    val m = Summary.mean(xs)
    val v = Summary.sd(xs) * Summary.sd(xs)
    assert(math.abs(m - mean) < 5 * math.sqrt(variance / n), s"$d: mean $m, expected $mean")
    assert(math.abs(v - variance) < 0.06 * variance + 1e-9, s"$d: variance $v, expected $variance")

  test("samplers: mean and variance of every distribution, small and large Poisson rates both") {
    given Random = Random(7)
    moments(Normal(3, 2), 3, 4)
    moments(Exponential(0.25), 4, 16)
    moments(Gamma(0.5, 2), 0.25, 0.125)                         // the shape < 1 boost
    moments(Gamma(9, 3), 3, 1)
    moments(Beta(2, 5), 2.0 / 7, 10.0 / (49 * 8))
    moments(Uniform(-1, 3), 1, 16.0 / 12)
    moments(Poisson(4.5), 4.5, 4.5)                             // Knuth
    moments(Poisson(120), 120, 120)                             // PTRS
    moments(Bernoulli(0.3), 0.3, 0.21)
    moments(Binomial(20, 0.4), 8, 4.8)
    moments(DiscreteUniform(0, 9), 4.5, 99.0 / 12)
  }

  test("proposals are symmetric moves inside the type, and a trace value comes back only as its own type") {
    val rng = Random(1)
    assertEquals(Bernoulli(0.5).propose(true, 1, rng), false)
    val k = Poisson(10).propose(10, 1, rng)
    assert(k != 10)
    assertEquals(Poisson(3).coerce(5), Some(5))
    // 5.5, not 5.0: on Scala.js there is one number type, and an integral Double IS an Int there
    assertEquals(Poisson(3).coerce(5.5), None)
    assertEquals(Normal(0, 1).coerce(0.5), Some(0.5))
    assertEquals(Normal(0, 1).coerce("x"), None)
  }

  test("a mixture's density sums its components out, and its sampler draws each component by its weight") {
    val m = Mixture(Vector(0.3 -> Normal(0, 1), 0.7 -> Normal(10, 2)))
    for x <- Seq(-3.0, 0.0, 5.0, 10.0, 40.0) do
      assertEqualsDouble(m.logPdf(x), math.log(0.3 * math.exp(Normal(0, 1).logPdf(x)) + 0.7 * math.exp(Normal(10, 2).logPdf(x))), 1e-9)
    assertEqualsDouble(Mixture(Vector(1.0 -> Normal(0, 1))).logPdf(2.5), Normal(0, 1).logPdf(2.5), 1e-12)
    assert(m.logPdf(1e4) > Double.NegativeInfinity, "far in a tail the log-sum-exp still holds a finite density")
    val rng = Random(3)
    val xs = Vector.fill(100000)(m.sample(rng))
    val near = xs.count(_ < 5).toDouble / xs.length
    assert(math.abs(near - 0.3) < 0.01, s"the first component's share $near")
    assertEqualsDouble(xs.sum / xs.length, 7.0, 0.05)
  }
