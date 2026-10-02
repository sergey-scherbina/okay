package okay.bayes

import scala.util.Random
import okay.!
import okay.testkit.Munit.Diagnosed
import Smooth.{param, observe, observeAll}
import Real.{exp, log, log1p, sqrt, pow, softplus, sigmoid, lgamma, logSumExp}

/** specs/okay-bayes.md stage 3b: reverse-mode AD, and NUTS on its exact gradients */
class TestSmooth extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  /** the tape's gradient against a central difference of the same function */
  def checkGradient(what: String, f: IndexedSeq[Real] => Real, at: Vector[Double]): Unit =
    val tape = new Tape
    val in = at.map(tape.variable)
    val out = f(in)
    val g = tape.gradient(out, in)
    for i <- at.indices do
      val h = 1e-6 * math.max(1, math.abs(at(i)))
      def value(x: Double) = f(at.updated(i, x).map(Real.const)).value
      val fd = (value(at(i) + h) - value(at(i) - h)) / (2 * h)
      assertEqualsDouble(g(i), fd, 1e-5 * math.max(1, math.abs(fd)), s"$what: d/dx$i")
    assertEqualsDouble(out.value, f(at.map(Real.const)).value, 0.0, s"$what: the value off the tape")

  test("every operation's derivative against a central difference") {
    checkGradient("arithmetic", x => (x(0) * x(1) - x(2) / x(0) + 3.0 * x(1)) / (2.0 - x(2)), Vector(1.3, -0.7, 0.4))
    checkGradient("exp, log, log1p, sqrt, pow", x => exp(x(0)) * log(x(1)) + log1p(x(2)) - sqrt(x(1)) + pow(x(1), 2.5), Vector(0.3, 2.1, 0.6))
    checkGradient("softplus, sigmoid, negation", x => softplus(x(0)) - sigmoid(-x(1)) + softplus(-x(0) * 40.0), Vector(-0.8, 1.7))
    checkGradient("lgamma (digamma)", x => lgamma(x(0)) + lgamma(x(1)) - lgamma(x(0) + x(1)), Vector(0.35, 7.9))
    checkGradient("log-sum-exp", x => logSumExp(Seq(x(0), x(1) * 2.0, -x(2))), Vector(1.0, -3.0, 0.5))
    checkGradient("a reused intermediate", x => { val y = x(0) * x(0); y * y + y }, Vector(1.7))
    assertEqualsDouble(Real.digamma(1), -0.5772156649015329, 1e-12)
    assertEqualsDouble(Real.digamma(0.5), -1.9635100260214235, 1e-12)
  }

  // a logistic regression, written both ways: an ordinary Model and a Grad program
  val xs: Vector[Double] = Vector.tabulate(40)(i => -2.0 + i / 10.0)
  val ys: Vector[Boolean] = { val rng = Random(9); xs.map(x => rng.nextDouble() < 1 / (1 + math.exp(-(1.5 * x - 0.3)))) }

  val logisticAd = for
    a <- param("a", Smooth.Normal(0, 5))
    b <- param("b", Smooth.Normal(0, 5))
    s <- param("s", Smooth.Exponential(1))
    _ <- observeAll(xs.zip(ys))(xy => Smooth.BernoulliLogit((a + b * xy._1) / (1.0 + s)), _._2)
  yield (a.value, b.value)

  /** the same density by hand from `Distribution`, s through the log with its Jacobian */
  def byHand(u: Array[Double]): Double =
    val (s, logJ) = Support.Positive.constrain(u(2))
    Distribution.Normal(0, 5).logPdf(u(0)) + Distribution.Normal(0, 5).logPdf(u(1)) + Distribution.Exponential(1).logPdf(s) + logJ +
      xs.zip(ys).map((x, y) => Distribution.Bernoulli(1 / (1 + math.exp(-(u(0) + u(1) * x) / (1 + s)))).logPdf(y)).sum

  test("the same model written both ways has the same log density, and the AD gradient is the finite one") {
    val t = Smooth.target(logisticAd)
    assertEquals(t.dim, 3)
    assertEquals(Smooth.names(logisticAd), Vector("a", "b", "s"))
    val finite = Target.finite(3)(byHand)
    val rng = Random(4)
    for _ <- 1 to 20 do
      val u = Array.fill(3)(rng.nextGaussian() * 2)
      val (lp, g) = t.gradient(u)
      assertEqualsDouble(lp, byHand(u), 1e-9 * math.abs(lp))
      assertEqualsDouble(t.logp(u), lp, 0.0)
      val (_, fg) = finite.gradient(u)
      for i <- 0 until 3 do assertEqualsDouble(g(i), fg(i), 1e-5 * math.max(1, math.abs(fg(i))), s"d/du$i at ${u.toVector}")
  }

  /** mean within 4 standard errors at its own ESS, sd within 10% */
  def agrees(xs: Vector[Double], mean: Double, sd: Double, what: String): Unit =
    val (m, s, ess) = (Summary.mean(xs), Summary.sd(xs), Summary.ess(xs))
    report(f"AD NUTS $what: mean $m%.4f (exact $mean%.4f), sd $s%.4f (exact $sd%.4f), ESS $ess%.0f of ${xs.length}")
    assert(math.abs(m - mean) < 4 * sd / math.sqrt(ess), s"$what: mean $m, exact $mean, ESS $ess")
    assert(math.abs(s - sd) < 0.1 * sd, s"$what: sd $s, exact $sd")

  test("conjugates through the three transforms, by Smooth.nuts") {
    val obs = Vector(1.2, 2.3, 0.7, 1.9, 1.4)
    val nn = for
      m <- param("m", Smooth.Normal(0, 2))
      _ <- observeAll(obs)(_ => Smooth.Normal(m, 1), y => Real.const(y))
    yield m.value
    val prec = 0.25 + obs.length
    agrees(Smooth.nuts(nn, samples = 3000).draws, obs.sum / prec, math.sqrt(1 / prec), "Normal–Normal")

    val counts = Vector(3, 5, 2, 4, 6, 1, 4)
    val gp = for
      l <- param("l", Smooth.Gamma(2, 1))
      _ <- observeAll(counts)(_ => Smooth.Poisson(l), identity)
    yield l.value
    val (shape, rate) = (2.0 + counts.sum, 1.0 + counts.length)
    agrees(Smooth.nuts(gp, samples = 3000).draws, shape / rate, math.sqrt(shape) / rate, "Gamma–Poisson")

    val flips = Vector.tabulate(30)(i => i < 21)
    val bb = for
      q <- param("q", Smooth.Beta(2, 2))
      _ <- observeAll(flips)(_ => Smooth.Bernoulli(q), identity)
    yield q.value
    agrees(Smooth.nuts(bb, samples = 3000).draws, 23.0 / 34, math.sqrt(23.0 * 11 / (34 * 34 * 35)), "Beta–Bernoulli")
  }

  test("the cost at d = 100: one AD gradient against one by central differences (201 runs)") {
    val d = 100
    val data = Vector.tabulate(d)(i => math.sin(i.toDouble))
    def means(i: Int): Unit ! Grad =
      if i == d then okay.pure[Grad, Unit](())
      else param(s"mu$i", Smooth.Normal(0, 1)).flatMap(m => observe(Smooth.Normal(m, 1), Real.const(data(i))).flatMap(_ => means(i + 1)))
    val t = Smooth.target(means(0))
    val finite = Target.finite(d)(t.logp)
    val u = Array.tabulate(d)(i => 0.1 * i - 3)
    val (_, g) = t.gradient(u)
    val (_, fg) = finite.gradient(u)
    for i <- 0 until d do assertEqualsDouble(g(i), fg(i), 1e-4 * math.max(1, math.abs(fg(i))))
    // the exact gradient of N(0,1) prior and N(mu,1) likelihood: −mu − (mu − y)
    for i <- 0 until d do assertEqualsDouble(g(i), -u(i) - (u(i) - data(i)), 1e-12)
    def best(f: () => Unit): Double =
      (1 to 7).map { _ => val t0 = System.nanoTime(); f(); (System.nanoTime() - t0) / 1e6 }.min
    val ad = best(() => { val _ = t.gradient(u) })
    val fd = best(() => { val _ = finite.gradient(u) })
    report(f"gradient at d = $d: AD $ad%.2f ms, central differences $fd%.2f ms (${fd / ad}%.0fx)")
    assert(ad < fd / 5, s"one AD gradient ($ad ms) should cost a fraction of 201 runs ($fd ms)")
  }
