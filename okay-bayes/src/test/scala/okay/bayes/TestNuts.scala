package okay.bayes

import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 3a: NUTS on unconstrained space, against closed forms */
class TestNuts extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  /** mean within 4 standard errors at its own ESS, sd within 10% */
  def agrees(xs: Vector[Double], mean: Double, sd: Double, what: String): Unit =
    val (m, s, ess) = (Summary.mean(xs), Summary.sd(xs), Summary.ess(xs))
    report(f"NUTS $what: mean $m%.4f (exact $mean%.4f), sd $s%.4f (exact $sd%.4f), ESS $ess%.0f of ${xs.length}")
    assert(math.abs(m - mean) < 4 * sd / math.sqrt(ess), s"$what: mean $m, exact $mean, ESS $ess")
    assert(math.abs(s - sd) < 0.1 * sd, s"$what: sd $s, exact $sd")

  test("supports: constrain and unconstrain are inverse, and the log Jacobian is the derivative's log") {
    for (s, u) <- Seq(Support.Real -> 0.7, Support.Positive -> -1.3, Support.Interval(2, 5) -> 0.4, Support.Interval(0, 1) -> -30.0) do
      val (x, logJ) = s.constrain(u)
      val h = 1e-6
      val slope = (s.constrain(u + h)._1 - s.constrain(u - h)._1) / (2 * h)
      assertEqualsDouble(s.unconstrain(x), u, 1e-6 * math.max(1, math.abs(u)))
      assertEqualsDouble(logJ, math.log(slope), 1e-4)
  }

  test("conjugates on ℝ, through the log and through the logit: Normal–Normal, Gamma–Poisson, Beta–Bernoulli") {
    val ys = Vector(1.2, 2.3, 0.7, 1.9, 1.4)
    val nn = for
      m <- sample("m", Normal(0, 2))
      _ <- observeAll(ys)(_ => Normal(m, 1), identity)
    yield m
    // posterior precision 1/4 + 5, mean (Σy) / precision
    val prec = 0.25 + ys.length
    agrees(nuts(nn, samples = 3000, burn = 1000).draws, ys.sum / prec, math.sqrt(1 / prec), "Normal–Normal")

    val counts = Vector(3, 5, 2, 4, 6, 1, 4)
    val gp = for
      l <- sample("l", Gamma(2, 1))
      _ <- observeAll(counts)(_ => Poisson(l), identity)
    yield l
    val (shape, rate) = (2.0 + counts.sum, 1.0 + counts.length)
    agrees(nuts(gp, samples = 3000, burn = 1000).draws, shape / rate, math.sqrt(shape) / rate, "Gamma–Poisson")

    val flips = Vector.tabulate(30)(i => i < 21)
    val bb = for
      q <- sample("q", Beta(2, 2))
      _ <- observeAll(flips)(_ => Bernoulli(q), identity)
    yield q
    val post = nuts(bb, samples = 3000, burn = 1000)
    agrees(post.draws, 23.0 / 34, math.sqrt(23.0 * 11 / (34 * 34 * 35)), "Beta–Bernoulli")
    assert(post.draws.forall(q => q > 0 && q < 1), "every draw inside the support")
    report(f"NUTS Beta–Bernoulli: acceptance ${post.acceptance("(nuts accept)")}%.2f, divergent ${post.acceptance("(divergent)")}%.4f, tree depth ${post.acceptance("(tree depth)")}%.2f")
  }

  test("a discrete site is refused by name") {
    val m = for
      k <- sample("k", Poisson(3))
      _ <- observe(Normal(k.toDouble, 1), 2.5)
    yield k
    val e = intercept[IllegalArgumentException](nuts(m, samples = 10, burn = 10))
    assert(e.getMessage.contains("'k' is discrete"), e.getMessage)
  }
