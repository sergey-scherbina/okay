package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import AbTest.Variant

/** specs/okay-bayes.md stage 5c: Bayesian Methods for Hackers ch.7 — A/B testing by expected revenue */
class TestAbTest extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("Dirichlet's moments against its closed forms") {
    val d = Dirichlet(Vector(2.0, 5.0, 3.0))
    val rng = Random(8)
    val xs = Vector.fill(200000)(d.sample(rng))
    for i <- 0 until 3 do
      val col = xs.map(_(i))
      assertEqualsDouble(Summary.mean(col), d.mean(i), 0.002)
      assertEqualsDouble(Summary.sd(col) * Summary.sd(col), d.covariance(i)(i), 0.0005)
    val c01 = xs.map(x => (x(0) - d.mean(0)) * (x(1) - d.mean(1))).sum / xs.length
    assertEqualsDouble(c01, d.covariance(0)(1), 0.0005)
    assert(xs.forall(x => math.abs(x.sum - 1) < 1e-12))
    // Dirichlet(1, 1) is Uniform on the simplex: density 1 everywhere inside
    assertEqualsDouble(Dirichlet(Vector(1.0, 1.0)).logPdf(Vector(0.3, 0.7)), 0.0, 1e-12)
    assertEqualsDouble(Dirichlet(Vector(2.0, 3.0)).logPdf(Vector(0.4, 0.6)), Distribution.Beta(2, 3).logPdf(0.4), 1e-12)
  }

  // tiers $79, $49, $25 and nothing: A had 1000 visitors, B 2000
  val values = Vector(79.0, 49.0, 25.0, 0.0)
  val a = Variant(values, Vector(10, 46, 80, 864))
  val b = Variant(values, Vector(45, 84, 200, 1671))

  test("each variant's revenue per visitor: the posterior mean and sd are the exact ones") {
    for (name, v) <- Seq("A" -> a, "B" -> b) do
      val xs = AbTest.revenue(v, 100000, Random(name.hashCode))
      val (m, s) = v.exact
      report(f"variant $name: revenue per visitor ${Summary.mean(xs)}%.3f ± ${Summary.sd(xs)}%.3f (exact $m%.3f ± $s%.3f), observed ${v.observed}%.3f")
      assertEqualsDouble(Summary.mean(xs), m, 4 * s / math.sqrt(xs.length))
      assertEqualsDouble(Summary.sd(xs), s, 0.02 * s)
  }

  test("P(B beats A) agrees between two independent runs, and is reported beside the conversion rates") {
    val (p1, diff) = AbTest.compare(a, b, 100000, Random(1))
    val (p2, _) = AbTest.compare(a, b, 100000, Random(2))
    val conv = (v: Variant) => 1 - v.counts.last.toDouble / v.visitors
    report(f"P(B's revenue per visitor beats A's) $p1%.4f (an independent run $p2%.4f); lift ${Summary.mean(diff)}%.3f per visitor, 95%% HDI [${Summary.hdi(diff, 0.95)._1}%.3f, ${Summary.hdi(diff, 0.95)._2}%.3f]; conversion A ${conv(a)}%.4f, B ${conv(b)}%.4f")
    assert(math.abs(p1 - p2) < 4 * math.sqrt(p1 * (1 - p1) * 2 / 100000) + 1e-9)
    val (ma, sa) = a.exact
    val (mb, sb) = b.exact
    assertEqualsDouble(Summary.mean(diff), mb - ma, 4 * math.sqrt(sa * sa + sb * sb) / math.sqrt(100000))
  }
