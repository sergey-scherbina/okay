package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Distribution.*

/** specs/okay-bayes.md stage 5a: Bayesian Methods for Hackers ch.4 — the exact Beta quantile, ranking, the law of large numbers */
class TestRank extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("Beta.cdf against closed forms and symmetry; quantile is its inverse") {
    for x <- Seq(0.0, 0.1, 0.37, 0.5, 0.93, 1.0) do
      assertEqualsDouble(Beta(1, 1).cdf(x), x, 1e-14)
      assertEqualsDouble(Beta(3.5, 1).cdf(x), math.pow(x, 3.5), 1e-13)
      assertEqualsDouble(Beta(2, 2).cdf(x), 3 * x * x - 2 * x * x * x, 1e-13)
      assertEqualsDouble(Beta(4.2, 9.1).cdf(x), 1 - Beta(9.1, 4.2).cdf(1 - x), 1e-13)
    // a large Beta, where the continued fraction does the work: the median of a symmetric one is 1/2
    assertEqualsDouble(Beta(800, 800).cdf(0.5), 0.5, 1e-12)
    for (a, b) <- Seq((1.0, 1.0), (2.0, 5.0), (0.5, 0.5), (30.0, 3.0), (701.0, 201.0)); p <- Seq(1e-6, 0.05, 0.5, 0.95) do
      val x = Beta(a, b).quantile(p)
      assertEqualsDouble(Beta(a, b).cdf(x), p, 1e-10 * math.max(1, p * 100), s"Beta($a, $b).quantile($p) = $x")
    assertEqualsDouble(Beta(2, 1).quantile(0.05), math.sqrt(0.05), 1e-12)
  }

  test("the book's approximate lower bound against the exact one: close for many votes, not for few") {
    for (u, d) <- Seq((1, 0), (5, 2), (50, 20), (500, 200), (5000, 2000)) do
      val exact = Rank.lowerBound(u, d)
      val approx = Rank.approxLowerBound(u, d)
      report(f"5%% lower bound, $u up $d down: exact $exact%.4f, the book's approximation $approx%.4f (off by ${approx - exact}%+.4f)")
    assert(math.abs(Rank.approxLowerBound(500, 200) - Rank.lowerBound(500, 200)) < 0.002)
    assert(math.abs(Rank.approxLowerBound(1, 0) - Rank.lowerBound(1, 0)) > 0.05, "one vote: the normal approximation is far off")
  }

  test("ranking by the lower bound: one vote of one does not beat 999 of 1000, and among equal ratios more evidence wins") {
    val items = Vector("one of one" -> (1, 0), "999 of 1000" -> (999, 1), "3 of 4" -> (3, 1), "75 of 100" -> (75, 25), "750 of 1000" -> (750, 250))
    val ranked = Rank.sort(items)(_._2).map(_._1)
    val naive = items.sortBy { case (_, (u, d)) => -u.toDouble / (u + d) }.map(_._1)
    report(s"ranked by the ratio: ${naive.mkString(", ")}; by the 5% lower bound: ${ranked.mkString(", ")}")
    assertEquals(ranked.head, "999 of 1000")
    assert(ranked.indexOf("750 of 1000") < ranked.indexOf("75 of 100") && ranked.indexOf("75 of 100") < ranked.indexOf("3 of 4"))
    assertEquals(naive.head, "one of one")
  }

  test("the law of large numbers as the book shows it: the spread of a mean of n draws falls as 1/sqrt(n)") {
    val rng = Random(4)
    val rate = 4.5
    for n <- Seq(10, 100, 1000) do
      val means = Vector.fill(2000)(Vector.fill(n)(Poisson(rate).sample(rng)).sum.toDouble / n)
      val spread = Summary.sd(means)
      report(f"mean of $n Poisson(4.5) draws: sd over 2000 repeats $spread%.4f, theory ${math.sqrt(rate / n)}%.4f")
      assert(math.abs(spread / math.sqrt(rate / n) - 1) < 0.1)
  }
