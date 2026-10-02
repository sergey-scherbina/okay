package okay.bayes

import scala.io.Source
import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** Bayesian Methods for Hackers, ch.3 — two clusters in 300 points, on the book's data */
object Ch3:
  val data: Vector[Double] =
    Source.fromInputStream(getClass.getResourceAsStream("/bmh/mixture_data.csv")).getLines().map(_.trim.toDouble).toVector

  /** the book's priors: p ~ Uniform(0, 1), centres ~ Normal(120, 10) and Normal(190, 10), sds ~ Uniform(0, 100) */
  def prior(p: Double, c0: Double, c1: Double, s0: Double, s1: Double): Double =
    Uniform(0, 1).logPdf(p) + Normal(120, 10).logPdf(c0) + Normal(190, 10).logPdf(c1) + Uniform(0, 100).logPdf(s0) + Uniform(0, 100).logPdf(s1)

  def clusters(p: Double, c0: Double, c1: Double, s0: Double, s1: Double): Mixture[Double] =
    Mixture(Vector(p -> Normal(c0, s0), (1 - p) -> Normal(c1, s1)))

  val mixture = for
    p <- sample("p", Uniform(0, 1))
    c0 <- sample("center0", Normal(120, 10))
    c1 <- sample("center1", Normal(190, 10))
    s0 <- sample("sd0", Uniform(0, 100))
    s1 <- sample("sd1", Uniform(0, 100))
    _ <- observeAll(data)(_ => clusters(p, c0, c1, s0, s1), identity)
  yield Vector(p, c0, c1, s0, s1)

  /** the log posterior up to a constant, written apart from the model */
  def logPost(t: Vector[Double]): Double =
    val pr = prior(t(0), t(1), t(2), t(3), t(4))
    if pr == NegInf then NegInf else pr + data.iterator.map(clusters(t(0), t(1), t(2), t(3), t(4)).logPdf).sum

  /**
   * the ORACLE: self-normalised importance sampling from a multivariate
   * Student-t (ν = 5) — unbiased in the limit whatever its centre and
   * scale, which only decide how many of the n draws count (its ESS).
   * Answers the draws with their normalised weights, and the ESS.
   */
  def importance(centre: Vector[Double], cov: Vector[Vector[Double]], n: Int, rng: Random): (Vector[(Vector[Double], Double)], Double) =
    val d = centre.length
    val nu = 5.0
    val l = Array.ofDim[Double](d, d)
    for i <- 0 until d; j <- 0 to i do
      var s = cov(i)(j)
      for k <- 0 until j do s -= l(i)(k) * l(j)(k)
      l(i)(j) = if i == j then math.sqrt(s) else s / l(j)(j)
    // log q up to a constant: −(ν + d)/2 · log(1 + |z|²/ν), z the standardised draw
    val draws = Vector.fill(n) {
      val z = Vector.fill(d)(rng.nextGaussian())
      val w = math.sqrt(nu / (2 * gamma1(nu / 2, rng)))
      val y = Vector.tabulate(d)(i => centre(i) + w * (0 to i).map(k => l(i)(k) * z(k)).sum)
      val zz = z.map(x => x * x).sum * w * w
      (y, logPost(y) + (nu + d) / 2 * math.log(1 + zz / nu))
    }
    val top = draws.map(_._2).max
    val ws = draws.map((_, lw) => math.exp(lw - top))
    val total = ws.sum
    (draws.map(_._1).zip(ws.map(_ / total)), total * total / ws.map(w => w * w).sum)

class TestHackersCh3 extends Diagnosed:
  import Ch3.*
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")
  val names = Vector("p", "center0", "center1", "sd0", "sd1")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  lazy val post = adaptive(mixture, samples = 10000, burn = 5000, chains = 4)
  lazy val mean = names.map(n => Summary.mean(post.site(n)))
  /** the oracle's weighted draws: a proposal centred on the chains and twice as wide — its quality decides the ESS, not the answer */
  lazy val (oracle, oracleEss) =
    val xs = post.draws
    val cov = Vector.tabulate(5, 5)((a, b) => xs.iterator.map(x => (x(a) - mean(a)) * (x(b) - mean(b))).sum / (xs.length - 1))
    importance(mean, cov.map(_.map(_ * 4)), 200000, Random(11))
  def exactly(f: Vector[Double] => Double): Double = oracle.iterator.filter(_._2 > 0).map((t, w) => f(t) * w).sum  // a draw outside the support weighs 0 and may not be evaluable

  test("the book's mixture: four chains agree with each other (R-hat) and with importance sampling") {
    for n <- names do assert(post.rhat(n) < 1.01, s"R-hat($n) ${post.rhat(n)}")
    val exact = names.indices.map(i => exactly(_(i)))
    val ess = oracleEss
    for i <- names.indices do
      val sd = Summary.sd(post.site(names(i)))
      val essMh = Summary.ess(post.site(names(i)))
      report(f"ch.3 ${names(i)}%-7s: adaptive ${mean(i)}%.3f ± $sd%.3f (ESS $essMh%.0f, R-hat ${post.rhat(names(i))}%.4f), importance sampling ${exact(i)}%.3f (ESS $ess%.0f)")
      // both are estimates: their difference within 4 of its own standard errors
      val err = 4 * sd * math.sqrt(1 / essMh + 1 / ess)
      assert(math.abs(mean(i) - exact(i)) < err, s"${names(i)}: ${mean(i)} vs ${exact(i)}, allowed $err")
  }

  test("the book's per-point question: P(a point at 175 belongs to the first cluster), as a posterior expectation") {
    def first(x: Double)(t: Vector[Double]): Double =
      val a = t(0) * math.exp(Normal(t(1), t(3)).logPdf(x))
      a / (a + (1 - t(0)) * math.exp(Normal(t(2), t(4)).logPdf(x)))
    for x <- Seq(150.0, 175.0, 220.0) do
      val got = Summary.mean(post.draws.map(first(x)))
      val want = exactly(first(x))
      val err = 4 * Summary.sd(post.draws.map(first(x))) * math.sqrt(1 / Summary.ess(post.draws.map(first(x))) + 1 / oracleEss)
      report(f"ch.3 P(first cluster | x = $x%.0f): adaptive $got%.4f, importance sampling $want%.4f")
      assert(math.abs(got - want) < err, s"x = $x: $got vs $want, allowed $err")
  }

  test("the book's convergence lesson: chains kept from their first draw disagree, the same chains after burn-in do not") {
    val raw = metropolis(mixture, samples = 100, burn = 0, chains = 4, seed = 5L)
    val worst = names.map(raw.rhat).max
    report(f"ch.3 R-hat with no burn-in, the first 100 draws from prior starts: worst $worst%.2f; after burn-in: worst ${names.map(post.rhat).max}%.4f")
    assert(worst > 1.1, s"chains from scattered starts should disagree early: R-hat $worst")
  }
