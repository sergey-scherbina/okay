package okay.bayes

import scala.util.Random
import okay.!
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 2b: SMC against the Kalman filter and closed-form evidence */
class TestSmc extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // a Gaussian random walk observed with noise: x(t) ~ N(x(t-1), q), y(t) ~ N(x(t), r), x(-1) = 0
  val (q, r) = (1.0, 1.0)
  val ys: Vector[Double] =
    val rng = Random(7)
    Vector.iterate(q * rng.nextGaussian(), 50)(_ + q * rng.nextGaussian()).map(_ + r * rng.nextGaussian())

  def track(ys: Vector[Double]): Double ! Model =
    def step(t: Int, x: Double): Double ! Model =
      if t == ys.length then okay.pure[Model, Double](x)
      else sample(s"x$t", Normal(x, q)).flatMap(x1 => observe(Normal(x1, r), ys(t)).flatMap(_ => step(t + 1, x1)))
    step(0, 0.0)

  /** the exact answers: the filtering mean and variance of the last state, and log p(y) */
  def kalman(ys: Vector[Double]): (Double, Double, Double) =
    var (m, v, logZ) = (0.0, 0.0, 0.0)
    for y <- ys do
      v += q * q
      logZ += Normal(m, math.sqrt(v + r * r)).logPdf(y)
      val k = v / (v + r * r)
      m += k * (y - m)
      v *= 1 - k
    (m, v, logZ)

  test("a state-space model step by step: the filtering mean and the log evidence are the Kalman filter's") {
    val (m, v, logZ) = kalman(ys)
    val ps = smc(track(ys), particles = 4000)
    val got = ps.expect(identity)
    report(f"Kalman, 50 steps: E[x49 | y] ${got}%.4f (exact $m%.4f, sd ${math.sqrt(v)}%.4f), log p(y) ${ps.logEvidence}%.3f (exact $logZ%.3f), ${ps.resamplings} resamplings")
    assert(math.abs(got - m) < 0.1 * math.sqrt(v), s"filtering mean $got vs $m")
    assert(math.abs(ps.logEvidence - logZ) < 0.3, s"log evidence ${ps.logEvidence} vs $logZ")
    assert(math.abs(ps.mean("x49") - got) < 1e-9, "the site's mean is the program's value's")
  }

  val flips: Vector[Boolean] = Vector.tabulate(40)(i => i % 3 != 0)

  test("a static model observed one point at a time: Beta–Bernoulli posterior mean and log evidence in closed form") {
    val (a, b) = (2.0, 2.0)
    val k = flips.count(identity)
    val model = for
      p <- sample("p", Beta(a, b))
      _ <- observeEach(flips)(_ => Bernoulli(p), identity)
    yield p
    val ps = smc(model, particles = 4000)
    val exact = logBeta(a + k, b + flips.length - k) - logBeta(a, b)
    val mean = (a + k) / (a + b + flips.length)
    report(f"Beta–Bernoulli: E[p] ${ps.expect(identity)}%.4f (exact $mean%.4f), log p(y) ${ps.logEvidence}%.4f (exact $exact%.4f)")
    assert(math.abs(ps.expect(identity) - mean) < 0.01)
    assert(math.abs(ps.logEvidence - exact) < 0.1, s"log evidence ${ps.logEvidence} vs $exact")
  }

  test("the evidence compares models: a fair coin against p ~ Uniform, the log Bayes factor in closed form") {
    val fair = observeEach(flips)(_ => Bernoulli(0.5), identity)
    val free = for
      p <- sample("p", Uniform(0, 1))
      _ <- observeEach(flips)(_ => Bernoulli(p), identity)
    yield ()
    val k = flips.count(identity)
    val exact = logBeta(1.0 + k, 1.0 + flips.length - k) - flips.length * math.log(0.5)
    val got = smc(free, particles = 4000).logEvidence - smc(fair, particles = 100).logEvidence
    report(f"log Bayes factor, p ~ Uniform against a fair coin, $k heads of ${flips.length}: $got%.4f (exact $exact%.4f)")
    assert(math.abs(smc(fair, particles = 100).logEvidence - flips.length * math.log(0.5)) < 1e-9, "a model with no draw is weighed exactly")
    assert(math.abs(got - exact) < 0.1, s"log Bayes factor $got vs $exact")
  }

  test("a resampled particle shares its continuation with its copies, and the copies draw their own futures") {
    val model = for
      x <- sample("x", Normal(0, 1))
      _ <- factor(-50 * x * x)
      y <- sample("y", Normal(0, 1))
    yield (x, y)
    val ps = smc(model, particles = 1000)
    val copies = ps.values.groupBy(_._1).valuesIterator.filter(_.length > 1).toVector
    report(s"after one resampling: ${copies.length} values of x held by several particles, the largest by ${copies.map(_.length).max}")
    assertEquals(ps.resamplings, 1)
    assert(copies.nonEmpty, "the weights were uneven enough to duplicate")
    assert(copies.forall(c => c.map(_._2).distinct.length == c.length), "every copy drew its own y")
  }
