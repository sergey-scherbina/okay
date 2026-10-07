package okay.bayes

import scala.util.Random
import okay.{Bulk, Chunks, through}
import okay.freer.{%}
import okay.freer.{!}
import okay.std.{Writer}
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 6: the online filter as a Stage, and the likelihood over a Bulk */
class TestStreams extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // the random walk of TestSmc: x(t) ~ N(x(t-1), q), y(t) ~ N(x(t), r), x(-1) = 0
  val (q, r) = (1.0, 1.0)
  val ys: Vector[Double] =
    val rng = Random(7)
    Vector.iterate(q * rng.nextGaussian(), 50)(_ + q * rng.nextGaussian()).map(_ + r * rng.nextGaussian())

  /** the Kalman filter's mean and sd after each observation, and the log evidence */
  def kalman: (Vector[(Double, Double)], Double) =
    var (m, v, logZ) = (0.0, 0.0, 0.0)
    val steps = ys.map { y =>
      v += q * q
      logZ += Normal(m, math.sqrt(v + r * r)).logPdf(y)
      val k = v / (v + r * r)
      m += k * (y - m)
      v *= 1 - k
      (m, math.sqrt(v))
    }
    (steps, logZ)

  test("an online filter over a stream: the filtering mean at every step, and the log evidence, are the Kalman filter's") {
    val f = Online.filter[Double, Double](okay.freer.pure[Model, Double](0.0),
      (x, y) => sample("x", Normal(x, q)).flatMap(x1 => observe(Normal(x1, r), y).map(_ => x1)), particles = 4000)
    val observations: Unit ! Writer % Double = ys.foldLeft(okay.freer.pure[Writer % Double, Unit](()))((p, y) => p.flatMap(_ => Writer.tell(y)))
    val posteriors = okay.freer.!.run(Writer.run(through(observations)(f.stage)))._1.toVector
    val (exact, logZ) = kalman
    assertEquals(posteriors.length, ys.length)
    val worst = posteriors.indices.map(t => math.abs(posteriors(t).expect(identity) - exact(t)._1) / exact(t)._2).max
    report(f"online filter, 50 observations through a Stage: worst filtering mean ${worst}%.3f Kalman sds off; log p(y) ${posteriors.last.logEvidence}%.3f (Kalman $logZ%.3f)")
    assert(worst < 0.15, s"a step's filtering mean is $worst sds from the Kalman filter's")
    assert(math.abs(posteriors.last.logEvidence - logZ) < 0.3)
  }

  given Bulk[Chunks] = Bulk.local(_ => Iterator.empty)

  val data: Vector[Double] = { val rng = Random(21); Vector.fill(2000)(3 + 2 * rng.nextGaussian()) }

  test("the same model over a Bulk and over a Vector: the same log density and gradient") {
    val rows = summon[Bulk[Chunks]].of(data)
    def model(bulk: Boolean) = for
      mu <- Smooth.param("mu", Smooth.Normal(0, 100))
      sigma <- Smooth.param("sigma", Smooth.HalfNormal(10))
      _ <- if bulk then Smooth.observeBulk(rows, Vector(mu, sigma))((p, y) => Smooth.Normal(p(0), p(1)).logPdf(y))
           else Smooth.observeAll(data)(_ => Smooth.Normal(mu, sigma), y => Real.const(y))
    yield mu.value
    val (b, v) = (Smooth.target(model(true)), Smooth.target(model(false)))
    val rng = Random(3)
    for _ <- 1 to 10 do
      val u = Array(rng.nextGaussian() * 3 + 3, rng.nextGaussian())
      val ((lb, gb), (lv, gv)) = (b.gradient(u), v.gradient(u))
      assertEqualsDouble(lb, lv, 1e-9 * math.abs(lv))
      for i <- 0 until 2 do assertEqualsDouble(gb(i), gv(i), 1e-9 * math.max(1, math.abs(gv(i))))
  }

  test("Bayes.observeBulk: the Model form over a Bulk, by adaptive, against the large-n posterior") {
    val rows = summon[Bulk[Chunks]].of(data)
    val model = for
      mu <- sample("mu", Normal(0, 100))
      sigma <- sample("sigma", Uniform(0, 50))
      _ <- observeBulk(rows)(y => Normal(mu, sigma).logPdf(y))
    yield mu
    val post = adaptive(model, samples = 2000, burn = 1000)
    val (m, s) = (data.sum / data.length, Summary.sd(data))
    val got = Summary.mean(post.draws)
    report(f"observeBulk by adaptive, ${data.length} rows: μ $got%.4f (large-n ${m}%.4f ± ${s / math.sqrt(data.length)}%.4f)")
    assert(math.abs(got - m) < 4 * (s / math.sqrt(data.length)) / math.sqrt(Summary.ess(post.draws)) + 0.1 * s / math.sqrt(data.length))
  }
