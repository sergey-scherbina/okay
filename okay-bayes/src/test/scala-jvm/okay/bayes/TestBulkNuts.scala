package okay.bayes

import scala.util.Random
import okay.{Bulk, Chunks}
import okay.testkit.Munit.Diagnosed

/** specs/okay-bayes.md stage 6: AD NUTS on 20 000 rows of a Bulk — one aggregate per gradient */
class TestBulkNuts extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  test("Normal mean and sd from 20 000 rows by AD NUTS over Bulk[Chunks], against the large-n posterior") {
    given bulk: Bulk[Chunks] = Bulk.local(_ => Iterator.empty)
    val data = { val rng = Random(22); Vector.fill(20000)(3 + 2 * rng.nextGaussian()) }
    val rows = bulk.cache(bulk.of(data))
    val model = for
      mu <- Smooth.param("mu", Smooth.Normal(0, 100))
      sigma <- Smooth.param("sigma", Smooth.HalfNormal(10))
      _ <- Smooth.observeBulk(rows, Vector(mu, sigma))((p, y) => Smooth.Normal(p(0), p(1)).logPdf(y))
    yield (mu.value, sigma.value)
    val t0 = System.nanoTime()
    val post = Smooth.nuts(model, samples = 1000, burn = 500)
    val secs = (System.nanoTime() - t0) / 1e9
    val (m, s, n) = (data.sum / data.length, Summary.sd(data), data.length.toDouble)
    val (mus, sigmas) = (post.draws.map(_._1), post.draws.map(_._2))
    val line = f"AD NUTS over a Bulk of ${data.length} rows: μ ${Summary.mean(mus)}%.4f ± ${Summary.sd(mus)}%.4f (large-n $m%.4f ± ${s / math.sqrt(n)}%.4f), σ ${Summary.mean(sigmas)}%.4f ± ${Summary.sd(sigmas)}%.4f (large-n $s%.4f ± ${s / math.sqrt(2 * n)}%.4f), ${secs}%.1f s"
    note(line); println(s"  okay-bayes | $line")
    assert(math.abs(Summary.mean(mus) - m) < 4 * (s / math.sqrt(n)) / math.sqrt(Summary.ess(mus)) + 0.05 * s / math.sqrt(n))
    assert(math.abs(Summary.sd(mus) / (s / math.sqrt(n)) - 1) < 0.15)
    assert(math.abs(Summary.mean(sigmas) - s) < 4 * (s / math.sqrt(2 * n)) / math.sqrt(Summary.ess(sigmas)) + 0.05 * s / math.sqrt(2 * n))
    assert(math.abs(Summary.sd(sigmas) / (s / math.sqrt(2 * n)) - 1) < 0.15)
  }
