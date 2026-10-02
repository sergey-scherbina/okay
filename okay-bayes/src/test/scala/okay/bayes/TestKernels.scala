package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 7a: kernels leave the posterior invariant, alone and combined */
class TestKernels extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // two independent conjugates, so the posterior is a product of closed forms: a Normal mean, a Poisson rate
  val ys = Vector(1.2, 2.3, 0.7, 1.9, 1.4)
  val counts = Vector(3, 5, 2, 4, 6, 1, 4)
  val model = for
    m <- sample("m", Normal(0, 2))
    l <- sample("l", Gamma(2, 1))
    _ <- observeAll(ys)(_ => Normal(m, 1), identity)
    _ <- observeAll(counts)(_ => Poisson(l), identity)
  yield (m, l)
  val (mMean, mSd) = { val prec = 0.25 + ys.length; (ys.sum / prec, math.sqrt(1 / prec)) }
  val (lShape, lRate) = (2.0 + counts.sum, 1.0 + counts.length)
  val (lMean, lSd) = (lShape / lRate, math.sqrt(lShape) / lRate)

  test("invariance: from exact posterior draws, one step of each kernel and each combination leaves the moments unchanged") {
    val n = 20000
    val rng = Random(17)
    val exact = Vector.fill(n)((mMean + mSd * rng.nextGaussian(), Gamma(lShape, lRate).sample(rng)))
    val kernels = Vector[(String, () => Kernel)](
      "site(m) >>> site(l)" -> (() => Kernel.sites("m", "l")),
      "everySite" -> (() => Kernel.everySite),
      "nuts(m, l)" -> (() => Kernel.nuts("m", "l")),
      "nuts(m) >>> site(l)" -> (() => Kernel.nuts("m") >>> Kernel.site("l")),
      "mixture(1 site(m), 2 nuts(l))" -> (() => Kernel.mixture(1.0 -> Kernel.site("m"), 2.0 -> Kernel.nuts("l"))),
      "site(l).times(3)" -> (() => Kernel.site("l").times(3)))
    for (name, make) <- kernels do
      val k = make()
      val moved = exact.map((m, l) => k.step(Trace(model, Bayes.pass(model, Map("m" -> m, "l" -> l), rng)), false, rng).sites)
      val (ms, ls) = (moved.map(_("m")), moved.map(_("l")))
      val share = moved.indices.count(i => moved(i)("m") != exact(i)._1 || moved(i)("l") != exact(i)._2).toDouble / n
      report(f"invariance, $name%-30s: m ${Summary.mean(ms)}%.4f ± ${Summary.sd(ms)}%.4f (exact $mMean%.4f ± $mSd%.4f), l ${Summary.mean(ls)}%.4f ± ${Summary.sd(ls)}%.4f (exact $lMean%.4f ± $lSd%.4f), moved ${share * 100}%.0f%%")
      assert(share > 0.2, s"$name barely moved: $share")
      assert(math.abs(Summary.mean(ms) - mMean) < 4 * mSd / math.sqrt(n), s"$name: E[m] ${Summary.mean(ms)}")
      assert(math.abs(Summary.mean(ls) - lMean) < 4 * lSd / math.sqrt(n), s"$name: E[l] ${Summary.mean(ls)}")
      assert(math.abs(Summary.sd(ms) / mSd - 1) < 0.03 && math.abs(Summary.sd(ls) / lSd - 1) < 0.03, s"$name: sds")
  }

  test("Bayes.sample with a composed kernel: a chain's posterior is the closed form") {
    val post = Bayes.sample(model, Kernel.nuts("m") >>> Kernel.site("l"), samples = 3000, burn = 1000, chains = 2)
    val (ms, ls) = (post.site("m"), post.site("l"))
    report(f"Bayes.sample, nuts(m) >>> site(l): m ${Summary.mean(ms)}%.4f (exact $mMean%.4f), l ${Summary.mean(ls)}%.4f (exact $lMean%.4f); acceptance ${post.acceptance}")
    assert(math.abs(Summary.mean(ms) - mMean) < 4 * mSd / math.sqrt(Summary.ess(ms)))
    assert(math.abs(Summary.mean(ls) - lMean) < 4 * lSd / math.sqrt(Summary.ess(ls)))
  }

  test("a discrete site handed to the NUTS block is refused by name") {
    val m = for
      k <- sample("k", Poisson(3))
      _ <- observe(Normal(k.toDouble, 1), 2.5)
    yield k
    val e = intercept[IllegalArgumentException](Bayes.sample(m, Kernel.nuts("k"), samples = 5, burn = 0))
    assert(e.getMessage.contains("'k' is discrete"), e.getMessage)
  }
