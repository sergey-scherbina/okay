package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Bayes.*

/** specs/okay-bayes.md stage 3a: NUTS on the book's ch.2 and ch.3 models, against the exact grid and the importance sampler */
class TestHackersNuts extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("Challenger: NUTS against the exact grid, divergences counted") {
    val post = nuts(Ch2.challenger, samples = 4000, burn = 2000, chains = 2)
    val (ea, eb, ep, sdb, _) = Ch2.grid
    val b = post.site("beta")
    val ess = Summary.ess(b)
    val p31 = Summary.mean(post.draws.map(_._3))
    report(f"NUTS Challenger: E[α] ${Summary.mean(post.site("alpha"))}%.3f (grid $ea%.3f), E[β] ${Summary.mean(b)}%.4f (grid $eb%.4f), sd(β) ${Summary.sd(b)}%.4f (grid $sdb%.4f), p31 $p31%.4f (grid $ep%.4f); ESS(β) $ess%.0f of ${b.length}, R-hat ${post.rhat("beta")}%.4f, divergent ${post.acceptance("(divergent)")}%.4f, tree depth ${post.acceptance("(tree depth)")}%.2f")
    assert(math.abs(Summary.mean(b) - eb) < 4 * sdb / math.sqrt(ess), s"E[β] ${Summary.mean(b)} vs $eb at ESS $ess")
    assert(math.abs(Summary.sd(b) - sdb) < 0.1 * sdb)
    assert(math.abs(p31 - ep) < 0.005)
    assert(post.acceptance("(divergent)") < 0.01, "a handful of divergences at most")
  }

  test("the ch.3 mixture: NUTS against importance sampling") {
    val names = Vector("p", "center0", "center1", "sd0", "sd1")
    val post = nuts(Ch3.mixture, samples = 1000, burn = 500, chains = 2)
    val mean = names.map(n => Summary.mean(post.site(n)))
    val xs = post.draws
    val cov = Vector.tabulate(5, 5)((a, b) => xs.iterator.map(x => (x(a) - mean(a)) * (x(b) - mean(b))).sum / (xs.length - 1))
    val (oracle, oess) = Ch3.importance(mean, cov.map(_.map(_ * 4)), 200000, Random(11))
    for i <- names.indices do
      val exact = oracle.iterator.filter(_._2 > 0).map((t, w) => t(i) * w).sum
      val s = post.site(names(i))
      val ess = Summary.ess(s)
      report(f"NUTS ch.3 ${names(i)}%-7s: ${mean(i)}%.3f ± ${Summary.sd(s)}%.3f (ESS $ess%.0f of ${s.length}), importance sampling $exact%.3f (ESS $oess%.0f)")
      assert(math.abs(mean(i) - exact) < 4 * Summary.sd(s) * math.sqrt(1 / ess + 1 / oess), s"${names(i)}: ${mean(i)} vs $exact")
    report(f"NUTS ch.3: divergent ${post.acceptance("(divergent)")}%.4f, tree depth ${post.acceptance("(tree depth)")}%.2f")
  }

  test("Challenger written over Grad: AD NUTS against the grid, its log density the ordinary model's") {
    import Smooth.{param, observeAll}
    val sd = Ch2.sd
    val challenger = for
      beta <- param("beta", Smooth.Normal(0, sd))
      alpha <- param("alpha", Smooth.Normal(0, sd))
      _ <- observeAll(Ch2.flights)(f => Smooth.BernoulliLogit(-(beta * f._1 + alpha)), _._2)
    yield (alpha.value, beta.value, Ch2.p(31, alpha.value, beta.value))
    // the same density as Ch2.challenger, written with Distribution, at the grid's mean and away from it
    val t = Smooth.target(challenger)
    for (b, a) <- Seq((0.2693, -17.511), (-0.1, 3.0), (1.0, -60.0)) do
      val plain = Distribution.Normal(0, sd).logPdf(b) + Distribution.Normal(0, sd).logPdf(a) +
        Ch2.flights.map((tf, d) => Distribution.Bernoulli(Ch2.p(tf, a, b)).logPdf(d)).sum
      assertEqualsDouble(t.logp(Array(b, a)), plain, 1e-9 * math.abs(plain))
    val post = Smooth.nuts(challenger, samples = 4000, burn = 2000, chains = 2)
    val (_, eb, ep, sdb, _) = Ch2.grid
    val bs = post.site("beta")
    val ess = Summary.ess(bs)
    val p31 = Summary.mean(post.draws.map(_._3))
    report(f"AD NUTS Challenger: E[β] ${Summary.mean(bs)}%.4f (grid $eb%.4f), sd(β) ${Summary.sd(bs)}%.4f (grid $sdb%.4f), p31 $p31%.4f (grid $ep%.4f); ESS(β) $ess%.0f of ${bs.length}, divergent ${post.acceptance("(divergent)")}%.4f")
    assert(math.abs(Summary.mean(bs) - eb) < 4 * sdb / math.sqrt(ess))
    assert(math.abs(Summary.sd(bs) - sdb) < 0.1 * sdb)
    assert(math.abs(p31 - ep) < 0.005)
  }
