package okay.bayes

import okay.testkit.Munit.Diagnosed
import Smooth.{param, observeAll}

/**
 * specs/okay-bayes.md stage 4, Live: PyMC's NUTS behind the import samples
 * OUR target — the call sites below are the ones ours runs, plus one import.
 */
class TestPyMCSampler extends Diagnosed:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("PyMC behind the import samples our target: a conjugate in closed form, Challenger against the grid") {
    import PyMC.given
    assertEquals(summon[Sampler].name, "pymc")
    val flips = Vector.tabulate(30)(i => i < 21)
    val bb = for
      q <- param("q", Smooth.Beta(2, 2))
      _ <- observeAll(flips)(_ => Smooth.Bernoulli(q), identity)
    yield q.value
    val post = Smooth.nuts(bb, samples = 2000, burn = 1000)
    val (m, s, ess) = (Summary.mean(post.draws), Summary.sd(post.draws), Summary.ess(post.draws))
    val (em, es) = (23.0 / 34, math.sqrt(23.0 * 11 / (34 * 34 * 35)))
    report(f"PyMC on our Beta–Bernoulli target: mean $m%.4f (exact $em%.4f), sd $s%.4f (exact $es%.4f), ESS $ess%.0f, divergent ${post.acceptance("(divergent)")}%.4f")
    assert(math.abs(m - em) < 4 * es / math.sqrt(ess))
    assert(math.abs(s - es) < 0.1 * es)

    val sd = Ch2.sd
    val challenger = for
      beta <- param("beta", Smooth.Normal(0, sd))
      alpha <- param("alpha", Smooth.Normal(0, sd))
      _ <- observeAll(Ch2.flights)(f => Smooth.BernoulliLogit(-(beta * f._1 + alpha)), _._2)
    yield (alpha.value, beta.value, Ch2.p(31, alpha.value, beta.value))
    val ch = Smooth.nuts(challenger, samples = 2000, burn = 1000)
    val (_, eb, ep, sdb, _) = Ch2.grid
    val bs = ch.site("beta")
    val essB = Summary.ess(bs)
    val p31 = Summary.mean(ch.draws.map(_._3))
    report(f"PyMC on our Challenger target: E[β] ${Summary.mean(bs)}%.4f (grid $eb%.4f), sd(β) ${Summary.sd(bs)}%.4f (grid $sdb%.4f), p31 $p31%.4f (grid $ep%.4f), ESS(β) $essB%.0f, divergent ${ch.acceptance("(divergent)")}%.4f, tree depth ${ch.acceptance("(tree depth)")}%.2f")
    assert(math.abs(Summary.mean(bs) - eb) < 4 * sdb / math.sqrt(essB))
    assert(math.abs(p31 - ep) < 0.01)
  }
