package okay.bayes

import okay.testkit.Munit.Diagnosed

/** specs/okay-bayes.md stage 7a on the book's models: a composed kernel on ch.1, a mixture kernel on ch.2 */
class TestKernelsHackers extends Diagnosed:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("the texting model by NUTS on the rates and a random walk on the switch point, against the exact posterior") {
    val post = Bayes.sample(Ch1.texting, Kernel.nuts("lambda_1", "lambda_2") >>> Kernel.site("tau"), samples = 4000, burn = 1500, chains = 2)
    val (pTau, m1, m2) = Ch1.exact
    val (l1, l2, taus) = (post.site("lambda_1"), post.site("lambda_2"), post.site("tau"))
    val (p44, p45) = (taus.count(_ == 44).toDouble / taus.length, taus.count(_ == 45).toDouble / taus.length)
    report(f"ch.1 by nuts(λ1, λ2) >>> site(τ): E[λ1] ${Summary.mean(l1)}%.3f (exact $m1%.3f), E[λ2] ${Summary.mean(l2)}%.3f (exact $m2%.3f), P(τ ∈ {44, 45}) ${p44 + p45}%.3f (exact ${pTau(44) + pTau(45)}%.3f); ESS(λ1) ${Summary.ess(l1)}%.0f")
    assert(math.abs(Summary.mean(l1) - m1) < 0.15, s"E[λ1] ${Summary.mean(l1)} vs exact $m1")
    assert(math.abs(Summary.mean(l2) - m2) < 0.2, s"E[λ2] ${Summary.mean(l2)} vs exact $m2")
    assert(math.abs(p44 + p45 - (pTau(44) + pTau(45))) < 0.04)
  }

  test("Challenger by a mixture of single-site moves and a joint NUTS block, against the grid") {
    val k = Kernel.mixture(1.0 -> Kernel.everySite, 1.0 -> Kernel.nuts("alpha", "beta"))
    val post = Bayes.sample(Ch2.challenger, k, samples = 4000, burn = 2000, chains = 2)
    val (_, eb, ep, sdb, _) = Ch2.grid
    val bs = post.site("beta")
    val ess = Summary.ess(bs)
    val p31 = Summary.mean(post.draws.map(_._3))
    report(f"Challenger by mixture(everySite, nuts(α, β)): E[β] ${Summary.mean(bs)}%.4f (grid $eb%.4f), sd(β) ${Summary.sd(bs)}%.4f (grid $sdb%.4f), p31 $p31%.4f (grid $ep%.4f), ESS(β) $ess%.0f of ${bs.length}")
    assert(math.abs(Summary.mean(bs) - eb) < 4 * sdb / math.sqrt(ess))
    assert(math.abs(Summary.sd(bs) - sdb) < 0.1 * sdb)
    assert(math.abs(p31 - ep) < 0.005)
  }
