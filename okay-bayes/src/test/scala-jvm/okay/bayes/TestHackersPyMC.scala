package okay.bayes



import okay.freer.given



import okay.std.given
import okay.codec.Schema
import okay.foreign.{Py, PyEnv, PyEval}
import okay.testkit.Munit.Diagnosed

/** what PyMC answers for the book's texting model */
final case class PyMCTexting(lambda1: Double, lambda2: Double, p44: Double, p45: Double, version: String) derives Schema

/** what PyMC answers for the book's Challenger regression */
final case class PyMCChallenger(alpha: Double, beta: Double, p31: Double) derives Schema

/** PyMC ITSELF as the oracle (Live: a uv-provisioned PyMC, okay-py): the book's ch.1 model as the book writes
 * it, sampled by PyMC's own defaults (NUTS for the rates, Metropolis for τ), against the exact posterior and
 * okay-bayes's Metropolis–Hastings. Python is a test oracle here, never a dependency of okay-bayes. */
class TestHackersPyMC extends Diagnosed:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(30, "min")
  private def uv: Boolean = sys.env.getOrElse("PATH", "").split(":").exists(d => java.nio.file.Files.isExecutable(java.nio.file.Path.of(d, "uv")))
  override def munitIgnore: Boolean = !uv

  val pymc = Py.module("hackers", """
    import os
    # pytensor's C backend passes `-ld64` to the linker, which a current macOS clang reads as "library d64";
    # the pure-Python backend is slower and enough for a 74-point model
    os.environ.setdefault("PYTENSOR_FLAGS", "cxx=")
    import numpy as np, pymc as pm

    def texting(counts):
        data = np.asarray(counts, dtype=float)
        n = len(data)
        with pm.Model():
            alpha = 1.0 / data.mean()
            l1 = pm.Exponential("lambda_1", alpha)
            l2 = pm.Exponential("lambda_2", alpha)
            tau = pm.DiscreteUniform("tau", lower=0, upper=n - 1)
            idx = np.arange(n)
            lam = pm.math.switch(tau > idx, l1, l2)
            pm.Poisson("obs", lam, observed=data)
            trace = pm.sample(5000, tune=2000, chains=2, cores=1, random_seed=42, progressbar=False, compute_convergence_checks=False)
        post = trace.posterior
        taus = post["tau"].values.ravel()
        return {"lambda1": float(post["lambda_1"].values.mean()), "lambda2": float(post["lambda_2"].values.mean()),
                "p44": float((taus == 44).mean()), "p45": float((taus == 45).mean()), "version": pm.__version__}

    def challenger(temps, damage):
        t = np.asarray(temps, dtype=float)
        d = np.asarray(damage, dtype=int)
        # The book's model, written as it is NUTS diverges on every draw here
        # (PyMC 6.3.2: alpha -8.3, beta -0.26, 5000 of 5000 divergent) though
        # its own logp and gradient are right. Not the geometry: PyMC's NUTS
        # on okay's density for the same raw-temperature model has no
        # divergence (TestPyMCSampler). The same posterior through a linear
        # change of variables (standardised temperature, constant Jacobian,
        # the book's priors kept as a Potential) samples with no divergence.
        m0, s0 = t.mean(), t.std()
        sd = 1.0 / np.sqrt(0.001)
        with pm.Model():
            zb = pm.Flat("zb")
            za = pm.Flat("za")
            beta = pm.Deterministic("beta", zb / s0)
            alpha = pm.Deterministic("alpha", za - zb * m0 / s0)
            pm.Potential("prior", pm.logp(pm.Normal.dist(0, sd), beta) + pm.logp(pm.Normal.dist(0, sd), alpha))
            pm.Bernoulli("obs", logit_p=-(beta * t + alpha), observed=d)
            trace = pm.sample(5000, tune=2000, chains=2, cores=1, random_seed=42, progressbar=False, compute_convergence_checks=False)
        a = trace.posterior["alpha"].values.ravel()
        b = trace.posterior["beta"].values.ravel()
        return {"alpha": float(a.mean()), "beta": float(b.mean()), "p31": float((1.0 / (1.0 + np.exp(b * 31 + a))).mean())}
  """)

  test("PyMC on the book's model agrees with the exact posterior — and so does okay-bayes, without Python") {
    val env = PyEnv(python = "3.12", packages = Map("pymc" -> ""))
    val w = env.start(modules = Seq(pymc))
    try
      given okay.Answers[PyEval] = w.handler
      val got = pymc.fn[PyMCTexting]("texting")(Ch1.counts).runWith
      assert(got.isRight, got.toString)
      val Right(py) = got: @unchecked
      val (pTau, m1, m2) = Ch1.exact
      println(f"  okay-bayes | PyMC ${py.version}: E[λ1] = ${py.lambda1}%.3f, E[λ2] = ${py.lambda2}%.3f, P(τ = 44) = ${py.p44}%.3f, P(τ = 45) = ${py.p45}%.3f")
      assert(math.abs(py.lambda1 - m1) < 0.15 && math.abs(py.lambda2 - m2) < 0.2, s"PyMC $py vs exact $m1, $m2")
      assert(math.abs(py.p44 + py.p45 - (pTau(44) + pTau(45))) < 0.05, s"PyMC $py vs exact τ")
    finally w.close()
  }

  test("PyMC (NUTS) on the book's Challenger regression agrees with the grid") {
    val env = PyEnv(python = "3.12", packages = Map("pymc" -> ""))
    val w = env.start(modules = Seq(pymc))
    try
      given okay.Answers[PyEval] = w.handler
      val got = pymc.fn[PyMCChallenger]("challenger")(Ch2.flights.map(_._1), Ch2.flights.map(f => if f._2 then 1 else 0)).runWith
      assert(got.isRight, got.toString)
      val Right(py) = got: @unchecked
      val (ea, eb, ep, sdb, _) = Ch2.grid
      println(f"  okay-bayes | PyMC Challenger: E[α] = ${py.alpha}%.3f, E[β] = ${py.beta}%.4f, E[p(31°F)] = ${py.p31}%.4f (grid $ea%.3f, $eb%.4f, $ep%.4f)")
      assert(math.abs(py.beta - eb) < 0.25 * sdb, s"PyMC $py vs grid $eb")
      assert(math.abs(py.p31 - ep) < 0.01, s"PyMC $py vs grid $ep")
    finally w.close()
  }

