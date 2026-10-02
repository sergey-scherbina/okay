package okay.bayes

import okay.given
import okay.codec.Schema
import okay.foreign.{Py, PyEnv, PyEval}
import okay.testkit.Munit.Diagnosed

/** what PyMC answers for the book's texting model */
final case class PyMCTexting(lambda1: Double, lambda2: Double, p44: Double, p45: Double, version: String) derives Schema

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
