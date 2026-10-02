package okay.bayes

import scala.util.Random
import okay.{Free, given}
import okay.codec.Schema
import okay.foreign.{ForeignWorker, Py, PyEnv, PyEval}

/**
 * PyMC'S NUTS on OUR target (specs/okay-bayes.md stage 4): the standard
 * sampler behind an import — `import okay.bayes.PyMC.given` and every
 * `Bayes.nuts` / `Smooth.nuts` in scope samples with PyMC instead of ours,
 * the call sites unchanged. PyMC cannot read a Scala program, so it is
 * handed the program's `Target`: a `pm.Potential` over a flat ℝᵈ whose log
 * density and gradient are a black-box pytensor Op calling back into okay
 * (`okay.call("logp")`, `okay.call("grad")`) — PyMC's documented
 * black-box likelihood. Two round trips per leapfrog step: this is for
 * checking ours against the reference, not for speed.
 *
 * okay-py is an OPTIONAL dependency of okay-bayes: without it on the
 * classpath the first use is refused by name ([[PyMC.missing]]). The
 * worker — a `uv`-provisioned Python 3.12 with PyMC — starts on first use
 * and stops with the JVM; `PyMC.on(worker)` uses one you started.
 */
object PyMC extends Sampler:
  val name = "pymc"
  given pymc: Sampler = this

  /** why this cannot run here, or None: okay-py missing from the classpath */
  def missing: Option[String] =
    try { Class.forName("okay.foreign.PyEnv", false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      "okay.bayes.PyMC needs okay-py, an optional dependency of okay-bayes (okay.foreign.PyEnv is not on the classpath): " +
        "add io.github.sergey-scherbina::okay-py — or use okay.bayes.Sampler.Okay, the default, which needs none of it")

  private lazy val worker: ForeignWorker =
    missing.foreach(why => throw IllegalStateException(why))
    val w = PyEnv(python = "3.12", packages = Map("pymc" -> "")).start(modules = Seq(module))
    Runtime.getRuntime.addShutdownHook(Thread(() => w.close()))
    w

  def run(target: Target, samples: Int, burn: Int, seed: Long, delta: Double): NutsChain =
    on(worker).run(target, samples, burn, seed, delta)

  /** PyMC on a worker the caller started (with `module` among its modules) and owns */
  def on(w: ForeignWorker): Sampler = new Sampler:
    val name = "pymc"
    def run(target: Target, samples: Int, burn: Int, seed: Long, delta: Double): NutsChain =
      given okay.Answers[PyEval] = w.handler
      // the wire carries finite numbers: a density of zero goes as a log weight no step accepts
      def finite(x: Double): Double = if x.isNaN || x == Double.NegativeInfinity then -1e300 else x
      val logp = Py.callback[Vector[Double], Double]("logp")(u => Free.pure(finite(target.logp(u.toArray))))
      val grad = Py.callback[Vector[Double], Vector[Double]]("grad")(u =>
        Free.pure(target.gradient(u.toArray)._2.toVector.map(g => if g.isNaN || g.isInfinite then 0.0 else g)))
      // a start with a finite density, chosen here as ours chooses it
      val rng = new Random(seed)
      var init = target.init(rng)
      var tries = 1
      while target.logp(init) == Double.NegativeInfinity && tries < 1000 do { init = target.init(rng); tries += 1 }
      val out = module.fn[Run]("sample").calling(Py.callbacks(logp, grad))(init.toVector, Vector(samples.toDouble, burn.toDouble, seed.toDouble, delta)).runWith
      out match
        case Right(r) => NutsChain(r.draws, r.step, Vector.empty, r.divergences.toInt, r.acceptance, r.depth)
        case Left(c) => throw IllegalStateException(s"PyMC: $c")

  final case class Run(draws: Vector[Vector[Double]], divergences: Long, acceptance: Double, depth: Double, step: Double) derives Schema

  /** the Python side: a flat ℝᵈ, a Potential through the black-box Op, PyMC's NUTS */
  val module = Py.module("okay_bayes_pymc", """
    import os
    os.environ.setdefault("PYTENSOR_FLAGS", "cxx=")   # pytensor's C backend passes -ld64, which a current macOS linker refuses
    import okay
    import numpy as np, pymc as pm, pytensor.tensor as pt
    from pytensor.graph.op import Op

    class _Grad(Op):
        itypes = [pt.dvector]
        otypes = [pt.dvector]
        def perform(self, node, inputs, outputs):
            outputs[0][0] = np.asarray(okay.call("grad", [float(x) for x in inputs[0]]), dtype="float64")

    class _Logp(Op):
        itypes = [pt.dvector]
        otypes = [pt.dscalar]
        def perform(self, node, inputs, outputs):
            outputs[0][0] = np.asarray(okay.call("logp", [float(x) for x in inputs[0]]), dtype="float64")
        def grad(self, inputs, g):
            return [g[0] * _Grad()(inputs[0])]

    def sample(init, config):
        samples, burn, seed, delta = int(config[0]), int(config[1]), int(config[2]), float(config[3])
        with pm.Model():
            u = pm.Flat("u", shape=len(init), initval=np.asarray(init, dtype="float64"))
            pm.Potential("target", _Logp()(u))
            tr = pm.sample(samples, tune=burn, chains=1, cores=1, random_seed=int(seed), progressbar=False,
                           compute_convergence_checks=False, target_accept=float(delta))
        st = tr.sample_stats
        return {"draws": tr.posterior["u"].values[0].tolist(),
                "divergences": int(st["diverging"].values.sum()),
                "acceptance": float(st["acceptance_rate"].values.mean()),
                "depth": float(st["tree_depth"].values.mean()),
                "step": float(st["step_size"].values.mean())}
  """)

/** a sampler by NAME, for a config value or a flag */
object Samplers:
  val names: Vector[String] = Vector("okay", "pymc")
  def byName(name: String): Either[String, Sampler] = name match
    case "okay" => Right(Sampler.Okay)
    case "pymc" => PyMC.missing.toLeft(PyMC)
    case other => Left(s"no sampler '$other': the choices are ${names.mkString(", ")}")
