package okay.bayes

import scala.annotation.tailrec
import okay.{!, Effect, Free, effect}
import Real.{exp, log, lgamma, log1p, softplus, sigmoid, square, sum}

/** a density over X whose log is differentiable — in its parameters, and in x when x is a `Real` */
trait Density[X]:
  def logPdf(x: X): Real

/** a density a PARAMETER is drawn from: it says where its values lie */
trait Prior extends Density[Real]:
  def support: Support

/**
 * A MODEL FOR GRADIENT INFERENCE (specs/okay-bayes.md stage 3b): the same
 * shape as `Model`, over `Real`. A parameter is named and has a prior; a
 * score is a differentiable log weight.
 */
enum Grad[+A] derives Effect:
  case Param(name: String, prior: Prior) extends Grad[Real]
  case Score(logWeight: Real) extends Grad[Unit]

/**
 * MODELS WITH EXACT GRADIENTS: write the model over `Grad`, and
 * `Smooth.target` is a `Target` whose gradient is one run of the program
 * plus one backward sweep of its tape; `Smooth.nuts` samples it with the
 * same `Nuts.sample` an ordinary model's finite-difference target uses.
 */
object Smooth:
  private val LogSqrt2Pi = 0.5 * math.log(2 * math.Pi)

  final case class Normal(mu: Real, sigma: Real) extends Prior:
    def logPdf(x: Real): Real = -0.5 * square((x - mu) / sigma) - log(sigma) - LogSqrt2Pi
    def support: Support = Support.Real
  /** |N(0, σ)|, on the positive half-line */
  final case class HalfNormal(sigma: Real) extends Prior:
    def logPdf(x: Real): Real =
      if x.value < 0 then Real.const(Double.NegativeInfinity) else math.log(2) - 0.5 * square(x / sigma) - log(sigma) - LogSqrt2Pi
    def support: Support = Support.Positive
  final case class Exponential(rate: Real) extends Prior:
    def logPdf(x: Real): Real = if x.value < 0 then Real.const(Double.NegativeInfinity) else log(rate) - rate * x
    def support: Support = Support.Positive
  /** shape α, RATE β, as `Distribution.Gamma` */
  final case class Gamma(shape: Real, rate: Real) extends Prior:
    def logPdf(x: Real): Real =
      if x.value <= 0 then Real.const(Double.NegativeInfinity)
      else shape * log(rate) - lgamma(shape) + (shape - 1) * log(x) - rate * x
    def support: Support = Support.Positive
  final case class Beta(a: Real, b: Real) extends Prior:
    def logPdf(x: Real): Real =
      if x.value <= 0 || x.value >= 1 then Real.const(Double.NegativeInfinity)
      else (a - 1) * log(x) + (b - 1) * log1p(-x) - (lgamma(a) + lgamma(b) - lgamma(a + b))
    def support: Support = Support.Interval(0, 1)
  /** its bounds are numbers, not parameters: a support cannot move with the model */
  final case class Uniform(lo: Double, hi: Double) extends Prior:
    def logPdf(x: Real): Real = Real.const(if x.value < lo || x.value > hi then Double.NegativeInfinity else -math.log(hi - lo))
    def support: Support = Support.Interval(lo, hi)

  final case class Bernoulli(p: Real) extends Density[Boolean]:
    def logPdf(x: Boolean): Real = if x then log(p) else log1p(-p)
  /** Bernoulli on the log-odds — stable where p is near 0 or 1 */
  final case class BernoulliLogit(logit: Real) extends Density[Boolean]:
    def logPdf(x: Boolean): Real = if x then -softplus(-logit) else -softplus(logit)
  final case class Poisson(rate: Real) extends Density[Int]:
    def logPdf(k: Int): Real = k.toDouble * log(rate) - rate - Distribution.logGamma(k + 1.0)
  /** components summed out by log-sum-exp */
  final case class Mixture(components: Vector[(Real, Density[Real])]) extends Density[Real]:
    def logPdf(x: Real): Real = Real.logSumExp(components.map((w, d) => log(w) + d.logPdf(x)))

  inline def param(name: String, prior: Prior): Real ! Grad = effect(Grad.Param(name, prior))
  inline def score(logWeight: Real): Unit ! Grad = effect(Grad.Score(logWeight))
  def observe[X](d: Density[X], x: X): Unit ! Grad = score(d.logPdf(x))
  /** every element in one score, each under the density `d` gives it */
  def observeAll[X, Y](xs: Iterable[X])(d: X => Density[Y], value: X => Y): Unit ! Grad =
    score(sum(xs.map(x => d(x).logPdf(value(x)))))

  /** (x, log |dx/du|) on the tape */
  def constrain(s: Support, u: Real): (Real, Real) = s match
    case Support.Real => (u, Real.const(0.0))
    case Support.Positive => (exp(u), u)
    case Support.Interval(lo, hi) => (lo + (hi - lo) * sigmoid(u), math.log(hi - lo) - softplus(-u) - softplus(u))
    case Support.Discrete => throw IllegalArgumentException("Smooth: a parameter cannot be discrete")

  private final case class Run[A](value: A, names: Vector[String], params: Vector[Real], logp: Real)

  /** run the program, the k-th parameter's unconstrained value `u(k)`, every log weight on the tape */
  private def run[A](p: A ! Grad, u: Int => Real): Run[A] =
    var names = Vector.empty[String]
    var params = Vector.empty[Real]
    var total = Real.const(0.0)
    def answer[X](op: Grad[X]): X = op match
      case Grad.Param(name, prior) =>
        require(!names.contains(name), s"Smooth: parameter '$name' drawn twice")
        val (x, logJ) = constrain(prior.support, u(names.length))
        names = names :+ name
        params = params :+ x
        total = total + prior.logPdf(x) + logJ
        x
      case Grad.Score(w) =>
        total = total + w
        ()
    @tailrec def loop(x: A ! Grad): A = (x.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(op: Grad[A] @unchecked) => answer(op)
      case Free.Bind(Free.Inject(op: Grad[x] @unchecked), k) => loop(k(answer(op)))
    val a = loop(p)
    Run(a, names, params, total)

  /** the program as a `Target` on unconstrained ℝᵈ, its gradient by the tape */
  def target[A](p: A ! Grad): Target =
    val names0 = run(p, _ => Real.const(0.0)).names
    def same(r: Run[A]): Unit =
      if r.names != names0 then
        throw IllegalArgumentException(s"Smooth: the parameters changed with a value (${r.names} against $names0) — use metropolis")
    new Target:
      def dim: Int = names0.length
      def logp(u: Array[Double]): Double =
        val r = run(p, k => if k < u.length then Real.const(u(k)) else Real.const(0.0))
        same(r)
        if r.logp.value.isNaN then Double.NegativeInfinity else r.logp.value
      def gradient(u: Array[Double]): (Double, Array[Double]) =
        val tape = new Tape
        val in = u.toIndexedSeq.map(tape.variable)
        val r = run(p, k => if k < in.length then in(k) else Real.const(0.0))
        same(r)
        val v = if r.logp.value.isNaN then Double.NegativeInfinity else r.logp.value
        (v, tape.gradient(r.logp, in))

  /** the program's parameter names, in the order the program draws them */
  def names[A](p: A ! Grad): Vector[String] = run(p, _ => Real.const(0.0)).names

  /** the `Sampler` in scope (ours unless an import says otherwise) on `target(p)`: the program's values and each parameter's draws */
  def nuts[A](p: A ! Grad, samples: Int, burn: Int = 1000, chains: Int = 1, seed: Long = 42L, delta: Double = 0.8)(using sampler: Sampler): Posterior[A] =
    val t = target(p)
    Posterior(Vector.tabulate(chains) { c =>
      val ch = sampler.run(t, samples, burn, seed + c, delta)
      val runs = ch.draws.map(u => run(p, k => Real.const(u(k))))
      Chain(runs.map(_.value), runs.map(r => r.names.zip(r.params.map(_.value)).toMap),
        Map("(nuts accept)" -> ch.acceptance, "(divergent)" -> ch.divergences.toDouble / math.max(1, samples), "(tree depth)" -> ch.meanDepth))
    })
