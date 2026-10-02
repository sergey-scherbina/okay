package okay.bayes

import scala.util.Random

/**
 * A PROBABILITY DISTRIBUTION as a model's draw needs it (specs/okay-bayes.md):
 * its log density (log mass for a count), a draw, a SYMMETRIC proposal
 * around a current value for Metropolis–Hastings, and `coerce` — the
 * distribution taking its own value back out of a trace that holds the
 * values of every site, by matching what it produces (no cast).
 *
 * Out of the support, `logPdf` is negative infinity: a proposal there is
 * simply never accepted.
 */
trait Distribution[A] extends Serializable:
  def logPdf(a: A): Double
  def sample(rng: Random): A
  /** a value near `a`, drawn so that `propose(b → a)` is as likely as `propose(a → b)` */
  def propose(a: A, scale: Double, rng: Random): A
  def coerce(v: Any): Option[A]
  /** the value as a number, for summaries */
  def numeric(a: A): Double
  /** where the values lie — what gradient samplers move through unconstrained;
   * a distribution that does not say is treated as discrete, and refused by them */
  def support: Support = Support.Discrete

/**
 * WHERE A DISTRIBUTION'S VALUES LIE, for a sampler that moves in
 * unconstrained space (HMC/NUTS): ℝ as it is, positive through the log, an
 * interval through a scaled logit — `constrain` maps u ∈ ℝ in, with the
 * log Jacobian of that map.
 */
enum Support:
  case Real, Positive
  case Interval(lo: Double, hi: Double)
  case Discrete

  def continuous: Boolean = this != Discrete

  /** (x, log |dx/du|) for an unconstrained u */
  def constrain(u: Double): (Double, Double) = this match
    case Real => (u, 0.0)
    case Positive => (math.exp(u), u)
    case Interval(lo, hi) =>
      // σ(u) and log σ(u)(1 − σ(u)) computed stably on both sides of 0
      val s = 1 / (1 + math.exp(-u))
      val logJ = math.log(hi - lo) - math.abs(u) - 2 * math.log1p(math.exp(-math.abs(u)))
      (lo + (hi - lo) * s, logJ)
    case Discrete => throw IllegalStateException("a discrete support has no unconstrained form")

  /** the u that `constrain` maps to x */
  def unconstrain(x: Double): Double = this match
    case Real => x
    case Positive => math.log(x)
    case Interval(lo, hi) => val s = (x - lo) / (hi - lo); math.log(s / (1 - s))
    case Discrete => throw IllegalStateException("a discrete support has no unconstrained form")

object Distribution:
  val NegInf: Double = Double.NegativeInfinity
  private val LogSqrt2Pi = 0.5 * math.log(2 * math.Pi)

  /** ln Γ(x) for x > 0 — Lanczos (g = 7, n = 9), ~15 significant digits */
  def logGamma(x: Double): Double =
    val g = 7.0
    val c = Array(0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)
    // the series for an argument >= 0.5; below it, the reflection formula on 1 - x
    def series(z: Double): Double =
      val y = z - 1
      var a = c(0)
      val t = y + g + 0.5
      var i = 1
      while i < 9 do { a += c(i) / (y + i); i += 1 }
      0.5 * math.log(2 * math.Pi) + (y + 0.5) * math.log(t) - t + math.log(a)
    if x < 0.5 then math.log(math.Pi / math.abs(math.sin(math.Pi * x))) - series(1 - x) else series(x)

  /** ln B(a, b) */
  def logBeta(a: Double, b: Double): Double = logGamma(a) + logGamma(b) - logGamma(a + b)

  /** ln C(n, k) */
  def logChoose(n: Int, k: Int): Double = logGamma(n + 1.0) - logGamma(k + 1.0) - logGamma(n - k + 1.0)

  private def double(v: Any): Option[Double] = v match
    case d: Double => Some(d)
    case _ => None
  private def int(v: Any): Option[Int] = v match
    case i: Int => Some(i)
    case _ => None
  private def bool(v: Any): Option[Boolean] = v match
    case b: Boolean => Some(b)
    case _ => None

  /** a Gaussian random-walk step, reflecting nothing: symmetric by construction */
  private def walk(a: Double, scale: Double, rng: Random): Double = a + scale * rng.nextGaussian()
  /** an integer random walk: ±1..±k, symmetric, never zero */
  private def step(a: Int, scale: Double, rng: Random): Int =
    val k = math.max(1, math.round(scale).toInt)
    val d = 1 + rng.nextInt(k)
    if rng.nextBoolean() then a + d else a - d

  /** Marsaglia & Tsang (2000) for shape >= 1, boosted by U^(1/shape) below 1; rate 1 */
  private[bayes] def gamma1(shape: Double, rng: Random): Double =
    if shape < 1 then marsagliaTsang(shape + 1, rng) * math.pow(rng.nextDouble(), 1 / shape)
    else marsagliaTsang(shape, rng)

  private def marsagliaTsang(shape: Double, rng: Random): Double =
      val d = shape - 1.0 / 3
      val c = 1 / math.sqrt(9 * d)
      var out = -1.0
      while out < 0 do
        var x = 0.0
        var v = -1.0
        while v <= 0 do
          x = rng.nextGaussian()
          v = 1 + c * x
        v = v * v * v
        val u = rng.nextDouble()
        if u < 1 - 0.0331 * x * x * x * x || math.log(u) < 0.5 * x * x + d * (1 - v + math.log(v)) then out = d * v
      out

  final case class Normal(mu: Double, sigma: Double) extends Distribution[Double]:
    require(sigma > 0, s"Normal: sigma must be positive, got $sigma")
    def logPdf(x: Double): Double = { val z = (x - mu) / sigma; -0.5 * z * z - math.log(sigma) - LogSqrt2Pi }
    def sample(rng: Random): Double = mu + sigma * rng.nextGaussian()
    def propose(a: Double, scale: Double, rng: Random): Double = walk(a, scale * sigma, rng)
    def coerce(v: Any): Option[Double] = double(v)
    def numeric(a: Double): Double = a
    override def support: Support = Support.Real

  final case class Exponential(rate: Double) extends Distribution[Double]:
    require(rate > 0, s"Exponential: rate must be positive, got $rate")
    def logPdf(x: Double): Double = if x < 0 then NegInf else math.log(rate) - rate * x
    def sample(rng: Random): Double = -math.log(1 - rng.nextDouble()) / rate
    def propose(a: Double, scale: Double, rng: Random): Double = walk(a, scale / rate, rng)
    def coerce(v: Any): Option[Double] = double(v)
    def numeric(a: Double): Double = a
    override def support: Support = Support.Positive

  /** shape α, RATE β (mean α/β), as PyMC's Gamma(alpha, beta) */
  final case class Gamma(shape: Double, rate: Double) extends Distribution[Double]:
    require(shape > 0 && rate > 0, s"Gamma: shape and rate must be positive, got $shape, $rate")
    def logPdf(x: Double): Double =
      if x <= 0 then NegInf else shape * math.log(rate) - logGamma(shape) + (shape - 1) * math.log(x) - rate * x
    def sample(rng: Random): Double = gamma1(shape, rng) / rate
    def propose(a: Double, scale: Double, rng: Random): Double = walk(a, scale * math.sqrt(shape) / rate, rng)
    def coerce(v: Any): Option[Double] = double(v)
    def numeric(a: Double): Double = a
    override def support: Support = Support.Positive

  final case class Beta(a: Double, b: Double) extends Distribution[Double]:
    require(a > 0 && b > 0, s"Beta: a and b must be positive, got $a, $b")
    def logPdf(x: Double): Double =
      if x <= 0 || x >= 1 then NegInf else (a - 1) * math.log(x) + (b - 1) * math.log(1 - x) - logBeta(a, b)
    def sample(rng: Random): Double = { val x = gamma1(a, rng); val y = gamma1(b, rng); x / (x + y) }
    def propose(v: Double, scale: Double, rng: Random): Double = walk(v, scale * 0.1, rng)
    def coerce(v: Any): Option[Double] = double(v)
    def numeric(v: Double): Double = v
    override def support: Support = Support.Interval(0, 1)

  final case class Uniform(lo: Double, hi: Double) extends Distribution[Double]:
    require(hi > lo, s"Uniform: hi must exceed lo, got [$lo, $hi]")
    def logPdf(x: Double): Double = if x < lo || x > hi then NegInf else -math.log(hi - lo)
    def sample(rng: Random): Double = lo + (hi - lo) * rng.nextDouble()
    def propose(a: Double, scale: Double, rng: Random): Double = walk(a, scale * (hi - lo) * 0.1, rng)
    def coerce(v: Any): Option[Double] = double(v)
    def numeric(a: Double): Double = a
    override def support: Support = Support.Interval(lo, hi)

  final case class Poisson(rate: Double) extends Distribution[Int]:
    require(rate > 0, s"Poisson: rate must be positive, got $rate")
    def logPdf(k: Int): Double = if k < 0 then NegInf else k * math.log(rate) - rate - logGamma(k + 1.0)
    def sample(rng: Random): Int =
      if rate < 30 then
        // Knuth: multiply uniforms until the product drops below e^-rate
        val limit = math.exp(-rate)
        var k = 0
        var p = rng.nextDouble()
        while p > limit do { k += 1; p *= rng.nextDouble() }
        k
      else
        // PTRS, Hörmann (1993): transformed rejection with squeeze, for large rates
        val slam = math.sqrt(rate)
        val loglam = math.log(rate)
        val b = 0.931 + 2.53 * slam
        val a = -0.059 + 0.02483 * b
        val invalpha = 1.1239 + 1.1328 / (b - 3.4)
        val vr = 0.9277 - 3.6224 / (b - 2)
        var out = -1
        while out < 0 do
          val u = rng.nextDouble() - 0.5
          val v = rng.nextDouble()
          val us = 0.5 - math.abs(u)
          val k = math.floor((2 * a / us + b) * u + rate + 0.43).toInt
          if us >= 0.07 && v <= vr then out = k
          else if k >= 0 && !(us < 0.013 && v > us) &&
            math.log(v) + math.log(invalpha) - math.log(a / (us * us) + b) <= -rate + k * loglam - logGamma(k + 1.0) then out = k
        out
    def propose(k: Int, scale: Double, rng: Random): Int = step(k, scale * math.sqrt(rate), rng)
    def coerce(v: Any): Option[Int] = int(v)
    def numeric(k: Int): Double = k.toDouble

  final case class Bernoulli(p: Double) extends Distribution[Boolean]:
    require(p >= 0 && p <= 1, s"Bernoulli: p must be in [0, 1], got $p")
    def logPdf(x: Boolean): Double = math.log(if x then p else 1 - p)
    def sample(rng: Random): Boolean = rng.nextDouble() < p
    def propose(x: Boolean, scale: Double, rng: Random): Boolean = !x
    def coerce(v: Any): Option[Boolean] = bool(v)
    def numeric(x: Boolean): Double = if x then 1.0 else 0.0

  final case class Binomial(n: Int, p: Double) extends Distribution[Int]:
    require(n >= 0 && p >= 0 && p <= 1, s"Binomial: n >= 0 and p in [0, 1], got $n, $p")
    def logPdf(k: Int): Double =
      if k < 0 || k > n then NegInf
      else if p == 0 then (if k == 0 then 0.0 else NegInf)
      else if p == 1 then (if k == n then 0.0 else NegInf)
      else logChoose(n, k) + k * math.log(p) + (n - k) * math.log(1 - p)
    def sample(rng: Random): Int =
      var k = 0
      var i = 0
      while i < n do { if rng.nextDouble() < p then k += 1; i += 1 }
      k
    def propose(k: Int, scale: Double, rng: Random): Int = step(k, scale * math.sqrt(n * p * (1 - p) + 1), rng)
    def coerce(v: Any): Option[Int] = int(v)
    def numeric(k: Int): Double = k.toDouble

  /** every integer in [lo, hi] — hi INCLUSIVE, as PyMC's DiscreteUniform */
  final case class DiscreteUniform(lo: Int, hi: Int) extends Distribution[Int]:
    require(hi >= lo, s"DiscreteUniform: hi must be >= lo, got [$lo, $hi]")
    def logPdf(k: Int): Double = if k < lo || k > hi then NegInf else -math.log(hi - lo + 1.0)
    def sample(rng: Random): Int = lo + rng.nextInt(hi - lo + 1)
    def propose(k: Int, scale: Double, rng: Random): Int = step(k, scale * math.max(1.0, (hi - lo) / 20.0), rng)
    def coerce(v: Any): Option[Int] = int(v)
    def numeric(k: Int): Double = k.toDouble

  /**
   * a WEIGHTED MIXTURE: draw a component by its weight, then draw from it.
   * The density sums the components out — log Σ w·p(x), by log-sum-exp — so
   * a model observing through a mixture never samples which component a
   * point came from (PyMC's `Mixture`). Proposals, `coerce` and `numeric`
   * are the first component's: every component draws from one space.
   */
  final case class Mixture[A](components: Vector[(Double, Distribution[A])]) extends Distribution[A]:
    require(components.nonEmpty && components.forall(_._1 >= 0) && math.abs(components.map(_._1).sum - 1) < 1e-9,
      s"Mixture: weights must be non-negative and sum to 1, got ${components.map(_._1)}")
    private val logWeights = components.map((w, _) => math.log(w))
    def logPdf(x: A): Double =
      val terms = components.indices.map(i => logWeights(i) + components(i)._2.logPdf(x))
      val top = terms.max
      if top == NegInf then NegInf else top + math.log(terms.iterator.map(t => math.exp(t - top)).sum)
    def sample(rng: Random): A =
      val u = rng.nextDouble()
      val cum = components.map(_._1).scanLeft(0.0)(_ + _).tail
      val i = cum.indexWhere(u < _)
      components(if i < 0 then components.length - 1 else i)._2.sample(rng)
    def propose(a: A, scale: Double, rng: Random): A = components.head._2.propose(a, scale, rng)
    def coerce(v: Any): Option[A] = components.head._2.coerce(v)
    def numeric(a: A): Double = components.head._2.numeric(a)
    override def support: Support = components.head._2.support
