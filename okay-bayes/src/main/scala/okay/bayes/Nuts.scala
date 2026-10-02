package okay.bayes

import scala.util.Random

/**
 * WHAT A GRADIENT SAMPLER NEEDS of a model (specs/okay-bayes.md stage 3):
 * a dimension and, on unconstrained ℝᵈ, the log density (up to a constant)
 * and its gradient. Where the gradient comes from is the target's own
 * business: central finite differences over an ordinary model
 * (`Target.finite`), or automatic differentiation.
 */
trait Target:
  def dim: Int
  def logp(u: Array[Double]): Double
  /** the log density and its gradient at u */
  def gradient(u: Array[Double]): (Double, Array[Double])
  /** a starting point; Stan's default, uniform on (−2, 2) per coordinate */
  def init(rng: Random): Array[Double] = Array.fill(dim)(rng.nextDouble() * 4 - 2)

object Target:
  /**
   * a target whose gradient is by CENTRAL FINITE DIFFERENCES: 2d + 1
   * evaluations of `f` per gradient, the step relative to each coordinate.
   * Truncation error O(h²); enough for a sampler, whose accept step corrects
   * any error in the gradient — it only costs efficiency, never correctness.
   */
  def finite(d: Int)(f: Array[Double] => Double): Target = new Target:
    def dim: Int = d
    def logp(u: Array[Double]): Double = f(u)
    def gradient(u: Array[Double]): (Double, Array[Double]) =
      val at = f(u)
      val g = new Array[Double](d)
      val v = u.clone()
      var i = 0
      while i < d do
        val h = 1e-5 * math.max(1.0, math.abs(u(i)))
        v(i) = u(i) + h
        val up = f(v)
        v(i) = u(i) - h
        val down = f(v)
        v(i) = u(i)
        g(i) = (up - down) / (2 * h)
        i += 1
      (at, g)

/** one NUTS chain: draws on ℝᵈ, the adapted step and mass, and what the trajectories did */
final case class NutsChain(draws: Vector[Vector[Double]], stepSize: Double, inverseMass: Vector[Double],
  divergences: Int, acceptance: Double, meanDepth: Double)

/**
 * THE NO-U-TURN SAMPLER (Hoffman & Gelman, JMLR 2014, Algorithm 6): HMC
 * whose trajectory doubles, forward or backward at random, until it turns
 * back on itself; a slice variable decides which states are admissible, and
 * the step size is tuned during warmup by dual averaging toward an
 * acceptance statistic `delta`. A diagonal inverse mass matrix is learnt
 * from the middle of warmup (Stan's regularised variance), after which the
 * step size is tuned again.
 */
object Nuts:
  private val DeltaMax = 1000.0

  private final class Point(val q: Array[Double], val p: Array[Double], val logp: Double, val grad: Array[Double])

  def sample(target: Target, samples: Int, burn: Int = 1000, seed: Long = 42L, delta: Double = 0.8, maxDepth: Int = 10): NutsChain =
    val rng = new Random(seed)
    val d = target.dim
    var minv = Array.fill(d)(1.0)

    def kinetic(p: Array[Double]): Double =
      var k = 0.0
      var i = 0
      while i < d do { k += p(i) * p(i) * minv(i); i += 1 }
      0.5 * k
    def momentum(): Array[Double] = Array.tabulate(d)(i => rng.nextGaussian() / math.sqrt(minv(i)))
    def leapfrog(x: Point, eps: Double): Point =
      val p = Array.tabulate(d)(i => x.p(i) + 0.5 * eps * x.grad(i))
      val q = Array.tabulate(d)(i => x.q(i) + eps * minv(i) * p(i))
      val (lp, g) = target.gradient(q)
      if lp.isNaN || lp == Double.NegativeInfinity then Point(q, p, Double.NegativeInfinity, g)
      else Point(q, Array.tabulate(d)(i => p(i) + 0.5 * eps * g(i)), lp, g)
    def joint(x: Point): Double = x.logp - kinetic(x.p)
    // (q⁺ − q⁻) · M⁻¹ p ≥ 0 at both ends: the trajectory has not turned back
    def noUTurn(minus: Point, plus: Point): Boolean =
      var a = 0.0
      var b = 0.0
      var i = 0
      while i < d do
        val dq = plus.q(i) - minus.q(i)
        a += dq * minv(i) * minus.p(i)
        b += dq * minv(i) * plus.p(i)
        i += 1
      a >= 0 && b >= 0

    // a start with a finite density
    var start = target.init(rng)
    var tries = 1
    while target.logp(start) == Double.NegativeInfinity && tries < 1000 do { start = target.init(rng); tries += 1 }
    require(target.logp(start) > Double.NegativeInfinity, "nuts: no starting point with a finite density in 1000 tries")
    var cur: Point =
      val (lp, g) = target.gradient(start)
      Point(start, new Array[Double](d), lp, g)

    def reasonableStep(): Double =
      var eps = 1.0
      def ratio(): Double =
        val x = Point(cur.q, momentum(), cur.logp, cur.grad)
        val y = leapfrog(x, eps)
        math.exp(joint(y) - joint(x))
      var r = ratio()
      val a = if r > 0.5 then 1 else -1
      var halvings = 0
      while (if a > 0 then r > 0.5 else !(r > 0.5)) && halvings < 100 do
        eps = if a > 0 then eps * 2 else eps / 2
        r = ratio()
        halvings += 1
      eps

    final case class Tree(minus: Point, plus: Point, chosen: Point, n: Int, ok: Boolean, alpha: Double, nAlpha: Int, diverged: Boolean)

    // recursion BOUNDED by maxDepth (default 10): `build` calls itself at depth − 1
    def build(x: Point, logu: Double, v: Int, depth: Int, eps: Double, joint0: Double): Tree =
      if depth == 0 then
        val y = leapfrog(x, v * eps)
        val j = joint(y)
        val n = if logu <= j then 1 else 0
        val diverged = !(logu < DeltaMax + j)
        Tree(y, y, y, n, !diverged, if j.isNaN then 0.0 else math.min(1.0, math.exp(j - joint0)), 1, diverged)
      else
        val t = build(x, logu, v, depth - 1, eps, joint0)
        if !t.ok then t
        else
          val u = if v < 0 then build(t.minus, logu, v, depth - 1, eps, joint0) else build(t.plus, logu, v, depth - 1, eps, joint0)
          val (minus, plus) = if v < 0 then (u.minus, t.plus) else (t.minus, u.plus)
          val total = t.n + u.n
          val chosen = if total > 0 && rng.nextDouble() < u.n.toDouble / total then u.chosen else t.chosen
          Tree(minus, plus, chosen, total, u.ok && noUTurn(minus, plus), t.alpha + u.alpha, t.nAlpha + u.nAlpha, t.diverged || u.diverged)

    // dual averaging (Nesterov 2009, as Hoffman & Gelman §3.2)
    val (gamma, t0, kappa) = (0.05, 10.0, 0.75)
    var eps = reasonableStep()
    var mu = math.log(10 * eps)
    var hBar = 0.0
    var logEpsBar = 0.0
    var m = 0
    def restartAdaptation(): Unit =
      eps = reasonableStep()
      mu = math.log(10 * eps)
      hBar = 0.0
      logEpsBar = 0.0
      m = 0

    // the mass window: draws from 15% to 75% of warmup, then the step tuned again on the new metric
    val (windowStart, windowEnd) = ((burn * 0.15).toInt, (burn * 0.75).toInt)
    val window = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    val draws = Vector.newBuilder[Vector[Double]]
    var divergences = 0
    var acceptSum = 0.0
    var depthSum = 0.0

    var it = 0
    while it < burn + samples do
      val x0 = Point(cur.q, momentum(), cur.logp, cur.grad)
      val joint0 = joint(x0)
      val logu = joint0 + math.log(rng.nextDouble())
      var minus = x0
      var plus = x0
      var next = cur
      var n = 1
      var ok = true
      var depth = 0
      var alpha = 0.0
      var nAlpha = 1
      var diverged = false
      while ok && depth < maxDepth do
        val v = if rng.nextBoolean() then 1 else -1
        val t = if v < 0 then build(minus, logu, v, depth, eps, joint0) else build(plus, logu, v, depth, eps, joint0)
        if v < 0 then minus = t.minus else plus = t.plus
        if t.ok && rng.nextDouble() < t.n.toDouble / n then next = t.chosen
        n += t.n
        ok = t.ok && noUTurn(minus, plus)
        alpha = t.alpha
        nAlpha = t.nAlpha
        diverged = diverged || t.diverged
        depth += 1
      cur = next
      val stat = alpha / nAlpha
      if it < burn then
        m += 1
        hBar = (1 - 1.0 / (m + t0)) * hBar + (delta - stat) / (m + t0)
        val logEps = mu - math.sqrt(m.toDouble) / gamma * hBar
        val w = math.pow(m.toDouble, -kappa)
        logEpsBar = w * logEps + (1 - w) * logEpsBar
        eps = math.exp(logEps)
        if it >= windowStart && it < windowEnd then window += cur.q.clone(): Unit
        if it == windowEnd - 1 && window.length > 10 then
          val k = window.length.toDouble
          minv = Array.tabulate(d) { i =>
            val mean = window.iterator.map(_(i)).sum / k
            val variance = window.iterator.map(q => (q(i) - mean) * (q(i) - mean)).sum / (k - 1)
            (k / (k + 5)) * variance + 1e-3 * (5 / (k + 5))
          }
          restartAdaptation()
        if it == burn - 1 then eps = math.exp(logEpsBar)
      else
        draws += cur.q.toVector
        if diverged then divergences += 1
        acceptSum += stat
        depthSum += depth
      it += 1
    NutsChain(draws.result(), eps, minv.toVector, divergences, acceptSum / math.max(1, samples), depthSum / math.max(1, samples))
