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

  private[bayes] final class Point(val q: Array[Double], val p: Array[Double], val logp: Double, val grad: Array[Double])

  /** one transition's outcome: where it went, its acceptance statistic, its depth, whether it diverged */
  private[bayes] final case class Step(to: Point, stat: Double, depth: Int, diverged: Boolean)

  /**
   * THE DYNAMICS of one target under one inverse mass matrix: leapfrog,
   * the doubling tree, and ONE NUTS transition (Algorithm 6). Shared by
   * `sample` (a whole chain) and `Adapting` (a kernel's transition, one at
   * a time); every random draw is taken in the order Algorithm 6 takes it.
   */
  private[bayes] final class Dynamics(target: Target, rng: Random):
    private val d = target.dim
    var minv: Array[Double] = Array.fill(d)(1.0)

    def at(q: Array[Double]): Point =
      val (lp, g) = target.gradient(q)
      Point(q, new Array[Double](d), lp, g)

    private def kinetic(p: Array[Double]): Double =
      var k = 0.0
      var i = 0
      while i < d do { k += p(i) * p(i) * minv(i); i += 1 }
      0.5 * k
    private def momentum(): Array[Double] = Array.tabulate(d)(i => rng.nextGaussian() / math.sqrt(minv(i)))
    private def leapfrog(x: Point, eps: Double): Point =
      val p = Array.tabulate(d)(i => x.p(i) + 0.5 * eps * x.grad(i))
      val q = Array.tabulate(d)(i => x.q(i) + eps * minv(i) * p(i))
      val (lp, g) = target.gradient(q)
      if lp.isNaN || lp == Double.NegativeInfinity then Point(q, p, Double.NegativeInfinity, g)
      else Point(q, Array.tabulate(d)(i => p(i) + 0.5 * eps * g(i)), lp, g)
    private def joint(x: Point): Double = x.logp - kinetic(x.p)
    // (q⁺ − q⁻) · M⁻¹ p ≥ 0 at both ends: the trajectory has not turned back
    private def noUTurn(minus: Point, plus: Point): Boolean =
      var a = 0.0
      var b = 0.0
      var i = 0
      while i < d do
        val dq = plus.q(i) - minus.q(i)
        a += dq * minv(i) * minus.p(i)
        b += dq * minv(i) * plus.p(i)
        i += 1
      a >= 0 && b >= 0

    /** a step size whose leapfrog moves the acceptance across 1/2, by doubling or halving (Hoffman & Gelman, Alg. 4) */
    def reasonableStep(cur: Point): Double =
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

    private final case class Tree(minus: Point, plus: Point, chosen: Point, n: Int, ok: Boolean, alpha: Double, nAlpha: Int, diverged: Boolean)

    // recursion BOUNDED by maxDepth (default 10): `build` calls itself at depth − 1
    private def build(x: Point, logu: Double, v: Int, depth: Int, eps: Double, joint0: Double): Tree =
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

    /** ONE NUTS transition from `cur` with step `eps` */
    def transition(cur: Point, eps: Double, maxDepth: Int): Step =
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
      Step(next, alpha / nAlpha, depth, diverged)

  /** dual averaging of the log step size toward an acceptance statistic `delta` (Nesterov 2009; Hoffman & Gelman §3.2) */
  private[bayes] final class DualAveraging(delta: Double, start: Double):
    private val (gamma, t0, kappa) = (0.05, 10.0, 0.75)
    private val mu = math.log(10 * start)
    private var hBar = 0.0
    private var logEpsBar = 0.0
    private var m = 0
    var eps: Double = start
    def learn(stat: Double): Unit =
      m += 1
      hBar = (1 - 1.0 / (m + t0)) * hBar + (delta - stat) / (m + t0)
      val logEps = mu - math.sqrt(m.toDouble) / gamma * hBar
      val w = math.pow(m.toDouble, -kappa)
      logEpsBar = w * logEps + (1 - w) * logEpsBar
      eps = math.exp(logEps)
    /** the averaged step, for sampling once tuning ends */
    def averaged: Double = if m == 0 then eps else math.exp(logEpsBar)

  /** Stan's regularised variance of a window of draws, per coordinate */
  private def regularised(window: collection.Seq[Array[Double]], d: Int): Array[Double] =
    val k = window.length.toDouble
    Array.tabulate(d) { i =>
      val mean = window.iterator.map(_(i)).sum / k
      val variance = window.iterator.map(q => (q(i) - mean) * (q(i) - mean)).sum / (k - 1)
      (k / (k + 5)) * variance + 1e-3 * (5 / (k + 5))
    }

  /**
   * NUTS as a KERNEL's transition, one call at a time, for a target whose
   * other coordinates may have moved between calls: while `tuning`, the
   * step size is dual-averaged and the diagonal metric re-estimated on
   * doubling windows (50, 100, 200, … draws, Stan's shape), the step tuned
   * again after each; once a call comes with tuning off, the step is
   * frozen at its average.
   */
  private[bayes] final class Adapting(delta: Double, maxDepth: Int):
    private var dual: DualAveraging = null
    private var frozen = false
    private val window = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    private var windowSize = 50
    private var minv: Array[Double] = null
    var lastStat = 0.0
    var lastDiverged = false
    def step(target: Target, q: Array[Double], tuning: Boolean, rng: Random): Array[Double] =
      val dyn = Dynamics(target, rng)
      if minv == null then minv = Array.fill(target.dim)(1.0)
      dyn.minv = minv
      val cur = dyn.at(q)
      if cur.logp == Double.NegativeInfinity then q
      else
        if dual == null then dual = DualAveraging(delta, dyn.reasonableStep(cur))
        if !tuning && !frozen then { frozen = true; dual.eps = dual.averaged }
        val s = dyn.transition(cur, dual.eps, maxDepth)
        lastStat = s.stat
        lastDiverged = s.diverged
        if tuning then
          dual.learn(s.stat)
          window += s.to.q.clone(): Unit
          if window.length >= windowSize then
            minv = regularised(window, target.dim)
            window.clear()
            windowSize *= 2
            dyn.minv = minv
            dual = DualAveraging(delta, dyn.reasonableStep(s.to))
        s.to.q

  def sample(target: Target, samples: Int, burn: Int = 1000, seed: Long = 42L, delta: Double = 0.8, maxDepth: Int = 10): NutsChain =
    val rng = new Random(seed)
    val d = target.dim
    val dyn = Dynamics(target, rng)

    // a start with a finite density
    var start = target.init(rng)
    var tries = 1
    while target.logp(start) == Double.NegativeInfinity && tries < 1000 do { start = target.init(rng); tries += 1 }
    require(target.logp(start) > Double.NegativeInfinity, "nuts: no starting point with a finite density in 1000 tries")
    var cur = dyn.at(start)

    var dual = DualAveraging(delta, dyn.reasonableStep(cur))
    var eps = dual.eps

    // the mass window: draws from 15% to 75% of warmup, then the step tuned again on the new metric
    val (windowStart, windowEnd) = ((burn * 0.15).toInt, (burn * 0.75).toInt)
    val window = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    val draws = Vector.newBuilder[Vector[Double]]
    var divergences = 0
    var acceptSum = 0.0
    var depthSum = 0.0

    var it = 0
    while it < burn + samples do
      val s = dyn.transition(cur, eps, maxDepth)
      cur = s.to
      if it < burn then
        dual.learn(s.stat)
        eps = dual.eps
        if it >= windowStart && it < windowEnd then window += cur.q.clone(): Unit
        if it == windowEnd - 1 && window.length > 10 then
          dyn.minv = regularised(window, d)
          dual = DualAveraging(delta, dyn.reasonableStep(cur))
          eps = dual.eps
        if it == burn - 1 then eps = dual.averaged
      else
        draws += cur.q.toVector
        if s.diverged then divergences += 1
        acceptSum += s.stat
        depthSum += s.depth
      it += 1
    NutsChain(draws.result(), eps, dyn.minv.toVector, divergences, acceptSum / math.max(1, samples), depthSum / math.max(1, samples))
