package okay.bayes

/**
 * THE BAYES ACTION (*Bayesian Methods for Hackers* ch.5; Berger,
 * *Statistical Decision Theory and Bayesian Analysis*, 1985): a posterior
 * is not yet a decision. Given a LOSS — what deciding `a` costs when the
 * truth is θ — the best decision minimises the expected loss over the
 * posterior, estimated over its draws. Squared loss gives back the mean,
 * absolute loss the median, but an asymmetric cost (overbidding loses the
 * prize, underbidding only some of it) moves the answer where no summary
 * statistic would.
 */
object Decision:
  /** E[loss(θ, a)] over the draws */
  def expectedLoss[T](draws: Seq[T], a: Double)(loss: (T, Double) => Double): Double =
    draws.iterator.map(loss(_, a)).sum / draws.length

  /**
   * argmin over a ∈ [lo, hi] of the expected loss: a grid of `points` to
   * find the basin (losses need not be convex), then golden-section search
   * between the best grid point's neighbours
   */
  def action[T](draws: Seq[T], lo: Double, hi: Double, points: Int = 400)(loss: (T, Double) => Double): Double =
    require(hi > lo && points >= 3 && draws.nonEmpty, "Decision.action: an interval, a grid of 3 or more, some draws")
    def f(a: Double) = expectedLoss(draws, a)(loss)
    val grid = Vector.tabulate(points)(i => lo + (hi - lo) * i / (points - 1))
    val best = grid.indices.minBy(i => f(grid(i)))
    var a = grid(math.max(0, best - 1))
    var b = grid(math.min(points - 1, best + 1))
    val ratio = (math.sqrt(5) - 1) / 2
    var c = b - ratio * (b - a)
    var d = a + ratio * (b - a)
    var (fc, fd) = (f(c), f(d))
    var i = 0
    while i < 200 && b - a > 1e-12 * math.max(1, math.abs(a)) do
      if fc <= fd then { b = d; d = c; fd = fc; c = b - ratio * (b - a); fc = f(c) }
      else { a = c; c = d; fc = fd; d = a + ratio * (b - a); fd = f(d) }
      i += 1
    val inside = 0.5 * (a + b)
    // the refinement never loses to the grid point it started from
    if f(inside) <= f(grid(best)) then inside else grid(best)

/** losses whose Bayes actions are known */
object Loss:
  /** (θ − a)²: the action is the posterior mean */
  val squared: (Double, Double) => Double = (t, a) => (t - a) * (t - a)
  /** |θ − a|: the action is the posterior median */
  val absolute: (Double, Double) => Double = (t, a) => math.abs(t - a)
  /** the pinball (quantile) loss: τ·(θ − a) above, (1 − τ)·(a − θ) below — the action is the τ-quantile */
  def pinball(tau: Double): (Double, Double) => Double = (t, a) => if t >= a then tau * (t - a) else (1 - tau) * (a - t)
