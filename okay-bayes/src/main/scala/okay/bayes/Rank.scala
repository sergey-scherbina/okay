package okay.bayes

/**
 * RANKING BY EVIDENCE (*Bayesian Methods for Hackers* ch.4): an item with
 * `up` approvals and `down` rejections has, under a uniform prior, a
 * Beta(1 + up, 1 + down) posterior for its true approval rate. Ordering by
 * the raw ratio puts one vote of one above 999 of 1000; ordering by a LOW
 * QUANTILE of the posterior asks "how good is it at least, plausibly?",
 * and a small sample cannot claim much.
 */
object Rank:
  /** the `level` quantile of Beta(1 + up, 1 + down) — exact */
  def lowerBound(up: Int, down: Int, level: Double = 0.05): Double =
    Distribution.Beta(1.0 + up, 1.0 + down).quantile(level)

  /** the book's approximation: the posterior mean less 1.65 posterior sds (a normal 5% bound) */
  def approxLowerBound(up: Int, down: Int): Double =
    val (a, b) = (1.0 + up, 1.0 + down)
    a / (a + b) - 1.65 * math.sqrt(a * b / ((a + b) * (a + b) * (a + b + 1)))

  /** `items`, best first by their lower bound */
  def sort[A](items: Seq[A], level: Double = 0.05)(votes: A => (Int, Int)): Vector[A] =
    items.toVector.map(x => (x, { val (u, d) = votes(x); lowerBound(u, d, level) })).sortBy(-_._2).map(_._1)
