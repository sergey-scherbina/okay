package okay.bayes

/**
 * READING A POSTERIOR (specs/okay-bayes.md): the numbers a chain is read
 * through — its mean and spread, quantiles, the highest-density interval,
 * how many INDEPENDENT draws it is worth (ESS), and whether several chains
 * agree (split R-hat). The definitions are ArviZ's / Stan's, so a number
 * here reads the same as one from PyMC.
 */
object Summary:
  def mean(xs: Seq[Double]): Double = xs.sum / xs.length

  def sd(xs: Seq[Double]): Double =
    val m = mean(xs)
    math.sqrt(xs.iterator.map(x => (x - m) * (x - m)).sum / (xs.length - 1))

  /** the q-quantile, linear between order statistics (numpy's default) */
  def quantile(xs: Seq[Double], q: Double): Double =
    val s = xs.sorted
    val h = (s.length - 1) * q
    val lo = math.floor(h).toInt
    val hi = math.min(lo + 1, s.length - 1)
    s(lo) + (h - lo) * (s(hi) - s(lo))

  /** the HIGHEST-DENSITY INTERVAL holding `mass` of the draws: the shortest such interval */
  def hdi(xs: Seq[Double], mass: Double = 0.94): (Double, Double) =
    val s = xs.sorted.toVector
    val n = s.length
    val k = math.max(1, math.floor(mass * n).toInt)
    var best = 0
    var width = Double.PositiveInfinity
    var i = 0
    while i + k < n do
      val w = s(i + k) - s(i)
      if w < width then { width = w; best = i }
      i += 1
    (s(best), s(math.min(best + k, n - 1)))

  /** the autocorrelation at lag `t`, biased (Stan's estimator) */
  private def autocorrelation(xs: IndexedSeq[Double], m: Double, v: Double, t: Int): Double =
    var s = 0.0
    var i = 0
    while i + t < xs.length do { s += (xs(i) - m) * (xs(i + t) - m); i += 1 }
    s / (xs.length * v)

  /** EFFECTIVE SAMPLE SIZE: n / (1 + 2 Σ ρ_t), the sum cut by Geyer's
   * initial positive sequence (pairs ρ_2k + ρ_2k+1 summed while positive) */
  def ess(xs: Seq[Double]): Double =
    val v0 = xs.toIndexedSeq
    val n = v0.length
    val m = mean(v0)
    val v = v0.iterator.map(x => (x - m) * (x - m)).sum / n
    if v == 0.0 then n.toDouble
    else
      var sum = 0.0
      var t = 1
      var go = true
      while go && t + 1 < n do
        val pair = autocorrelation(v0, m, v, t) + autocorrelation(v0, m, v, t + 1)
        if pair > 0 then { sum += pair; t += 2 } else go = false
      n / (1 + 2 * sum)

  /** SPLIT R-HAT (Gelman et al., BDA3 §11.4): each chain halved, between-
   * against within-chain variance; about 1.0 when the chains agree */
  def rhat(chains: Seq[Seq[Double]]): Double =
    val halves = chains.flatMap { c =>
      val h = c.length / 2
      Seq(c.take(h), c.slice(h, 2 * h))
    }.filter(_.length > 1)
    val n = halves.map(_.length).min
    val parts = halves.map(_.take(n))
    val means = parts.map(mean)
    val grand = mean(means)
    val b = n * means.iterator.map(x => (x - grand) * (x - grand)).sum / (parts.length - 1)
    val w = mean(parts.map(p => { val s = sd(p); s * s }))
    val varPlus = (n - 1.0) / n * w + b / n
    math.sqrt(varPlus / w)
