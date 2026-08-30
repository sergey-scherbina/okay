package okay

/**
 * Retry policies ARE streams of delays (milliseconds): the stream
 * algebra is the policy algebra — take limits the attempts, map
 * scales, ++ chains phases, zip mixes. Consumed by retry, supervised
 * and retryChunks (specs/parallel-resilience.md).
 */
object Retry {

  /** the same delay every time */
  def constant(ms: Long): LazyList[Long] = LazyList.continually(ms)

  /** n immediate retries */
  def immediate(n: Int): LazyList[Long] = constant(0).take(n)

  /** exponential backoff, capped */
  def exponential(base: Long, factor: Double = 2.0,
                  cap: Long = Long.MaxValue): LazyList[Long] =
    LazyList.iterate(base.toDouble)(_ * factor).map(d => math.min(d.toLong, cap))

  /** multiply each delay by a deterministic factor in [0.5, 1.5) */
  def jittered(policy: LazyList[Long], seed: Long = 42): LazyList[Long] =
    val rs = LazyList.iterate(seed)(x => x * 6364136223846793005L + 1442695040888963407L).tail
    policy.lazyZip(rs).map((d, r) => (d * (0.5 + math.floorMod(r, 1000) / 1000.0)).toLong)
}
