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

  /**
   * `retry` as an Async PROGRAM, every platform (retry-js,
   * 2026-09-09): the attempt's failure arrives as data
   * (`Async.attempt`), the delay is an `Async.sleep` on the platform
   * timer — nothing parks a thread, so it runs under Node exactly as on
   * the JVM. Same contract as the blocking `retry`: the program reruns
   * FROM ITS BEGINNING on any exception (at-least-once, for replayable
   * work), a delay of 0 sleeps nothing, and a policy exhausted fails
   * with the LAST error.
   */
  def async[A](policy: LazyList[Long])(prog: => A ! Async)(using Scheduler, Timer): A ! Async =
    def go(delays: LazyList[Long]): A ! Async =
      Async.attempt(prog).flatMap:
        case Right(a) => pure(a)
        case Left(e) => delays match
          case d #:: rest =>
            if d > 0 then Async.sleep(d).flatMap(_ => go(rest)) else go(rest)
          case _ => okay.async(throw e)
    go(policy)
}
