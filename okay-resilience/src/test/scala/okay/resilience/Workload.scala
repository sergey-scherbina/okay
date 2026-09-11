package okay.resilience

import scala.collection.mutable

/**
 * A concurrency limit, as the thing under test (adaptive-concurrency).
 *
 * What is measured here is the POLICY — how many calls to allow in
 * flight — and not `Bulkhead`'s parking, which has its own tests.
 * Keeping them apart is the point: a control loop judged through a
 * scheduler would be judged partly by the scheduler.
 */
trait Limit:
  def permits: Int
  /** one completed call: how long it took, and how many were in
    * flight when it finished */
  def observe(latencyMillis: Long, inFlight: Int): Unit

/** the permit count a person picked and nobody changes */
final class Fixed(val permits: Int) extends Limit:
  def observe(latencyMillis: Long, inFlight: Int): Unit = ()

/**
 * The gradient controller, in the shape Netflix's concurrency-limits
 * made standard, written small and honestly:
 *
 *   gradient   = the best latency ever seen / the latency seen lately
 *   new limit  = limit * gradient + sqrt(limit)
 *
 * The reading: when recent latency matches the best latency the
 * downstream ever gave, the gradient is 1 and the limit grows by its
 * own square root — probing for more. When latency has doubled, the
 * gradient is 0.5 and the limit is cut towards half. The square-root
 * term is the queue this deliberately allows: without it the limit
 * can never grow, because growth is what creates the queue that
 * proves there is room.
 *
 * Simplified from the original in two ways, both stated rather than
 * hidden: the long-term latency is a running minimum with a decay
 * instead of a windowed percentile, and there is no separate
 * exponential back-off on timeouts (the breaker is that, one layer
 * out).
 */
final class Gradient(min: Int = 1, max: Int = 200, start: Int = 10,
                     smoothing: Double = 0.2, decayEvery: Int = 200) extends Limit:
  private var limit: Double = start.toDouble
  private var best: Long = Long.MaxValue
  private var recent: Double = 0.0
  private var samples: Long = 0

  def permits: Int = math.max(min, math.min(max, limit.round.toInt))

  def observe(latencyMillis: Long, inFlight: Int): Unit =
    samples += 1
    if latencyMillis < best then best = latencyMillis
    // the recent latency, smoothed — one slow call must not halve the limit
    recent = if recent == 0.0 then latencyMillis.toDouble else recent * 0.8 + latencyMillis * 0.2
    // let the best decay back up, or a downstream that got permanently
    // slower would be measured against a past it can never reach again
    if samples % decayEvery == 0 then best = (best * 1.05).toLong.max(best + 1)

    // Only move the limit on a sample that actually EXERCISED it.
    // The first cut said `inFlight >= limit * 0.5`, which lets the
    // limit run to twice the concurrency ever observed: measured, it
    // climbed 10 -> 51 while latency was still flat, then collapsed to
    // 6 when the arrivals caught up. The condition has to be "at the
    // limit", not "half of it" — sqrt(limit) is the same slack the
    // growth term uses.
    if inFlight >= limit - math.sqrt(limit) then
      val gradient = math.max(0.5, math.min(1.0, best.toDouble / math.max(1.0, recent)))
      val next = limit * gradient + math.sqrt(limit)
      limit = limit * (1 - smoothing) + next * smoothing

/**
 * The downstream, simulated in VIRTUAL time.
 *
 * The performance skill's first rule is that a number measured on a
 * busy machine is noise wearing a result's clothes, and a control
 * loop judged by wall-clock throughput on a shared laptop would be
 * judged by the laptop. So nothing here touches a real clock or a
 * real thread: two runs of one configuration are the same run.
 *
 * The service modelled is the one every concurrency limit exists
 * for — latency that RISES with the calls in flight, because a queue
 * forms somewhere. Up to `capacity` concurrent calls each take
 * `baseMillis`; beyond it every extra call in flight adds
 * `queueMillis` to everyone. That is why more concurrency stops
 * buying throughput and starts buying only latency.
 */
object Workload:

  final case class Result(admitted: Long, rejected: Long, virtualMillis: Long,
                          p50: Long, p99: Long, throughputPerSecond: Double,
                          finalPermits: Int):
    def show: String =
      f"ok $admitted%5d  refused $rejected%5d  p50 $p50%5d  p99 $p99%6d  ${throughputPerSecond}%6.1f/s  limit $finalPermits%3d"

  /**
   * `arrivals` calls offered one every `everyMillis`; `capacityAt`
   * says how many the downstream can serve at once AT A GIVEN TIME —
   * a function, so a run can halve it half way and see which policy
   * follows.
   */
  def run(limit: Limit, arrivals: Int, everyMillis: Long,
          baseMillis: Long, queueMillis: Long,
          capacityAt: Long => Int): Result =
    var clock = 0L
    var inFlight = 0
    var admitted = 0L
    var rejected = 0L
    val latencies = mutable.ArrayBuffer.empty[Long]
    // (finishes at, started at)
    // earliest finish first: a max-heap on the negated instant
    given Ordering[(Long, Long)] = Ordering.by((e: (Long, Long)) => -e._1)
    val running = mutable.PriorityQueue.empty[(Long, Long)]
    var nextArrival = 0L
    var offered = 0

    def serviceTime(at: Long): Long =
      val over = math.max(0, inFlight + 1 - capacityAt(at))
      baseMillis + over * queueMillis

    while offered < arrivals || running.nonEmpty do
      val arrivalDue = if offered < arrivals then Some(nextArrival) else None
      val finishDue = running.headOption.map(_._1)
      // the earliest event wins; a tie completes first, so a permit
      // freed at the same instant is available to the arrival
      val takeFinish = (arrivalDue, finishDue) match
        case (Some(a), Some(f)) => f <= a
        case (None, Some(_)) => true
        case _ => false

      if takeFinish then
        val (at, startedAt) = running.dequeue()
        clock = at
        inFlight -= 1
        val took = at - startedAt
        latencies += took
        limit.observe(took, inFlight + 1)
      else
        clock = nextArrival
        offered += 1
        nextArrival += everyMillis
        if inFlight < limit.permits then
          admitted += 1
          running.enqueue((clock + serviceTime(clock), clock))
          inFlight += 1
        else rejected += 1

    val sorted = latencies.toVector.sorted
    def q(p: Double): Long =
      if sorted.isEmpty then 0L else sorted(math.min(sorted.size - 1, (sorted.size * p).toInt))
    Result(admitted, rejected, clock, q(0.5), q(0.99),
      if clock == 0 then 0.0 else admitted * 1000.0 / clock, limit.permits)
