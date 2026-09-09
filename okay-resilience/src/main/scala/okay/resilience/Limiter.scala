package okay.resilience

import okay.*
import okay.codec.Schema

object Limiter:
  final case class Stats(keys: Int, admitted: Long, delayed: Long,
                         rejected: Long) derives Schema

  private final case class Bucket(tokens: Double, at: Long)
  private final case class St(buckets: Map[String, Bucket], calls: Long,
                              admitted: Long, delayed: Long, rejected: Long)

  /** how often the map is swept for full buckets */
  private val sweepEvery = 64

/**
 * A token bucket per key: `burst` tokens at most, refilled at
 * `ratePerSecond`. A call takes a token; with none left it is
 * refused with `Refused.Exhausted` naming the wait, or — when the
 * wait is within `maxWaitMillis` — it takes the token in advance and
 * PARKS for it (the bucket goes briefly negative, which is what
 * reserves the caller's place). A full bucket is indistinguishable
 * from an absent one, so full buckets are evicted on every
 * `sweepEvery`th call and the map follows activity, not history.
 */
final class Limiter(val name: String, ratePerSecond: Double, burst: Int,
                    maxWaitMillis: Long = 0,
                    clock: () => Long = () => System.currentTimeMillis)
  extends Reporting[Limiter.Stats]:
  import Limiter.*

  require(ratePerSecond > 0, "a limiter needs a positive rate")
  require(burst >= 1, "a limiter needs a burst of at least one")

  private val cell = TRef(St(Map.empty, 0L, 0L, 0L, 0L))

  def stats: Stats =
    val s = cell.get
    Stats(s.buckets.size, s.admitted, s.delayed, s.rejected)

  /** the operation after its token — now, after a park, or never */
  def admit[A](key: String = "")(prog: => A ! Async)(using Timer): A ! Async =
    okay.async(take(key)).flatMap {
      case Left(wait) => throw Refused.Exhausted(name, key, Some(wait))
      case Right(0L) => prog
      case Right(wait) => Async.sleep(wait).flatMap(_ => prog)
    }

  /** Right(wait to park): admitted; Left(wait): refused */
  private def take(key: String): Either[Long, Long] =
    val now = clock()
    cell.modify { s =>
      val b = s.buckets.get(key) match
        case Some(Bucket(t, at)) => Bucket(math.min(burst.toDouble, t + (now - at) / 1000.0 * ratePerSecond), now)
        case None => Bucket(burst.toDouble, now)
      val calls = s.calls + 1
      val taken: (Map[String, Bucket], Either[Long, Long]) =
        if b.tokens >= 1 then (s.buckets.updated(key, b.copy(tokens = b.tokens - 1)), Right(0L))
        else
          val wait = math.ceil((1 - b.tokens) / ratePerSecond * 1000).toLong
          if wait <= maxWaitMillis then (s.buckets.updated(key, b.copy(tokens = b.tokens - 1)), Right(wait))
          else (s.buckets.updated(key, b), Left(wait))
      val (buckets, outcome) = taken
      // full AS OF NOW: a bucket is only refilled when its key is
      // touched, so the stored count is stale and the sweep refills
      def full(v: Bucket): Boolean = v.tokens + (now - v.at) / 1000.0 * ratePerSecond >= burst
      val swept =
        if calls % sweepEvery == 0 then buckets.filterNot((k, v) => k != key && full(v))
        else buckets
      val next = outcome match
        case Right(0L) => s.copy(buckets = swept, calls = calls, admitted = s.admitted + 1)
        case Right(_) => s.copy(buckets = swept, calls = calls, admitted = s.admitted + 1, delayed = s.delayed + 1)
        case Left(_) => s.copy(buckets = swept, calls = calls, rejected = s.rejected + 1)
      (next, outcome)
    }
