package okay.resilience

import okay.*
import okay.http.{Http, Request, Response}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/**
 * The deterministic adversary (specs/resilience.md, stage 2): an
 * `Http` that delays, drops or fails calls by a PLAN, each call's
 * fate drawn from the seed and its ordinal — so two runs with one
 * seed meet the same faults in the same places, whatever the
 * scheduler does in between (a hedged second attempt is ordinal 2
 * even when it finishes first). A found bug is a seed; a fix is
 * verified by replaying it. Sim's move, one seam up.
 */
object Faults:

  /** what one call met */
  enum Fault:
    case None
    /** the wire took this long before answering */
    case Slow(millis: Long)
    /** the wire broke: a thrown error, nothing answered */
    case Drop
    /** the far end answered this status */
    case Status(code: Int)

  /**
   * By ORDINAL (1-based, fixed) and by RATE (drawn from the seed).
   * A fixed fault wins over a drawn one; a call meets one fault at
   * most, drop before status before slow.
   */
  final case class Plan(dropAt: Set[Long] = Set.empty,
                        failAt: Map[Long, Int] = Map.empty,
                        slowAt: Map[Long, Long] = Map.empty,
                        dropRate: Double = 0.0,
                        failRate: Double = 0.0, failStatus: Int = 503,
                        slowRate: Double = 0.0, slowMillis: Long = 0L)

  final case class Stats(calls: Long, dropped: Long, failed: Long, slowed: Long)

  /** SplitMix64 on (seed, ordinal): a call's draw depends on nothing
    * but those two, which is what makes a hedged race replayable */
  private def draw(seed: Long, n: Long): Double =
    var z = seed + n * 0x9E3779B97F4A7C15L
    z = (z ^ (z >>> 30)) * 0xBF58476D1CE4E5B9L
    z = (z ^ (z >>> 27)) * 0x94D049BB133111EBL
    z = z ^ (z >>> 31)
    (z >>> 11).toDouble / (1L << 53).toDouble

  /** the fate of call `n` under `plan` with `seed` — a pure function */
  def fate(seed: Long, plan: Plan)(n: Long): Fault =
    if plan.dropAt(n) then Fault.Drop
    else plan.failAt.get(n) match
      case Some(code) => Fault.Status(code)
      case None => plan.slowAt.get(n) match
        case Some(ms) => Fault.Slow(ms)
        case None =>
          val r = draw(seed, n)
          if r < plan.dropRate then Fault.Drop
          else if r < plan.dropRate + plan.failRate then Fault.Status(plan.failStatus)
          else if r < plan.dropRate + plan.failRate + plan.slowRate then Fault.Slow(plan.slowMillis)
          else Fault.None

  final class Dropped(val ordinal: Long) extends RuntimeException(s"wire dropped call $ordinal", null, false, false)

  /** the adversary around `inner`; `log` says what each call met */
  final class Injected(seed: Long, plan: Plan, inner: Http)(using Timer) extends Http:
    private val ordinal = AtomicLong(0)
    private val met = AtomicReference(Vector.empty[(Long, Fault)])

    def log: Vector[(Long, Fault)] = met.get.sortBy(_._1)
    def stats: Stats =
      val l = log.map(_._2)
      Stats(l.size.toLong, l.count(_ == Fault.Drop).toLong,
        l.count(_.isInstanceOf[Fault.Status]).toLong, l.count(_.isInstanceOf[Fault.Slow]).toLong)

    def send(r: Request): Response ! Async =
      okay.async {
        val n = ordinal.incrementAndGet()
        val f = fate(seed, plan)(n)
        met.updateAndGet(_ :+ (n, f))
        (n, f)
      }.flatMap { (n, f) =>
        f match
          case Fault.None => inner.send(r)
          case Fault.Slow(ms) => Async.sleep(ms).flatMap(_ => inner.send(r))
          case Fault.Drop => throw Dropped(n)
          case Fault.Status(code) =>
            pure(Response(code, Seq(("content-type", "text/plain")), Http.one(s"injected $code".getBytes("UTF-8"))))
      }

  def http(seed: Long, plan: Plan)(inner: Http)(using Timer): Injected = Injected(seed, plan, inner)
