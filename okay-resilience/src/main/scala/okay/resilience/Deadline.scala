package okay.resilience

import okay.*
import okay.http.Request

/**
 * A deadline is an absolute instant on THIS node's clock; what
 * travels is the REMAINING budget, recomputed at every hop — gRPC's
 * model (`grpc-timeout`), which needs no clock agreement between
 * services and loses only the transit time. The header is ours
 * because REST has no standard one; the semantics are gRPC's so a
 * gateway can translate.
 */
final case class Deadline(atMillis: Long):
  def remaining(now: Long): Long = atMillis - now

object Deadline:
  val header = "x-deadline-ms"

  private def wall(): Long = System.currentTimeMillis

  def in(millis: Long, clock: () => Long = wall): Deadline = Deadline(clock() + millis)

  /** the program within the budget, or `Refused.DeadlineExceeded`:
    * before starting when the budget is already gone, by cancellation
    * when it runs out mid-way */
  def enforce[A](d: Deadline, clock: () => Long = wall)(prog: => A ! Async)
                (using S: Scheduler, T: Timer): A ! Async =
    okay.async(d.remaining(clock())).flatMap { left =>
      if left <= 0 then throw Refused.DeadlineExceeded(left)
      else Async.await[A] { k =>
        // not `Async.timeout`: that is a race, and a race waits for
        // the other contender when one FAILS — a refusal under a
        // deadline would wait the whole budget to come out as a
        // timeout. Here the first outcome of either kind settles it.
        val done = java.util.concurrent.atomic.AtomicBoolean(false)
        val f = S.fork(() => prog)
        val cancelTimer = T.after(left) { () =>
          if !done.getAndSet(true) then
            f.cancel()
            k(Left(Refused.DeadlineExceeded(d.remaining(clock()))))
        }
        f.onComplete { r =>
          if !done.getAndSet(true) then
            cancelTimer()
            k(r)
        }
        () => { if !done.getAndSet(true) then { cancelTimer(); f.cancel() } }
      }
    }

  /** the header, as a local deadline; damaged or negative reads as absent */
  def read(r: Request, clock: () => Long = wall): Option[Deadline] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(header) => v.trim }
      .flatMap(v => v.toLongOption.filter(_ >= 0))
      .map(ms => Deadline(clock() + ms))

  /** the request carrying the REMAINING budget (never below zero);
    * an earlier deadline header is replaced */
  def carry(r: Request, d: Deadline, clock: () => Long = wall): Request =
    val left = math.max(0L, d.remaining(clock()))
    r.copy(headers = r.headers.filterNot(_._1.equalsIgnoreCase(header)) :+ (header, left.toString))
