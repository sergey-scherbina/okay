package okay.resilience

import okay.*
import okay.codec.Schema

object Breaker:
  enum State derives Schema:
    case Closed, Open, HalfOpen

  final case class Stats(state: State, consecutiveFailures: Int,
                         calls: Long, failures: Long, rejected: Long,
                         opened: Long) derives Schema

  /** the whole state, one value in one cell */
  private final case class St(state: State, consecutive: Int, openedAt: Long,
                              probing: Boolean, calls: Long, failures: Long,
                              rejected: Long, opened: Long)

/**
 * The circuit breaker: `failures` CONSECUTIVE failures open it; while
 * open the operation is not run and the refusal names the remaining
 * open time; after `openMillis` ONE probe runs (half-open) — its
 * success closes the circuit, its failure re-opens it for a fresh
 * `openMillis`. What a failure IS is the caller's predicate: an
 * exception by default, and a returned value can be one too (a 5xx).
 */
final class Breaker(val name: String, failures: Int, openMillis: Long,
                    clock: () => Long = () => System.currentTimeMillis)
  extends Reporting[Breaker.Stats]:
  import Breaker.*

  require(failures >= 1, "a breaker needs at least one failure to trip on")

  private val cell = TRef(St(State.Closed, 0, 0L, false, 0L, 0L, 0L, 0L))

  def stats: Stats =
    val s = cell.get
    Stats(s.state, s.consecutive, s.calls, s.failures, s.rejected, s.opened)

  /** the operation, or `Refused.BreakerOpen` without running it */
  def protect[A](prog: => A ! Async)
                (failing: Either[Throwable, A] => Boolean = (r: Either[Throwable, A]) => r.isLeft): A ! Async =
    okay.async(admit()).flatMap {
      case Some(wait) => throw Refused.BreakerOpen(name, wait)
      case None =>
        Attempt(prog).map { r =>
          record(failing(r))
          r.fold(t => throw t, identity)
        }
    }

  /** None: go; Some(wait): refused, with the remaining open time when
    * it is known (a probe already in flight has none to promise) */
  private def admit(): Option[Option[Long]] =
    val now = clock()
    cell.modify { s =>
      s.state match
        case State.Closed => (s.copy(calls = s.calls + 1), None)
        case State.Open =>
          val left = s.openedAt + openMillis - now
          if left <= 0 then (s.copy(state = State.HalfOpen, probing = true, calls = s.calls + 1), None)
          else (s.copy(rejected = s.rejected + 1), Some(Some(left)))
        case State.HalfOpen =>
          if s.probing then (s.copy(rejected = s.rejected + 1), Some(None))
          else (s.copy(probing = true, calls = s.calls + 1), None)
    }

  private def record(failed: Boolean): Unit =
    val now = clock()
    cell.modify { s =>
      val counted = if failed then s.copy(failures = s.failures + 1) else s
      val next = s.state match
        case State.Closed =>
          if !failed then counted.copy(consecutive = 0)
          else if s.consecutive + 1 >= failures then
            counted.copy(state = State.Open, consecutive = s.consecutive + 1, openedAt = now, opened = s.opened + 1)
          else counted.copy(consecutive = s.consecutive + 1)
        case State.HalfOpen =>
          if failed then counted.copy(state = State.Open, openedAt = now, probing = false, opened = s.opened + 1)
          else counted.copy(state = State.Closed, consecutive = 0, probing = false)
        case State.Open => counted   // a call admitted before the trip: counted, no transition
      (next, ())
    }
