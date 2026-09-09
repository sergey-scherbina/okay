package okay.resilience

import okay.{Async, !}
import okay.!.{Effect, resume}
import okay.Free.{Bind, Pure}

/**
 * Five handlers around one operation (specs/resilience.md): a
 * circuit breaker, a bulkhead, a keyed token-bucket limiter, hedged
 * requests and a travelling deadline. The program stays blind to
 * them — `http.send(r)` is what it says — and the edge composes them
 * around the seam, the way `Tracer.traced` wraps a Handler and
 * `Secure.bearer` wraps a route.
 *
 * Shared by all five: state is ONE cell moved by one `modify`; time
 * is an injected clock; a refusal is a `Refused` — a named exception
 * that says which piece refused and how long to wait when it knows.
 */

/** a refusal by one of the pieces: local, before or instead of the
  * operation — never the far end's answer, which stays data */
sealed abstract class Refused(msg: String) extends RuntimeException(msg, null, false, false):
  /** how long a caller should wait, when the refuser knows */
  def retryAfterMillis: Option[Long]

object Refused:
  final class BreakerOpen(val name: String, val retryAfterMillis: Option[Long])
    extends Refused(s"breaker '$name' is open")
  final class BulkheadFull(val name: String)
    extends Refused(s"bulkhead '$name' is full"):
    def retryAfterMillis: Option[Long] = None
  final class Exhausted(val name: String, val key: String, val retryAfterMillis: Option[Long])
    extends Refused(s"limiter '$name' exhausted for '$key'")
  final class DeadlineExceeded(val remainingMillis: Long)
    extends Refused(s"deadline exceeded by ${-remainingMillis} ms"):
    def retryAfterMillis: Option[Long] = None

/** every piece reports itself as a value with a Schema, so a metric
  * and a span attribute need no second definition */
trait Reporting[S]:
  def name: String
  def stats: S

/**
 * Observe how a closed Async program ends, as a value, on the fiber
 * it is already on — no spawn, no Scheduler. The walk is the one
 * `Async.Drive` makes: a `Run` is re-issued with its thunk guarded,
 * an `Await` with its callback's `Left` turned into an answer. It
 * costs one extra node per operation of the guarded program, which
 * is the price of not paying a fiber per call. Public because
 * okay-ops's `Lifecycle` and `Red` need the same observation
 * (service-lifecycle); it is a building block, not a policy.
 */
object Attempt:
  def apply[A](p: A ! Async): Either[Throwable, A] ! Async =
    // `resume` runs continuations (a `flatMap` body after `Pure`), and
    // a continuation may throw — the breaker's own refusal does. That
    // is a failure of the program like any other.
    val head = try Right(p.resume) catch case t: Throwable => Left(t)
    head match
      case Left(t) => Pure(Left(t))
      case Right(h) => (h: @unchecked) match
        case Pure(a) => Pure(Right(a))
        case Effect(e) => step(e, (x: A) => Pure(x))
        case Bind(Effect(e), k) => step(e, k)

  /** the continuation applied, its own throw made an answer */
  private def continue[X, A](k: X => A ! Async, x: X): Either[Throwable, A] ! Async =
    val next = try Right(k(x)) catch case t: Throwable => Left(t)
    next match
      case Right(p) => apply(p)
      case Left(t) => Pure(Left(t))

  // `k` is the continuation the GADT match typed at X
  private def step[X, A](e: Async[X], k: X => A ! Async): Either[Throwable, A] ! Async = e match
    case Async.Run(f) =>
      okay.effect[Async, Either[Throwable, X]](Async.Run(() => try Right(f()) catch case t: Throwable => Left(t)))
        .flatMap {
          case Right(x) => continue(k, x)
          case Left(t) => Pure(Left(t))
        }
    case Async.Await(reg) =>
      okay.effect[Async, Either[Throwable, X]](Async.Await(cb => reg(r => cb(Right(r)))))
        .flatMap {
          case Right(x) => continue(k, x)
          case Left(t) => Pure(Left(t))
        }
