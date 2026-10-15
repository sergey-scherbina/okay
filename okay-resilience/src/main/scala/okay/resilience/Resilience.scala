package okay.resilience

import okay.{!, Async, defer, pure}

/**
 * Five handlers around one operation (specs/resilience.md): a
 * circuit breaker, a bulkhead, a keyed token-bucket limiter, hedged
 * requests and a travelling deadline. The program stays blind to
 * them — `http.send(r)` is what it says — and the edge composes them
 * around the seam, the way `Tracer.traced` wraps an Answers and
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
  final class NoEndpoint(val service: String)
    extends Refused(s"no endpoint for service '$service'"):
    def retryAfterMillis: Option[Long] = None

/** every piece reports itself as a value with a Schema, so a metric
  * and a span attribute need no second definition */
trait Reporting[S]:
  def name: String
  def stats: S

/**
 * Observe how a closed Async program ends, as a value, on the fiber
 * it is already on — no spawn, no Scheduler. No walk: the root's
 * guard (`recover`) sees every way the program fails where it fails —
 * a throwing `Run`, an `Await` whose callback answered `Left` (the
 * drive fails the rest at that operation), a continuation that throws
 * (the breaker's own refusal does), and building the program itself
 * (`prog` is by name, deferred inside the guard). Public because
 * okay-ops's `Lifecycle` and `Red` need the same observation
 * (service-lifecycle); it is a building block, not a policy.
 */
object Attempt:
  def apply[A](p: => A ! Async): Either[Throwable, A] ! Async = in[A, Async](p)

  /**
   * The same observation over ANY row: every operation stays where it
   * is, performed by whoever handles it, and the answer is how the
   * whole program ended. A streaming seam needs this — `okay.llm.Transport`
   * posts and tells its response lines, so its program is
   * `Unit ! Writer % String + Async`; the guard spans the WHOLE stream
   * and not merely its first line (resilient-transport).
   */
  def in[A, Es[_]](p: => A ! Es): Either[Throwable, A] ! Es =
    defer(p).map(a => Right(a): Either[Throwable, A]).recover(t => pure[Either[Throwable, A], Es](Left(t)))
