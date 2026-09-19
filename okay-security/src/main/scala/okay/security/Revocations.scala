package okay.security

import okay.{!, Async, Scheduler}

/**
 * SOMEBODY ELSE'S LIST OF WHAT IS OFF (specs/security.md stage 7).
 *
 * `Capability.checking` and `McpAuth.capabilities` take `revoked` as a
 * PREDICATE rather than a set, precisely so the list can live
 * anywhere — a constant, a column, or a service whose whole business
 * is keeping it. This class is the last shape: a local snapshot of a
 * remote list, refreshed by a program the caller runs, read
 * synchronously by the door.
 *
 * NOTHING HERE CHANGES THE CHECK. The capability's own question is
 * unchanged and the door is unchanged; what this adds is the two
 * things an EXTERNAL list brings with it, which a bare
 * `String => Boolean` would let a caller forget.
 *
 * **The dangerous failure is not "the registry is down".** It is "the
 * registry answered empty": a source that turns an error into
 * `Set.empty` un-revokes everyone the instant it breaks, silently,
 * and at precisely the worst moment. So `Source` answers an `Either`
 * with a named failure, a failed refresh KEEPS the list it had, and
 * no path in this class shrinks a list by accident.
 *
 * **Staleness is a decision, so there is no default.** A remote list
 * is only as good as its last successful fetch. After `freshFor`
 * milliseconds the snapshot stops being evidence, and `whileStale`
 * says what that means for this door: `Allow` where the list is
 * advisory (a kill switch that lags beats a door that jams), `Deny`
 * where it is load-bearing. BEFORE THE FIRST SUCCESSFUL FETCH the
 * same rule applies, which is why `Deny` refuses everything until one
 * arrives — that is fail-closed meaning what it says.
 *
 * **The hot path does not go to the network.** A capability is
 * checked per tool, and `tools/list` checks every tool, so a remote
 * call inside the predicate is one round trip per tool per request.
 * Reading is a field read; refreshing is a separate program on
 * whatever schedule the caller already has (a fiber, a timer, a
 * stream of updates pushed to `took`).
 */
final class Revocations(freshFor: Long, whileStale: Revocations.Stale):

  import Revocations.Stale

  @volatile private var held: Set[String] = Set.empty
  @volatile private var at: Long = 0L
  @volatile private var ever: Boolean = false
  @volatile private var why: Option[String] = None

  /** the predicate the door takes. `now` is a function because the
   * door asks once per request and applies the answer per tool */
  def revoked(now: () => Long): String => Boolean = id =>
    if !ever then whileStale == Stale.Deny
    else if now() - at <= freshFor then held.contains(id)
    else whileStale match
      case Stale.Allow => held.contains(id)   // the last answer still serves
      case Stale.Deny => true                 // no evidence is not evidence of none

  /** a successful answer: the list, and when it arrived */
  def took(ids: Set[String], at: Long): Unit =
    held = ids; this.at = at; ever = true; why = None

  /** a failed attempt. THE LIST IS KEPT — this is the whole reason
   * `Source` answers an Either instead of a set */
  def missed(why: String): Unit = this.why = Some(why)

  /**
   * One pass against a source. A source that ANSWERS a failure and a
   * source that THROWS are the same thing here: named, recorded, and
   * the previous list untouched.
   */
  def refresh(source: Revocations.Source)(now: () => Long)
             (using Scheduler): Unit ! Async =
    Async.attempt(source()).map {
      case Right(Right(ids)) => took(ids, now())
      case Right(Left(named)) => missed(named)
      case Left(thrown) => missed(Option(thrown.getMessage).getOrElse(thrown.toString))
    }

  /** how old the last successful answer is, or None if there has
   * never been one */
  def age(now: Long): Option[Long] = if ever then Some(now - at) else None

  /** why the last attempt failed, if the last attempt failed */
  def failure: Option[String] = why

  /** what is held right now — for an operator's report, not for a
   * decision: the decision is `revoked`, which also weighs staleness */
  def listed: Set[String] = held

object Revocations:

  /**
   * An external registry, as this library needs it: an answer, or a
   * NAMED FAILURE. Never an empty set standing in for "I could not
   * ask" — see the class comment for why that one matters most.
   */
  type Source = () => Either[String, Set[String]] ! Async

  /** what the door does while the snapshot is stale, or before the
   * first answer has ever arrived */
  enum Stale:
    /** the list is advisory: keep serving the last answer */
    case Allow
    /** the list is load-bearing: refuse rather than guess */
    case Deny
