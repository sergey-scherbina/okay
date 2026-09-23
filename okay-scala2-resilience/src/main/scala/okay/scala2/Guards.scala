package okay.scala2

import okay.given
import okay.resilience.{Breaker, Bulkhead, Deadline, Hedge, Limiter}

/**
 * okay-resilience for Scala 2.13 (specs/scala2-facade.md, stage 15.1).
 *
 * Probed first: the pieces themselves are readable from scalac 2.13, and
 * a Scala 2 caller builds them with okay-resilience's own constructors:
 * `new Breaker(name, failures, openMillis)`, `new Bulkhead(...)`,
 * `new Limiter(...)`, `Deadline.in(millis)`; a refusal is okay's
 * `Refused.*`; retry policies are `okay.Retry.constant/exponential/...`.
 * What 2.13 cannot use is the one thing each piece DOES: transform a
 * program, `A ! Async => A ! Async`. So this object is those
 * transformations over `Eff[Async, A]`, each one call into the piece.
 *
 * Named `Guards`, not `Resilient`, because a Scala 2 file that imports
 * both `okay.resilience._` and `okay.scala2._` would find two
 * `Resilient`s and refuse the name as ambiguous.
 */
object Guards {

  /** refuse while the breaker is open (`Refused.BreakerOpen`); record the
   * outcome, where `failing` says what counts as a failure (a thrown
   * exception by default, a 5xx if you say so) */
  def breaker[A](b: Breaker)(prog: Eff[Async, A],
                             failing: Either[Throwable, A] => Boolean = (r: Either[Throwable, A]) => r.isLeft): Eff[Async, A] =
    Async.lift(b.protect(Async.core(prog))(failing))

  /** at most the bulkhead's permits in flight, its queue parked, the
   * rest refused at once (`Refused.BulkheadFull`) */
  def bulkhead[A](b: Bulkhead)(prog: Eff[Async, A]): Eff[Async, A] =
    Async.lift(b.limit(Async.core(prog)))

  /** a token per call from `key`'s bucket, or `Refused.Exhausted` with
   * the wait */
  def limiter[A](l: Limiter, key: String = "")(prog: Eff[Async, A]): Eff[Async, A] =
    Async.lift(l.admit(key)(Async.core(prog)))

  /** a slow attempt is joined by another after `afterMillis`, up to `max`
   * in flight; the first success answers and the rest are cancelled.
   * Only for operations that are safe to repeat. */
  def hedge[A](afterMillis: Long, max: Int = 2)(prog: Eff[Async, A]): Eff[Async, A] =
    Async.lift(Hedge.run(afterMillis, max)(Async.core(prog)))

  /** refuse an expired budget before running, and cancel a run that
   * outlives it (`Refused.DeadlineExceeded`) */
  def deadline[A](d: Deadline)(prog: Eff[Async, A]): Eff[Async, A] =
    Async.lift(Deadline.enforce(d)(Async.core(prog)))

  /** run `prog` again after each failure, waiting each delay of `policy`
   * in turn (`okay.Retry.constant`, `exponential`, `jittered`, ...);
   * when the policy runs out, the last failure is the answer */
  def retry[A](policy: LazyList[Long])(prog: Eff[Async, A]): Eff[Async, A] =
    Async.lift(okay.Retry.async(policy)(Async.core(prog)))
}
