package okay

/**
 * How a FORWARDED operation reports its failure to the scope that
 * forwarded it (resource-guard, 2026-09-09).
 *
 * `Resource.run` hands every non-Resource operation to the outer
 * handler and keeps the scope's finalizers in the residual; when that
 * operation fails OUT THERE, the residual — finalizers included — is
 * abandoned, and a transaction region's brake never runs (found
 * through pgjdbc: a throwing statement left the connection in the
 * transaction). Only `Async` operations can fail on the outer handler
 * that way — a `Run` thunk throws, an `Await` callback answers Left —
 * so this typeclass says, per row, how to attach a hook that runs
 * first.
 *
 * Typed all the way: `Async`'s instance is a GADT match on the
 * operation, the row instances split by Async's OWN `TypeableK`
 * through the kernel `<|>` (the one place a row is ever cast) and
 * come back by plain upcast, and a row without Async gets the
 * identity from the low-priority companion. `Async` is anchored on
 * either side of `+`, so `Async + G`, `F + Async` and the deeper rows
 * `(Async + F) + G` / `(F + G) + Async` all resolve; the probe that
 * showed the unanchored `F + G` instance cannot pin `F` is in the
 * lane's record (specs/sql.md).
 */
trait Failing[F[+_]]:
  /** the same operation, with `onFailure` run before its failure
   * reaches whoever handles it; an operation that cannot fail on the
   * outer handler is returned as it is */
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

trait FailingLow:
  /** a row without Async: nothing to guard */
  given plain[F[+_]]: Failing[F] = new:
    def guard[X](e: F[X], onFailure: () => Unit): F[X] = e

object Failing extends FailingLow:
  given async: Failing[Async] = new:
    def guard[X](e: Async[X], onFailure: () => Unit): Async[X] = e match
      case Async.Run(f) => Async.Run(() => try f() catch { case t: Throwable => onFailure(); throw t })
      case Async.Await(reg) => Async.Await(k => reg { r => if r.isLeft then onFailure(); k(r) })

  given asyncLeft[G[+_]](using g: Failing[G]): Failing[Async + G] = new:
    def guard[X](e: Async[X] | G[X], onFailure: () => Unit): Async[X] | G[X] =
      <|>[Async, G](e) match
        case Left(a) => async.guard(a, onFailure)
        case Right(x) => g.guard(x, onFailure)

  given asyncRight[F[+_]](using f: Failing[F]): Failing[F + Async] = new:
    def guard[X](e: F[X] | Async[X], onFailure: () => Unit): F[X] | Async[X] =
      <|>[Async, F](e) match
        case Left(a) => async.guard(a, onFailure)
        case Right(x) => f.guard(x, onFailure)
