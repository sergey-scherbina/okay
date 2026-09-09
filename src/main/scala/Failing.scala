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
 * TWO ROADS, and the difference is measured (row-typeclass-recipe):
 *
 *  - the TYPED road, for the shapes the compiler can pin: `Async`
 *    itself is a GADT match, and `Async + G` / `F + Async` split by
 *    Async's OWN `TypeableK` through the kernel `<|>` and come back
 *    by plain upcast. No cast. These are the shapes every
 *    `Resource.run` in this repository actually passes.
 *  - the TOTAL road (`FailingLow.anyRow`), for every other shape: a
 *    row nests as it likes and `(Async + S) + P` matches no anchored
 *    instance, so the fallback tests the operation's own class and
 *    casts once. It is the LOW-priority instance, so it only answers
 *    where the typed ones cannot.
 *
 * Refuted on the way: an UNANCHORED `given [F, G]: Failing[F + G]` is
 * selected by dotty but cannot pin `F` (ambiguous `TypeableK[F]`),
 * and an anchored set alone leaves deeper rows to a silent identity —
 * which is the defect this hook exists to prevent. TestFailing walks
 * the shapes and asserts that BOTH roads guard.
 */
trait Failing[F[+_]]:
  /** the same operation, with `onFailure` run before its failure
   * reaches whoever handles it; an operation that cannot fail on the
   * outer handler is returned as it is */
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

trait FailingLow:
  /**
   * ANY row, by the operation's own class — the total road, and the
   * one place this file casts.
   *
   * It exists because the typed instances below can only be written
   * for row SHAPES the compiler can pin (`Async`, `Async + G`,
   * `G + Async`), and a row nests as it likes: `(Async + S) + P` is
   * not `Async + ?G` to the implicit search, so no anchored instance
   * matches it. Without this fallback such a row would silently take
   * an identity instance and the scope's finalizers would be
   * abandoned again — measured, and asserted in TestFailing. Silence
   * is the one failure mode this hook must not have, so the default
   * is TOTAL rather than typed.
   *
   * The cast is the kernel's own claim in miniature: the class test
   * proves the operation IS an `Async.Run`/`Async.Await`, the
   * replacement is the same constructor at the same answer type, and
   * `F[X]` is erased — only the type system cannot say so. Nothing
   * else in this file casts; the instances below are the typed road
   * for the shapes that occur, and they are what the common
   * `Resource.run[A, Async]` and `[A, Async + G]` actually use.
   */
  given anyRow[F[+_]]: Failing[F] = new:
    def guard[X](e: F[X], onFailure: () => Unit): F[X] = e match
      case r: Async.Run[?] =>
        Async.Run(() => try r.run() catch { case t: Throwable => onFailure(); throw t }).asInstanceOf[F[X]]
      case a: Async.Await[?] =>
        Async.Await[Any](k => a.register { r => if r.isLeft then onFailure(); k(r) }).asInstanceOf[F[X]]
      case other => other

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
