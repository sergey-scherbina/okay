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
 * TWO INSTANCES, and the second one's cast is the point rather than a
 * blemish: a row is a type LAMBDA, `A + B + C` nests to the left, and
 * an instance can only be written for a shape the compiler can pin.
 * So the default answers by the OPERATION's own class instead, which
 * is total over every nesting. The alternative — typed instances for
 * the shapes plus an identity default — was built, measured and
 * REFUTED: `(Async + S) + P` matched no instance, the identity
 * answered, and the row went silently unguarded (the defect this hook
 * exists to prevent). An identity default is the one thing to refuse.
 *
 * Also refuted, in the same lane: an UNANCHORED
 * `given [F[+_], G[+_]]: Failing[F + G]` is selected by dotty and then
 * cannot pin `F` (ambiguous `TypeableK[F]`); and instances ANCHORED on
 * the concrete effect (`Failing[Async + G]`, `Failing[F + Async]`,
 * splitting through the kernel `<|>` with no cast) do work — they were
 * written, and then deleted as decoration, because a total default
 * already answers those shapes identically and at the same cost. What
 * survives of that road is `Failing[Async]` below: a single effect IS
 * a shape the compiler pins, it is what most `Resource.run` call sites
 * pass, and it needs no cast at all.
 *
 * The whole story, with the measurements: docs/typepedia.md, "The
 * row-typeclass recipe"; the shapes are walked in TestFailing.
 */
trait Failing[F[+_]]:
  /** the same operation, with `onFailure` run before its failure
   * reaches whoever handles it; an operation that cannot fail on the
   * outer handler is returned as it is */
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

trait FailingLow:
  /**
   * ANY row, by the operation's own class — total over every nesting,
   * and the one place this file casts.
   *
   * The cast is the kernel's own claim in miniature: the class test
   * proves the operation IS an `Async.Run`/`Async.Await`, the
   * replacement is the same constructor at the same answer type, and
   * `F[X]` is erased — only the type system cannot say so. A row with
   * no Async operation in it never reaches a cast: the match falls
   * through and returns the operation untouched.
   */
  given anyRow[F[+_]]: Failing[F] = new:
    def guard[X](e: F[X], onFailure: () => Unit): F[X] = e match
      case r: Async.Run[?] =>
        Async.Run(() => try r.run() catch { case t: Throwable => onFailure(); throw t }).asInstanceOf[F[X]]
      case a: Async.Await[?] =>
        Async.Await[Any](k => a.register { r => if r.isLeft then onFailure(); k(r) }).asInstanceOf[F[X]]
      case other => other

object Failing extends FailingLow:
  /** the typed road, for the row every second `Resource.run` passes:
   * one effect is a shape the compiler pins, so the operation is
   * rebuilt under a GADT match and no cast is needed. Higher priority
   * than `anyRow`, which would answer the same way through the cast. */
  given async: Failing[Async] = new:
    def guard[X](e: Async[X], onFailure: () => Unit): Async[X] = e match
      case Async.Run(f) => Async.Run(() => try f() catch { case t: Throwable => onFailure(); throw t })
      case Async.Await(reg) => Async.Await(k => reg { r => if r.isLeft then onFailure(); k(r) })
