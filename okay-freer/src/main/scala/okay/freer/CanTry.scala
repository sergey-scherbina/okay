package okay.freer

import scala.util.*
import scala.annotation.implicitNotFound

/**
 * The seam direct-try stands on: how a monad CATCHES a JVM throw
 * from the computation's own code. Strict monads (Option, Either,
 * List) run at construction, so a try around the value covers
 * everything; a Free row runs LATER under its handlers, so the
 * instance guards the continuations through the tree — a throw from
 * a pure segment between effects lands in the handler, while a
 * throw from inside an effect's HANDLER stays that handler's
 * business (stated, not hidden). A context function (`E ?=> X`,
 * direct-try-ctx) is lazy a THIRD way — a closure over its
 * environment, not run until applied — so its instance defers the
 * try to APPLICATION time (`ctxFn` below), the honest counterpart to
 * the Free row's per-step guard. The instances are NAMED, not a
 * catch-all: a lazy monad given the STRICT instance would try the
 * CONSTRUCTION and never the run, and the catch would silently never
 * fire — so an F without an instance is a compile error that says
 * so, and a strict monad of your own declares itself in one line:
 * `given CanTry[M] = CanTry.strict`.
 */
@implicitNotFound("no CanTry[${F}]: `try` in a direct block needs to know how ${F} catches a throw.\nStrict monads (Option, Either, List, Vector, Try), Free rows, and context functions (E ?=> X)\nhave instances; for a strict monad of your own declare `given CanTry[${F}] = CanTry.strict` — a\nCont-shaped LAZY monad has no honest instance: its body runs after the try, catch in the run instead.")
trait CanTry[F[_]]:
  def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A]

object CanTry:
  import okay.freer.!.*
  /** strict monads: the whole computation happens at construction */
  def strict[F[_]]: CanTry[F] = new:
    def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A] =
      try fa catch case e: Throwable => h(e)

  given option: CanTry[Option] = strict
  given either: [E] => CanTry[[X] =>> Either[E, X]] = strict
  given list: CanTry[List] = strict
  given vector: CanTry[Vector] = strict
  given tries: CanTry[Try] = strict
  /** direct-try-ctx: a context function is lazy in its environment
   * (a closure, not run until applied), so the try must defer to
   * APPLICATION time, not construction — the honest counterpart to
   * `rows`' per-step guard. Also sidesteps the dotty 3.7.4 erasure
   * crash ("bad adapt for M\$proxy2.pure(a)") the STRICT shape hit
   * when tried here during the 2026-09-02 audit — a different
   * generated-code shape, not a version bump */
  given ctxFn: [E] => CanTry[[X] =>> E ?=> X] = new:
    def tryIn[A](fa: => (E ?=> A))(h: Throwable => (E ?=> A)): E ?=> A =
      (e: E) ?=> (try fa(using e) catch case ex: Throwable => h(ex)(using e))
  /** Free rows: guard construction AND every continuation step */
  given rows: [Fx[+_]] => CanTry[[X] =>> X ! Fx] = new:
    def tryIn[A](fa: => A ! Fx)(h: Throwable => A ! Fx): A ! Fx = guardRows[A, Fx](fa)(h)(HandleFrames.catching[A, Fx](h)(fa))

  /** `fa` with every step under a `try` answered by `h` — a fold `d` deep; on a machine, `frame` (`rows`'s catch
   * frame, or `HandleFrames.unwinding`'s) */
  private[okay] def guardRows[A, Fx[+_]](fa: => A ! Fx)(h: Throwable => A ! Fx)(frame: => Shift.U[Fx, A]): A ! Fx =
      // the head of `x`, a nested run (a handler, a machine run) forced on the way, under this step's `try` —
      // as its fold `d` deep, as its frame on a machine at HandleFrames.Limit (handle-frames-catch)
      @scala.annotation.tailrec def headOf(d: Int)(x: A ! Fx): A ! Fx = (x.resumeRun: @unchecked) match
        case r @ Return(_) => r
        case i @ Inject(_) => i
        case b @ Bind(Inject(_), _) => b
        case y => headOf(d)(HandleFrames.shallow(y, d))
      def step(d: Int)(p: () => A ! Fx): A ! Fx =
        // a dropped continuation's throw is no failure of this step: on, past the `try` (resource-abort-releases)
        (try Right(headOf(d)(p())) catch { case e: Shift.Discontinued => throw e; case e: Throwable => Left(e) }) match
          case Left(e) => h(e)
          // the stack's convention: the head answers one of three shapes
          case Right(head) => (head: @unchecked) match
            case Return(a) => Free.Return(a)
            case Inject(op) => Free.Inject(op)
            case Bind(Inject(op), k) => Free.Bind(Free.Inject(op), x => step(d)(() => k(x)))
      // a value: its fold forced by anything, a catch FRAME on a machine that meets it — the `try` as data
      HandleFrames.run[A, Fx](d => step(d)(() => fa), frame)
