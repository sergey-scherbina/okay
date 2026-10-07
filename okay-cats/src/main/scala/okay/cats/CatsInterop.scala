package okay.cats

import okay.{%, +, Async}
import okay.freer.{!, Free, Throws, effect, runEither}
import okay.freer.!.*
import okay.freer.given
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.IORuntime

/**
 * Interop with cats (specs/interop.md): instances inward, conversions
 * outward, nothing more. import okay.cats.given for the instances.
 */

/** every okay program is a cats Monad (tailRecM builds lazily — the
 * recursion hides in the flatMap closure, run stack-safely by Free) */
given [F[+_]]: _root_.cats.StackSafeMonad[[A] =>> A ! F] with
  def pure[A](a: A): A ! F = okay.freer.pure(a)
  def flatMap[A, B](fa: A ! F)(f: A => B ! F): B ! F = fa.flatMap(f)

/**
 * MonadError over a row containing Throws % E: raiseError is the
 * effect's raise, recovery goes through runEither and re-raising.
 */
// The TypeableK is never USED in the body, and the compiler says so —
// but it is load-bearing anyway: it narrows this given so it does not
// tie with given_StackSafeMonad_! when cats asks for an Invariant.
// Removing it makes MonadErrorTests fail to resolve. An unused using
// on a given can be doing work that is not in the body.
@annotation.nowarn("id=E198")  // the message filter stopped matching; the id does
given [E, F[+_]](using okay.TypeableK[Throws % E])
: _root_.cats.MonadError[[A] =>> A ! Throws % E + F, E] =
  new _root_.cats.StackSafeMonad[[A] =>> A ! Throws % E + F]
    with _root_.cats.MonadError[[A] =>> A ! Throws % E + F, E]:
    def pure[A](a: A): A ! Throws % E + F = okay.freer.pure(a)
    def flatMap[A, B](fa: A ! Throws % E + F)(f: A => B ! Throws % E + F) = fa.flatMap(f)
    def raiseError[A](e: E): A ! Throws % E + F = effect(Throws(e))
    def handleErrorWith[A](fa: A ! Throws % E + F)(f: E => A ! Throws % E + F) =
      !.widen[Either[E, A], F, Throws % E](runEither[A, F, E](fa))
        .flatMap {
          case Right(a) => okay.freer.pure(a)
          case Left(e) => f(e)
        }

/**
 * A cats `IO` marked inside a `direct` block over an okay program
 * (specs/direct-foreign-mark.md) — `io.?`, `io.reflect` or `!io` — as one
 * `Async` operation, [[CatsInterop.fromIO]]; needs an `IORuntime` at the mark.
 */
given ioForeign(using rt: IORuntime): okay.ForeignEffect[IO] with
  type G[+X] = Async[X]
  def lift[A](m: IO[A]): A ! Async = CatsInterop.fromIO(m)

object CatsInterop {

  /**
   * OUR Scheduler specialized to THEIR runtime: fork runs the thunk
   * as an IO on cats-effect's blocking pool (our thunks may park —
   * that is what their blocking pool is for), join parks the CALLER
   * (an okay virtual thread) on the future — the compute pool never
   * blocks. Bring it into scope to run okay fibers, parMap, merge and
   * supervision on the cats-effect runtime.
   */
  def scheduler(using rt: IORuntime, parks: okay.Answers[Async]): okay.Scheduler = new:
    def fork[A](prog: () => A ! okay.Async): okay.Fiber[A] =
      val (fut, cancelIO) = IO.blocking(prog().runWith).unsafeToFutureCancelable()
      new okay.Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit =
          fut.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
        def cancel(): Unit = { val _ = cancelIO() }
        def answered: Boolean = fut.isCompleted

  /** run an okay Async program as an IO (it may park — IO.blocking).
   * `Answers[Async]` is the evidence that this platform can park: given
   * on the JVM and Native (`import okay.given`), absent on JS, where
   * [[toIOAsync]] is the door */
  def toIO[A](p: => A ! Async)(using okay.Answers[Async]): IO[A] = IO.blocking(p.runWith)

  /** Run a callback-driven okay Async program as an IO without parking a
   * thread while an Await is pending (specs/cats-io-async.md); cancelling
   * the IO cancels the drive. Async.Run may block, so programs containing
   * it belong at [[toIO]]. */
  def toIOAsync[A](p: => A ! Async): IO[A] =
    IO.async[A] { cb =>
      IO {
        val running = Async.runAsyncCancellable(p)
        running.future.onComplete(t => cb(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
        Some(IO(running.cancel()))
      }
    }

  /**
   * An IO as an Async operation (specs/cats-io-async.md): an `Await` on
   * `unsafeToFutureCancelable`. The callback runner parks no thread while
   * it runs (`runWith` parks as for any Await), a failure crosses as the
   * same throwable, and cancelling the okay side cancels the IO — its
   * finalizers run, and a late result resumes nothing.
   */
  def fromIO[A](io: IO[A])(using rt: IORuntime): A ! Async =
    Async.await[A] { k =>
      val (done, cancel) = io.unsafeToFutureCancelable()
      done.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
      () => { val _ = cancel() }
    }

  /** okay's accumulating `Validated` as cats' (specs/interop-classes.md) */
  def toCatsValidated[E, A](v: okay.freer.Validated[E, A]): _root_.cats.data.Validated[E, A] = v match
    case okay.freer.Validated.Valid(a) => _root_.cats.data.Validated.Valid(a)
    case okay.freer.Validated.Invalid(e) => _root_.cats.data.Validated.Invalid(e)

  /** cats' `Validated` as okay's */
  def fromCatsValidated[E, A](v: _root_.cats.data.Validated[E, A]): okay.freer.Validated[E, A] = v match
    case _root_.cats.data.Validated.Valid(a) => okay.freer.Validated.Valid(a)
    case _root_.cats.data.Validated.Invalid(e) => okay.freer.Validated.Invalid(e)

  /** an okay Free program as a cats free monad, operation for operation */
  def toCats[F[+_], A](p: A ! F): _root_.cats.free.Free[F, A] =
    (p.resume: @unchecked) match
      case Return(a) => _root_.cats.free.Free.pure(a)
      case Inject(e) => _root_.cats.free.Free.liftF(e)
      case Bind(Inject(e), k) =>
        _root_.cats.free.Free.liftF(e).flatMap(x => toCats(k(x)))

  /** a cats free monad as an okay program, by initiality (foldMap into
   * our Monad instance through the injecting FunctionK) */
  def fromCats[F[+_], A](c: _root_.cats.free.Free[F, A]): A ! F =
    c.foldMap(new _root_.cats.arrow.FunctionK[F, [X] =>> X ! F]:
      def apply[X](fx: F[X]): X ! F = effect(fx))
}
