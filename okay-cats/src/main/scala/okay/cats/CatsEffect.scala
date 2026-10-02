package okay.cats

import okay.{!, +, Async, ==>, effect}
import okay.!.*
import _root_.cats.~>
import _root_.cats.effect.{IO, LiftIO}
import _root_.cats.effect.kernel.{Cont, Deferred, Fiber, MonadCancel, Outcome, Poll, Ref, Sync, Unique}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/**
 * AN OKAY PROGRAM AS CATS-EFFECT'S `F` (specs/cats-effect-instances.md).
 *
 * Code written `F[_]: Async` — http4s, doobie, fs2's effectful streams,
 * most of typelevel — runs at [[CatsEffect.Program]]:
 *
 *   type Program[A] = A ! CatsFx + Async
 *
 * Its binds are okay's tree. cats-effect's own primitives —
 * `uncancelable`/`poll`, `canceled`, `onCancel`, `start`, `racePair`,
 * `sleep`, `cont`, `evalOn`, `Ref`/`Deferred` — are ONE effect, `CatsFx`,
 * in the row: a program that needs cats-effect's runtime says so in its
 * type, and okay's own runners refuse it at compile time instead of
 * throwing at run time.
 *
 * [[CatsEffect.toIO]] runs it: a `foldMap` into `IO`, so the WHOLE
 * program is one IO fiber and masking and the cancellation flag live
 * where cats-effect keeps them. The refuted alternative is an IO round
 * trip per operation, `fromIO(ioOp(toIO(fa)))`: each would run in a
 * fiber of its own, and a `canceled` inside `uncancelable` would cancel
 * that fresh unmasked fiber instead of being deferred to `poll`.
 */
final case class CatsFx[+A](build: CatsEffect.Interp => IO[A])

object CatsEffect:

  /**
   * An okay program that may use cats-effect's runtime — OPAQUE, as
   * `Par` is (Par.scala), and for the reason `Par` is: transparent, a
   * `Program` would also be an `A ! F`, the default program monad of
   * CatsInterop.scala would answer `cats.Monad[Program]` beside this
   * file's `Async`, and every `traverse` would be ambiguous. Two cures
   * were measured and refuted first: a negative row test on the default
   * (`NotGiven[CatsFx[Any] <:< F[Any]]`) broke cats' partial unification
   * for every program, and a nearer import did not out-rank a package
   * given (`Isomorphisms` still found both). Opaque, a `Program` has
   * exactly one instance, found with no import (its companion's).
   */
  opaque type Program[A] = A ! CatsFx + Async

  /** the door in for a program already in this row */
  def apply[A](p: A ! CatsFx + Async): Program[A] = p

  extension [A](p: Program[A])
    /** the door out: the okay program it is */
    def toOkay: A ! CatsFx + Async = p

  /** what a `CatsFx` operation is handed: the interpreter of its
   * sub-programs, applied lazily inside the IO it builds */
  type Interp = [X] => Program[X] => IO[X]

  /**
   * Run a program as ONE IO fiber. okay's `Async.Run` is `IO.delay`, an
   * `Async.Await` is `IO.async` with its canceller as the finalizer, a
   * `CatsFx` builds its own IO.
   *
   * A WALK, NOT `foldMap`, and the difference is a law. `foldMap` closes
   * with `/ pure`, which puts a `flatMap(IO.pure)` after the LAST
   * operation; in IO every bind is a point where cancellation is seen,
   * so `onCancel(uncancelable(_ => fa), fin)` gained a step INSIDE the
   * `onCancel` after the mask came off, and its finalizer ran where IO's
   * would not ("onCancel associates over uncancelable boundary" failed,
   * cats-effect-laws). Here a single operation is exactly its IO and a
   * bind is exactly one `flatMap`. Stack-safe: the recursive call is in
   * the `flatMap`'s continuation, run by IO's own loop.
   */
  def toIO[A](p: Program[A]): IO[A] = (p.resume: @unchecked) match
    case Return(a) => IO.pure(a)
    case Inject(e) => step(e)
    case Bind(Inject(e), k) => step(e).flatMap(x => toIO(k(x)))

  private val interp: Interp = [X] => (p: Program[X]) => IO.defer(toIO(p))

  private val step: (CatsFx + Async) ==> IO = [X] => (e: (CatsFx + Async)[X]) => e match
    case CatsFx(build) => build(interp)
    case Async.Run(f) => IO.delay(f())
    case Async.Await(register, _) =>
      IO.async[X](cb => IO { val cancel = register(cb); Some(IO(cancel())) })

  /** a plain okay `Async` program as one of these — the row only grows */
  def lift[A](p: A ! Async): Program[A] = !.widen[A, Async, CatsFx](p)

  /** an IO as one operation of a program */
  def liftIO[A](io: IO[A]): Program[A] = op(_ => io)

  private def op[A](build: Interp => IO[A]): Program[A] = effect[CatsFx + Async, A](CatsFx(build))

  private val liftK: IO ~> Program = new (IO ~> Program):
    def apply[A](io: IO[A]): Program[A] = liftIO(io)

  private def fiber[A](f: Fiber[IO, Throwable, A]): Fiber[Program, Throwable, A] =
    new Fiber[Program, Throwable, A]:
      def cancel: Program[Unit] = liftIO(f.cancel)
      def join: Program[Outcome[Program, Throwable, A]] = liftIO(f.join.map(_.mapK(liftK)))

  /** cats-effect's whole hierarchy — `Async` down to `Functor` — at
   * [[Program]], with `LiftIO`; in the companion of the opaque type, so
   * it needs no import */
  given programAsync: _root_.cats.effect.kernel.Async[Program] with LiftIO[Program] with
    def liftIO[A](io: IO[A]): Program[A] = CatsEffect.liftIO(io)

    // ---- Monad: okay's tree, okay's stack-safe loop
    def pure[A](a: A): Program[A] = okay.pure(a)
    def flatMap[A, B](fa: Program[A])(f: A => Program[B]): Program[B] = fa.flatMap(f)
    override def map[A, B](fa: Program[A])(f: A => B): Program[B] = fa.map(f)
    def tailRecM[A, B](a: A)(f: A => Program[Either[A, B]]): Program[B] = !.loop(a)(f)

    // ---- MonadError
    def raiseError[A](e: Throwable): Program[A] = liftIO(IO.raiseError(e))
    def handleErrorWith[A](fa: Program[A])(f: Throwable => Program[A]): Program[A] =
      op(i => i(fa).handleErrorWith(e => i(f(e))))

    // ---- MonadCancel
    def forceR[A, B](fa: Program[A])(fb: Program[B]): Program[B] = op(i => i(fa).forceR(i(fb)))
    def uncancelable[A](body: Poll[Program] => Program[A]): Program[A] =
      op(i => IO.uncancelable(p => i(body(new Poll[Program]:
        def apply[X](fx: Program[X]): Program[X] = op(j => p(j(fx)))))))
    def canceled: Program[Unit] = liftIO(IO.canceled)
    def onCancel[A](fa: Program[A], fin: Program[Unit]): Program[A] = op(i => i(fa).onCancel(i(fin)))

    // ---- GenSpawn
    def start[A](fa: Program[A]): Program[Fiber[Program, Throwable, A]] = op(i => i(fa).start.map(fiber))
    override def never[A]: Program[A] = liftIO(IO.never)
    def cede: Program[Unit] = liftIO(IO.cede)
    // IO's own racePair rather than GenConcurrent's deferred-based default
    override def racePair[A, B](fa: Program[A], fb: Program[B]): Program[Either[
        (Outcome[Program, Throwable, A], Fiber[Program, Throwable, B]),
        (Fiber[Program, Throwable, A], Outcome[Program, Throwable, B])]] =
      op(i => IO.racePair(i(fa), i(fb)).map {
        case Left((oa, fb2)) => Left((oa.mapK(liftK), fiber(fb2)))
        case Right((fa2, ob)) => Right((fiber(fa2), ob.mapK(liftK)))
      })
    override def unique: Program[Unique.Token] = liftIO(IO.unique)

    // ---- GenConcurrent: cats-effect's own, seen through the program
    def ref[A](a: A): Program[Ref[Program, A]] = liftIO(IO.ref(a).map(_.mapK(liftK)))
    def deferred[A]: Program[Deferred[Program, A]] = liftIO(IO.deferred[A].map(_.mapK(liftK)))

    // ---- Clock / GenTemporal
    def monotonic: Program[FiniteDuration] = liftIO(IO.monotonic)
    def realTime: Program[FiniteDuration] = liftIO(IO.realTime)
    protected def sleep(time: FiniteDuration): Program[Unit] = liftIO(IO.sleep(time))

    // ---- Sync
    def suspend[A](hint: Sync.Type)(thunk: => A): Program[A] = liftIO(IO.suspend(hint)(thunk))

    // ---- Async
    def evalOn[A](fa: Program[A], ec: ExecutionContext): Program[A] = op(i => i(fa).evalOn(ec))
    def executionContext: Program[ExecutionContext] = liftIO(IO.executionContext)
    def cont[K, R](body: Cont[Program, K, R]): Program[R] =
      op(i => IO.cont(new Cont[IO, K, R]:
        def apply[G[_]](implicit G: MonadCancel[G, Throwable]): (Either[Throwable, K] => Unit, G[K], IO ~> G) => G[R] =
          (resume, get, lift) => body[G].apply(resume, get, new (Program ~> G):
            def apply[X](p: Program[X]): G[X] = lift(i(p)))))

