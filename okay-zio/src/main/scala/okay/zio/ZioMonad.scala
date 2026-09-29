package okay.zio

import okay.{!, %, +, Async, Throws, raise}
import _root_.zio.{Task, ZIO}

/**
 * ZIO as okay's Monad (specs/zio-direct-cancel.md): with
 * `import okay.zio.given`, `direct[Task] { val x = z.?; ... }` binds ZIO
 * values with okay's own marks. ZIO's flatMap is the bind, so stack
 * safety is ZIO's trampoline.
 */
given zioMonad[R, E]: okay.Monad[[A] =>> ZIO[R, E, A]] with
  def pure[A](a: A): ZIO[R, E, A] = ZIO.succeed(a)
  override def fmap[A, B](a: ZIO[R, E, A], f: A => B): ZIO[R, E, B] = a.map(f)
  extension [A](a: ZIO[R, E, A])
    def flatMap[B](f: A => ZIO[R, E, B]): ZIO[R, E, B] = a.flatMap(f)

extension [A](p: => A ! Async)
  /** the okay program as a Task — [[ZioInterop.toZIO]], right for any
   * program, a blocking `Async.Run` included */
  def asZIO: Task[A] = ZioInterop.toZIO(p)

extension [A](z: Task[A])
  /** the Task as an okay program — [[ZioInterop.fromZIO]] */
  def asOkay: A ! Async = ZioInterop.fromZIO(z)

/**
 * ZIO values marked inside a `direct` block over an okay program
 * (specs/direct-foreign-mark.md), `z.?` / `z.reflect` — never `!z`, which
 * is ZIO's own negation. The most specific instance wins: no environment
 * and a Throwable error is one `Async` operation; no environment adds the
 * typed error as `Throws % E`; otherwise the whole [[ZioRow]].
 */
given zioForeignAsync[E <: Throwable]: okay.ForeignEffect[[X] =>> ZIO[Any, E, X]] with
  type G[+X] = Async[X]
  def lift[A](m: ZIO[Any, E, A]): A ! Async = ZioInterop.fromZIO(m)

given zioForeignThrows[E]: okay.ForeignEffect[[X] =>> ZIO[Any, E, X]] with
  type G[+X] = (Throws % E + Async)[X]
  def lift[A](m: ZIO[Any, E, A]): A ! G =
    !.widen[Either[E, A], Async, Throws % E](ZioInterop.fromZIO(m.either)).flatMap {
      case Right(a) => okay.pure(a)
      case Left(e) => !.widen[A, Throws % E, Async](raise[E, A](e))
    }

given zioForeignRow[R, E]: okay.ForeignEffect[[X] =>> ZIO[R, E, X]] with
  type G[+X] = ZioRow[R, E][X]
  def lift[A](m: ZIO[R, E, A]): A ! G = ZioInterop.fromZIOTyped(m)
