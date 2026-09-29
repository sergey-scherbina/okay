package okay.zio

import okay.{!, Async}
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
