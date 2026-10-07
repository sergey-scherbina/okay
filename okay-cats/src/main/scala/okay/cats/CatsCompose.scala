package okay.cats

import okay.Async
import okay.freer.{!}
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.IORuntime

/**
 * ONE EXPRESSION ACROSS LIBRARIES (specs/interop-compose.md): an IO and
 * an `A => IO[B]` cross into okay with okay's `asOkay` (this file's
 * `ToOkay` instance), an okay program and an
 * `A => B ! Async` cross out with `asIO`. The function forms are what let
 * `>=>` compose cats, ZIO, kyo and okay functions in one chain; the value
 * forms are what an IO for-comprehension calls okay with.
 *
 * Every okay program crosses as `A ! Async`: any other effect in its row
 * is handled first (`Reader.run`, `runEither`), because cats has no
 * place to put it.
 */

/** an IO crosses into okay as one `Async` operation — [[CatsInterop.fromIO]];
 * `io.asOkay` is okay's own extension, which finds this */
given ioToOkay[A](using IORuntime): okay.ToOkay[IO[A], A] = io => CatsInterop.fromIO(io)

extension [A](p: => A ! Async)
  /** this okay program as an IO — [[CatsInterop.toIO]], right for any
   * program, a blocking `Async.Run` included; JVM and Native (it parks) */
  def asIO(using okay.Answers[Async]): IO[A] = CatsInterop.toIO(p)

extension [A, B](f: A => B ! Async)
  /** this okay function as an IO one; JVM and Native (it parks) */
  def asIO(using okay.Answers[Async]): A => IO[B] = a => CatsInterop.toIO(f(a))
