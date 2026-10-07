package okay.zio

import okay.{Async, Chunks}

import okay.freer.{%, +}
import okay.freer.{!}
import okay.std.{Reader, Throws, raise, runEither}
import okay.given
import okay.freer.given
import _root_.zio.{Exit, Runtime, Scope, Task, Unsafe, ZEnvironment, ZIO}
import _root_.zio.stream.ZStream
import scala.concurrent.ExecutionContext.parasitic

/**
 * Interop with ZIO (specs/interop.md): the effect bridge runs each
 * side to completion on the other's terms (a virtual thread parks for
 * ZIO; ZIO blocks for okay), the stream bridge moves CHUNK FOR CHUNK —
 * both sides are chunked, so nothing is re-buffered.
 */
/** ZIO's three channels as okay's effects (specs/zio-typed-row.md) */
type ZioRow[R, E] = Reader % ZEnvironment[R] + Throws % E + Async

object ZioInterop {

  /**
   * OUR Scheduler specialized to THEIR runtime: fork runs the thunk
   * as a blocking ZIO on the zio blocking pool, join parks the okay
   * caller on the fiber, cancel interrupts it. One given, and okay
   * fibers, parMap, merge and supervision run on the ZIO runtime.
   */
  def scheduler(runtime: Runtime[Any] = Runtime.default): okay.Scheduler = new:
    def fork[A](prog: () => A ! okay.Async): okay.Fiber[A] =
      val fiber = Unsafe.unsafe(implicit u =>
        runtime.unsafe.fork(ZIO.attemptBlocking(prog().runWith)))
      new okay.Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit =
          Unsafe.unsafe { implicit u =>
            val _ = runtime.unsafe.fork(fiber.await.map {
              case _root_.zio.Exit.Success(a) => k(Right(a))
              case _root_.zio.Exit.Failure(c) => k(Left(c.squash))
            })
            ()
          }
        def cancel(): Unit = Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(fiber.interruptFork).getOrThrowFiberFailure()
          ()
        }
        /** ZIO's own non-blocking look at the fiber's exit */
        def answered: Boolean = Unsafe.unsafe(implicit u => fiber.unsafe.poll.isDefined)

  /** run an okay Async program as a ZIO (it may park — attemptBlocking) */
  def toZIO[A](p: => A ! Async): Task[A] = ZIO.attemptBlocking(p.runWith)

  /** Run a callback-driven Okay Async program as a ZIO without parking a
   * thread while Await is pending. Interruption unregisters the active Await.
   * Async.Run may block, so programs containing it belong at [[toZIO]]. */
  def toZIOAsync[A](p: => A ! Async): Task[A] =
    ZIO.asyncInterrupt { done =>
      val running = Async.runAsyncCancellable(p)
      running.future.onComplete {
        case scala.util.Success(a) => done(ZIO.succeed(a))
        case scala.util.Failure(e) => done(ZIO.fail(e))
      }(using parasitic)
      Left(ZIO.succeed(running.cancel()))
    }

  /**
   * A ZIO as an Async operation (specs/zio-direct-cancel.md): an
   * `Await` on a forked fiber. The callback runner parks no thread while
   * it runs (`runWith` parks as it does for any Await), a failure crosses
   * as the same throwable, and cancelling the okay side interrupts the
   * fiber — its finalizers run, and a late result resumes nothing.
   */
  def fromZIO[A](z: Task[A], runtime: Runtime[Any] = Runtime.default): A ! Async =
    Async.await[A] { k =>
      Unsafe.unsafe { implicit u =>
        val fiber = runtime.unsafe.fork(z)
        fiber.unsafe.addObserver {
          case Exit.Success(a) => k(Right(a))
          case Exit.Failure(c) => k(Left(c.squash))
        }
        () => { val _ = runtime.unsafe.fork(fiber.interrupt) }
      }
    }

  /**
   * The whole `ZIO[R, E, A]` as an okay program (specs/zio-typed-row.md):
   * the environment is read from the Reader, a typed failure is
   * `raise(e)`, a defect or an interruption fails the Async run, and
   * cancelling the okay side interrupts the fiber — as [[fromZIO]].
   */
  def fromZIOTyped[R, E, A](z: ZIO[R, E, A], runtime: Runtime[Any] = Runtime.default): A ! ZioRow[R, E] =
    // widened by name, not `.at`: membership is not searchable at a row
    // whose arguments are abstract (AGENTS.md, "an obligation over a row
    // is carried"), and each of the three names its own place
    !.widen[ZEnvironment[R], Reader % ZEnvironment[R], Throws % E + Async](Reader.ask[ZEnvironment[R]])
      .flatMap { env =>
        !.widen[Either[E, A], Async, Reader % ZEnvironment[R] + Throws % E](
          fromZIO(z.provideEnvironment(env).either, runtime)).flatMap {
          case Right(a) => okay.freer.pure(a)
          case Left(e) => !.widen[A, Throws % E, Reader % ZEnvironment[R] + Async](raise[E, A](e))
        }
      }

  /**
   * An okay program over [[ZioRow]] as the whole `ZIO[R, E, A]`: its
   * Reader is ZIO's environment, `raise(e)` is `fail(e)`, and a
   * throwable escaping the program is a defect. Run as [[toZIO]]
   * (the blocking pool), so any program is safe here.
   */
  def toZIOTyped[R, E, A](p: => A ! ZioRow[R, E]): ZIO[R, E, A] =
    ZIO.environmentWithZIO[R] { env =>
      toZIO(runEither[A, Async, E](Reader.run[ZEnvironment[R], A, Throws % E + Async](env)(p)))
        .orDie.flatMap(ZIO.fromEither(_))
    }

  /** a chunked okay stream as a ZStream, chunk for chunk (the pull is pure) */
  def toZStream[A](p: Chunks[A]): ZStream[Any, Nothing, A] =
    ZStream.unfoldChunk(p)(rest =>
      Chunks.pull(rest).map((c, r) => (_root_.zio.Chunk.fromIterable(c), r)))

  /**
   * A ZStream as chunked okay stream: the stream's scoped iterator is
   * opened once and driven lazily — the scope closes when the
   * iterator ends. Linear, like every external source.
   */
  def fromZStream[A](s: ZStream[Any, Throwable, A], size: Int = 64,
                     runtime: Runtime[Any] = Runtime.default): Chunks[A] =
    Unsafe.unsafe { implicit u =>
      val scope = runtime.unsafe.run(Scope.make).getOrThrowFiberFailure()
      val it = runtime.unsafe.run(scope.extend(s.toIterator)).getOrThrowFiberFailure()
      val closing = new Iterator[A]:
        def hasNext: Boolean =
          val h = it.hasNext
          if !h then runtime.unsafe.run(scope.close(_root_.zio.Exit.unit)).getOrThrowFiberFailure()
          h
        def next(): A = it.next().fold(throw _, identity)
      Chunks.fromIterator(closing, size)
    }
}
