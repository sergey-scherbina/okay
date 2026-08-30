package okay.zio

import okay.{!, Async, Chunk, Chunks, async}
import okay.given
import _root_.zio.{Runtime, Scope, Task, Unsafe, ZIO}
import _root_.zio.stream.ZStream

/**
 * Interop with ZIO (specs/interop.md): the effect bridge runs each
 * side to completion on the other's terms (a virtual thread parks for
 * ZIO; ZIO blocks for okay), the stream bridge moves CHUNK FOR CHUNK —
 * both sides are chunked, so nothing is re-buffered.
 */
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
            runtime.unsafe.fork(fiber.await.map {
              case _root_.zio.Exit.Success(a) => k(Right(a))
              case _root_.zio.Exit.Failure(c) => k(Left(c.squash))
            })
            ()
          }
        def cancel(): Unit = Unsafe.unsafe { implicit u =>
          runtime.unsafe.run(fiber.interruptFork).getOrThrowFiberFailure()
          ()
        }

  /** run an okay Async program as a ZIO (it may park — attemptBlocking) */
  def toZIO[A](p: => A ! Async): Task[A] = ZIO.attemptBlocking(p.runWith)

  /** a ZIO as an Async operation: the virtual thread parks for it */
  def fromZIO[A](z: Task[A], runtime: Runtime[Any] = Runtime.default): A ! Async =
    async(Unsafe.unsafe(implicit u => runtime.unsafe.run(z).getOrThrowFiberFailure()))

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
