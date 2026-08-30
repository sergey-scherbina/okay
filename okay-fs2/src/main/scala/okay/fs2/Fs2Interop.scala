package okay.fs2

import okay.{Chunk, Chunks, Channel}
import okay.given
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.IORuntime

/**
 * Interop with fs2 (specs/interop.md): streams cross CHUNK FOR CHUNK —
 * both sides are chunked, nothing is re-buffered. Outbound is pure
 * (our pull is); inbound runs the fs2 stream on its own runtime into
 * one of our channels, whose bounded capacity backpressures fs2
 * through a blocking send.
 */
object Fs2Interop {

  /** a chunked okay stream as a pure fs2 stream, chunk for chunk */
  def toFs2[A](p: Chunks[A]): _root_.fs2.Stream[_root_.fs2.Pure, A] =
    _root_.fs2.Stream.unfoldChunk(p)(rest =>
      Chunks.pull(rest).map((c, r) => (_root_.fs2.Chunk.from(c), r)))

  /**
   * An fs2 IO stream as a chunked okay stream, with THEIR backpressure
   * primitives: the fs2 side offers into a bounded cats-effect Queue —
   * offer SUSPENDS THE IO FIBER when the queue is full, no thread
   * blocks on their runtime. Our side takes from the queue by parking
   * an okay virtual thread (Loom pays for the blocking, as always
   * here). Each side waits in its own native way.
   */
  def fromFs2[A](s: _root_.fs2.Stream[IO, A], capacity: Int = 64)
                (using IORuntime): Chunks[A] =
    import _root_.cats.effect.std.Queue
    val q = Queue.bounded[IO, Option[_root_.fs2.Chunk[A]]](capacity).unsafeRunSync()
    s.chunks.evalMap(ch => q.offer(Some(ch)))
      .compile.drain
      .guarantee(q.offer(None))
      .unsafeRunAsync(_ => ())
    def go(): Chunks[A] = Chunks.defer:
      q.take.unsafeRunSync() match
        case None => Chunks.end
        case Some(ch) =>
          okay.produce(Chunks.wrap[A](ch.iterator.asInstanceOf[Iterator[AnyRef]].toArray))
            .flatMap(_ => go())
    go()
}
