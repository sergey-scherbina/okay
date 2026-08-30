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
   * An fs2 IO stream as an okay channel of chunks: an IO fiber feeds
   * the channel (closing it at the end); a bounded capacity
   * backpressures fs2 by parking the feeding fiber in a blocking send.
   */
  def fromFs2[A](s: _root_.fs2.Stream[IO, A], capacity: Int = Int.MaxValue)
                (using IORuntime): Channel[Chunk[A]] =
    val c = Channel[Chunk[A]](capacity)
    s.chunks
      .evalMap(ch => IO.blocking(
        c.send(Chunks.wrap[A](ch.iterator.asInstanceOf[Iterator[AnyRef]].toArray))))
      .compile.drain
      .unsafeRunAsync(_ => c.close())
    c
}
