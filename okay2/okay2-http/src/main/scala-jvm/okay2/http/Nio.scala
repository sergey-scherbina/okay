package okay2.http

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.ArraySeq
import okay2.{!, Resource, Writer, pure}
import okay2.async.{Async, Scheduler}
import okay2.stream.{Chunk, Source}

/** raw NIO channels (okay-http's Nio.scala): bytes out, a chunked source
 * in, a listener that is a Resource and serves each connection on its
 * own fiber */
object Nio {

  final class Conn(private[Nio] val ch: SocketChannel, size: Int = 8192) {

    /** the whole chunk: a partial write is looped until drained */
    def send(b: Chunk[Byte]): Unit ! Async = Async {
      val buf = ByteBuffer.wrap(b.toArray)
      while (buf.hasRemaining) { val _ = ch.write(buf) }
    }

    def send(line: String): Unit ! Async = send(ArraySeq.unsafeWrapArray(line.getBytes(UTF_8)))

    /** what arrives, as it arrives, to the end of the stream */
    def bytes: Source[Chunk[Byte]] = {
      def go: Source[Chunk[Byte]] =
        Async[Option[Chunk[Byte]]] {
          val buf = ByteBuffer.allocate(size)
          if (ch.read(buf) < 0) None
          else {
            buf.flip()
            val out = new Array[Byte](buf.remaining()); buf.get(out)
            Some(ArraySeq.unsafeWrapArray(out))
          }
        }.flatMap[Writer[Chunk[Byte]] with Async, Unit] {
          case None => pure(())
          case Some(c) => Writer.tell[Chunk[Byte]](c).flatMap(_ => go)
        }
      go
    }

    def close(): Unit ! Async = Async(shut())

    private[Nio] def shut(): Unit = try ch.close() catch { case _: Throwable => () }
  }

  def connect(host: String, port: Int): Conn ! Async =
    Async(new Conn(SocketChannel.open(new InetSocketAddress(host, port))))

  /** a listener on `port` (0: any free one), each connection served on
   * its own fiber; closed when the Resource scope ends */
  def listen(port: Int)(serve: Conn => Unit ! Async)(implicit S: Scheduler): ServerSocketChannel ! Resource =
    Resource.acquire {
      val server = ServerSocketChannel.open()
      server.bind(new InetSocketAddress(port))
      def loop: Unit ! Async =
        Async(server.accept()).flatMap { ch =>
          val _ = Async.spawn(serve(new Conn(ch)))
          loop
        }
      val _ = Async.spawn(loop) // dies with the channel: accept throws when it closes
      server
    }(s => try s.close() catch { case _: Throwable => () })

  def port(s: ServerSocketChannel): Int = s.getLocalAddress match {
    case a: InetSocketAddress => a.getPort
    case other => throw new IllegalStateException(s"not an inet listener: $other")
  }
}
