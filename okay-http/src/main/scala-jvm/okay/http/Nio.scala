package okay.http

import okay.*
import okay.given

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Non-blocking sockets, and the one place on the JVM where the
 * callback shape is the NATURAL one.
 *
 * Everywhere else in this repository a blocking call parks a virtual
 * thread and that is the right trade — Loom made it cheap and the
 * interop modules all take it. `AsynchronousSocketChannel` is
 * different: its `CompletionHandler` is register-and-be-called-back,
 * which is precisely `Async.Await`'s shape, down to the canceller. So
 * this transport parks nothing at all, and does it without adapting
 * anything: `Async.Await(k => ...)` IS the completion handler.
 *
 * What this is NOT is an HTTP client. Writing HTTP/1.1 by hand where
 * `java.net.http` exists is work without a payoff — the same reasoning
 * specs/http.md uses for not cross-building to Native. This is the byte
 * level, which is what raw NIO honestly offers: two ends, chunks
 * between them, and `Nio.link` for a line protocol on top. Netty is
 * the answer to "NIO with HTTP", and that codec is worth a dependency.
 */
object Nio {

  /** the row a byte stream lives in */
  private type F = Writer % Chunk[Byte] + Async

  /**
   * One connection, either end of it.
   *
   * `bytes` is a `Source`, so it is an ordinary `Stream` in `Async` and
   * every combinator applies — including `Http.framing`, which turns it
   * into lines without knowing it is looking at a socket.
   */
  final class Conn(private[Nio] val ch: AsynchronousSocketChannel,
                   size: Int = 8192) {

    def send(b: Chunk[Byte]): Unit ! Async =
      val buf = ByteBuffer.wrap(b.toArray)
      def go: Unit ! Async =
        if !buf.hasRemaining then pure(())
        else Async.await[Integer] { k =>
          ch.write(buf, null, handler[Integer](k))
          () => ()
        }.flatMap(_ => go)   // a write is partial by contract; drain it

      go

    def send(line: String): Unit ! Async =
      send(scala.collection.immutable.ArraySeq.unsafeWrapArray(line.getBytes(UTF_8)))

    /** the bytes as they arrive; end of stream ends the source */
    def bytes: Source[Chunk[Byte]] =
      def go: Source[Chunk[Byte]] =
        effect[F, Chunk[Byte] | Null](Async.Await[Chunk[Byte] | Null] { k =>
          val buf = ByteBuffer.allocate(size)
          ch.read(buf, null, new CompletionHandler[Integer, Null] {
            def completed(n: Integer, a: Null): Unit =
              if n < 0 then k(Right(null))
              else
                buf.flip()
                val out = new Array[Byte](buf.remaining()); buf.get(out)
                k(Right(scala.collection.immutable.ArraySeq.unsafeWrapArray(out)))
            def failed(e: Throwable, a: Null): Unit = k(Left(e))
          })
          () => ()
        }).flatMap {
          case null => pure(())
          case c: Chunk[Byte] @unchecked =>
            effect[F, Unit](Writer(c)).flatMap(_ => go)
        }

      go

    /** effectful, like everything else here — `shut` is the raw one a
     * finalizer needs */
    def close(): Unit ! Async = async { shut() }

    private[Nio] def shut(): Unit = try ch.close() catch case _: Throwable => ()
  }

  private def handler[A](k: Either[Throwable, A] => Unit): CompletionHandler[A, Null] =
    new CompletionHandler[A, Null] {
      def completed(v: A, a: Null): Unit = k(Right(v))
      def failed(e: Throwable, a: Null): Unit = k(Left(e))
    }

  /** connect, without parking anything */
  def connect(host: String, port: Int): Conn ! Async =
    Async.await[Conn] { k =>
      val ch = AsynchronousSocketChannel.open()
      ch.connect(InetSocketAddress(host, port), null,
        new CompletionHandler[Void, Null] {
          def completed(v: Void, a: Null): Unit = k(Right(Conn(ch)))
          def failed(e: Throwable, a: Null): Unit = k(Left(e))
        })
      () => try ch.close() catch case _: Throwable => ()
    }

  /**
   * Listen, serving each accepted connection with `serve` on its own
   * fiber. The server channel is a `Resource` — it owns a thread group,
   * and handing one back without a scope is a leak with instructions.
   */
  def listen(port: Int)(serve: Conn => Unit ! Async)
            (using Scheduler): AsynchronousServerSocketChannel ! Resource =
    Resource.acquire {
      val server = AsynchronousServerSocketChannel.open()
      server.bind(InetSocketAddress(port))

      def accept(): Unit =
        server.accept(null, new CompletionHandler[AsynchronousSocketChannel, Null] {
          def completed(ch: AsynchronousSocketChannel, a: Null): Unit =
            accept()                       // the next one, before serving this
            Async.spawn(serve(Conn(ch)))
            ()
          def failed(e: Throwable, a: Null): Unit = ()   // the channel closed
        })

      accept()
      server
    }(s => try s.close() catch case _: Throwable => ())

  /** the port a listener bound to — useful when 0 asked for any free one */
  def port(s: AsynchronousServerSocketChannel): Int =
    s.getLocalAddress.asInstanceOf[InetSocketAddress].getPort

  /**
   * A connection AS an MCP link: newline-delimited lines over a raw
   * socket, with no HTTP anywhere.
   *
   * MCP's own framing is a line per message, and `Http.framing` already
   * turns a byte source into lines — so this is two lines of glue, and
   * it means an MCP server can be reached over a bare TCP socket as
   * well as over pipes and over a WebSocket.
   */
  def link(c: Conn): okay.mcp.Link = new okay.mcp.Link:
    def send(line: String): Unit ! Async = c.send(line + "\n")

    def lines: Source[String] =
      through[Chunk[Byte], String, Async, Unit, Unit](c.bytes)(
        !.widen[Unit, Take % Chunk[Byte] + Writer % String, Async](Http.framing))
}
