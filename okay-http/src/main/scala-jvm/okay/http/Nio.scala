package okay.http

import okay.*
import okay.given

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Raw TCP: blocking channels parked on virtual threads.
 *
 * This file used to stand on `AsynchronousSocketChannel`, on the
 * premise that the completion-handler shape is `Async.Await`'s shape
 * and therefore the natural one. The premise was refuted by
 * measurement (okay-http/BUGS.md: nio-serve-stall): under rapid
 * channel churn on macOS the JDK's asynchronous layer loses
 * completion events — ~1.5 per 1000 rounds, pinned to the accept
 * dispatch, on the default channel group and a dedicated one alike —
 * and a lost accept also killed the re-arm, silencing the listener
 * forever. Blocking channels have no completion to lose, Loom makes
 * the parking free, and the cluster transport benchmark priced the
 * two within noise of each other (docs/benchmarks.md). specs/nio.md
 * carries the full argument.
 *
 * What this is NOT is an HTTP client. Writing HTTP/1.1 by hand where
 * `java.net.http` exists is work without a payoff — the same reasoning
 * specs/http.md uses for not cross-building to Native. This is the byte
 * level: two ends, chunks between them, and `Nio.link` for a line
 * protocol on top. Netty is the answer to "NIO with HTTP", and that
 * codec is worth a dependency.
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
  final class Conn(private[Nio] val ch: SocketChannel,
                   size: Int = 8192) {

    def send(b: Chunk[Byte]): Unit ! Async = async {
      val buf = ByteBuffer.wrap(b.toArray)
      // a write is partial by contract; drain it
      while buf.hasRemaining do { val _ = ch.write(buf) }
    }

    def send(line: String): Unit ! Async =
      send(scala.collection.immutable.ArraySeq.unsafeWrapArray(line.getBytes(UTF_8)))

    /** the bytes as they arrive; end of stream ends the source */
    def bytes: Source[Chunk[Byte]] =
      def go: Source[Chunk[Byte]] =
        effect[F, Chunk[Byte] | Null](Async.Run { () =>
          val buf = ByteBuffer.allocate(size)
          if ch.read(buf) < 0 then null
          else
            buf.flip()
            val out = new Array[Byte](buf.remaining()); buf.get(out)
            scala.collection.immutable.ArraySeq.unsafeWrapArray(out)
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

  /** connect — parks the fiber, which is what fibers are for */
  def connect(host: String, port: Int): Conn ! Async =
    async { Conn(SocketChannel.open(InetSocketAddress(host, port))) }

  /**
   * Listen, serving each accepted connection with `serve` on its own
   * fiber. The accept loop is itself a fiber parked in `accept()`;
   * closing the Resource unparks it (ClosedChannelException) and ends
   * it — there is no completion to lose and no re-arm to kill.
   */
  def listen(port: Int)(serve: Conn => Unit ! Async)
            (using Scheduler): ServerSocketChannel ! Resource =
    Resource.acquire {
      val server = ServerSocketChannel.open()
      server.bind(InetSocketAddress(port))
      def loop: Unit ! Async =
        async(server.accept()).flatMap { ch =>
          val _ = Async.spawn(serve(Conn(ch)))
          loop
        }
      val _ = Async.spawn(loop)   // dies with the channel: accept throws when it closes
      server
    }(s => try s.close() catch case _: Throwable => ())

  /** the port a listener bound to — useful when 0 asked for any free one */
  def port(s: ServerSocketChannel): Int =
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
