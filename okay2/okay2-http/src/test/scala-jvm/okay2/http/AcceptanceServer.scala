package okay2.http

import java.io.{BufferedInputStream, DataInputStream, InputStream, OutputStream}
import java.net.{InetAddress, ServerSocket, Socket => TcpSocket}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.ArraySeq
import okay2.{!, Pure}
import okay2.async.Async
import okay2.platform._
import okay2.stream.{Channel, Source}

/**
 * The JVM end of the acceptance run: ONE port serving both halves of
 * `Acceptance.check`, as okay-jetty does for okay-http.
 *
 * okay2 has no Jetty and its `Server` (the JDK's) cannot upgrade to a
 * WebSocket, so this front reads each connection's head and splits:
 * a WebSocket handshake is answered here and the session is the SHARED
 * `Acceptance.echo` Stage, run by `Ws.over` over a server-side `Socket`;
 * anything else is piped, bytes unchanged, to `Server` answering
 * `Acceptance.routes` on `backend`. Both halves therefore run library
 * code; only the byte plumbing is the test's.
 */
final class AcceptanceServer(backend: Int) extends AutoCloseable {
  private val server = new ServerSocket(0, 50, InetAddress.getLoopbackAddress)
  private val open = new ConcurrentLinkedQueue[TcpSocket]()

  daemon("okay2-http-test-acceptance") {
    while (!server.isClosed) {
      val c = server.accept()
      open.add(c): Unit
      daemon("okay2-http-test-acceptance-conn")(handle(c))
    }
  }

  def port: Int = server.getLocalPort

  def close(): Unit = {
    quietly(server.close())
    open.forEach(c => quietly(c.close()))
  }

  private def quietly(f: => Unit): Unit = try f catch { case _: Throwable => () }

  private def daemon(name: String)(body: => Unit): Unit = {
    val t = new Thread(() => quietly(body), name)
    t.setDaemon(true)
    t.start()
  }

  private def handle(c: TcpSocket): Unit = {
    val in = new DataInputStream(new BufferedInputStream(c.getInputStream))
    val out = c.getOutputStream
    val head = WsWire.readHead(in)
    if (WsWire.isUpgrade(head)) session(head, in, out) else pipe(head, in, out)
    c.close()
  }

  /**
   * REST: the head and everything after it to the backend, its answer
   * back. Both heads go with `Connection: close`, so this connection
   * carries ONE exchange: a kept-alive one would carry the client's next
   * request, a WebSocket handshake included, straight past the split.
   * Node's fetch pool does exactly that: the first run sent `/ws` down
   * the connection `/person` had opened, and the backend answered it.
   */
  private def pipe(head: String, in: InputStream, out: OutputStream): Unit = {
    val b = new TcpSocket(InetAddress.getLoopbackAddress, backend)
    open.add(b): Unit
    val bout = b.getOutputStream
    bout.write(closing(head))
    bout.flush()
    daemon("okay2-http-test-acceptance-up") { in.transferTo(bout): Unit; b.shutdownOutput() }
    val bin = new BufferedInputStream(b.getInputStream)
    out.write(closing(WsWire.readHead(bin)))
    bin.transferTo(out): Unit
    out.flush()
    b.close()
  }

  /** a head with its Connection header replaced by `close` */
  private def closing(head: String): Array[Byte] = {
    val lines = head.split("\r\n").toList.filterNot(_.toLowerCase.startsWith("connection:"))
    WsWire.headBytes(lines.mkString("", "\r\n", "\r\nConnection: close\r\n\r\n"))
  }

  /** WebSocket: the handshake, then `Acceptance.echo` over the socket */
  private def session(head: String, in: DataInputStream, out: OutputStream): Unit = {
    WsWire.accept(head, out)
    val q = Channel[Frame]()
    var closed = false
    def write(b0: Int, data: Array[Byte]): Unit = out.synchronized {
      if (!closed) {
        WsWire.write(out, b0, data)
        if ((b0 & 0x0f) == 0x8) closed = true
      }
    }

    daemon("okay2-http-test-acceptance-frames") {
      var reading = true
      val partial = scala.collection.mutable.ArrayBuilder.make[Byte]
      var partialOp = 0
      def deliver(op: Int, data: Array[Byte]): Unit =
        if (op == 0x1) q.offer(Frame.Text(new String(data, UTF_8))): Unit
        else q.offer(Frame.Binary(ArraySeq.unsafeWrapArray(data))): Unit
      while (reading) {
        WsWire.read(in) match {
          case None => reading = false
          case Some((fin, op, data)) => op match {
            case 0x8 =>
              val code = if (data.length >= 2) ((data(0) & 0xff) << 8) | (data(1) & 0xff) else Frame.Normal
              q.offer(Frame.Close(code, new String(data.drop(2), UTF_8))): Unit
              reading = false
            case 0x9 => write(0x8a, data)
            case 0x0 =>
              partial ++= data
              if (fin) { deliver(partialOp, partial.result()); partial.clear() }
            case 0x1 | 0x2 =>
              if (fin) deliver(op, data) else { partialOp = op; partial.clear(); partial ++= data }
            case _ => ()
          }
        }
      }
      q.close()
    }

    val socket = new Socket {
      def send(f: Frame): Unit ! Async = Async {
        f match {
          case Frame.Text(s) => write(0x81, s.getBytes(UTF_8))
          case Frame.Binary(b) => write(0x82, b.toArray)
          case Frame.Ping(b) => write(0x89, b.toArray)
          case Frame.Pong(b) => write(0x8a, b.toArray)
          case Frame.Close(code, r) => write(0x88, Array((code >> 8).toByte, code.toByte) ++ r.getBytes(UTF_8))
        }
      }
      def frames: Source[Frame] = Channel.ChannelOps(q).drained
      def close(code: Int, reason: String): Unit ! Async = send(Frame.Close(code, reason))
    }

    // the session ends when the client's frames do; its Close answered
    !.run(Async.run[Unit, Pure](Ws.over(socket)(Acceptance.echo).flatMap(_ => socket.close())))
  }
}
