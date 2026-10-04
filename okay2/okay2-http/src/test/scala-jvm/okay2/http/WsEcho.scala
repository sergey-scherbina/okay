package okay2.http

import java.io.{DataInputStream, InputStream, OutputStream}
import java.net.{ServerSocket, Socket => TcpSocket}
import java.nio.charset.StandardCharsets.UTF_8

/** a test-scope RFC 6455 echo server over a plain socket (okay-http's
 * WsEcho): the library does not serve WebSocket; this one handshakes,
 * unmasks, echoes (optionally in fragments), answers a ping, and may
 * send parting words before it answers a Close */
final class WsEcho(val fragmentEvery: Int = 0, val partingWords: Int = 0) extends AutoCloseable {
  private val server = new ServerSocket(0)
  @volatile private var client: TcpSocket = null
  private val thread = new Thread(() => try serve() catch { case _: Throwable => () }, "okay2-http-test-wsecho")
  thread.setDaemon(true)
  thread.start()

  def port: Int = server.getLocalPort
  def url: String = s"ws://127.0.0.1:$port"

  def close(): Unit = {
    try server.close() catch { case _: Throwable => () }
    if (client != null) try client.close() catch { case _: Throwable => () }
  }

  private def serve(): Unit = {
    val s = server.accept()
    client = s
    val in = new DataInputStream(s.getInputStream)
    val out = s.getOutputStream
    handshake(in, out)
    loop(in, out)
  }

  private def handshake(in: InputStream, out: OutputStream): Unit = WsWire.accept(WsWire.readHead(in), out)

  private def loop(in: DataInputStream, out: OutputStream): Unit = {
    var open = true
    val partial = scala.collection.mutable.ArrayBuilder.make[Byte]
    var partialOp = 0
    while (open) {
      WsWire.read(in) match {
        case None => open = false
        case Some((fin, opcode, data)) =>
          opcode match {
            case 0x8 =>
              var w = 0
              while (w < partingWords) { send(out, 0x1, s"parting-$w".getBytes(UTF_8)); w += 1 }
              send(out, 0x8, data); open = false
            case 0x9 => send(out, 0xa, data)
            case 0xa => ()
            case 0x0 =>
              partial ++= data
              if (fin) { echo(out, partialOp, partial.result()); partial.clear() }
            case 0x1 | 0x2 =>
              if (fin) echo(out, opcode, data)
              else { partialOp = opcode; partial.clear(); partial ++= data }
            case _ => ()
          }
      }
    }
  }

  private def echo(out: OutputStream, opcode: Int, data: Array[Byte]): Unit =
    if (fragmentEvery > 0 && data.length > fragmentEvery) {
      val parts = data.grouped(fragmentEvery).toVector
      parts.zipWithIndex.foreach { case (p, i) =>
        frame(out, (if (i == parts.length - 1) 0x80 else 0x00) | (if (i == 0) opcode else 0x00), p)
      }
    } else send(out, opcode, data)

  private def send(out: OutputStream, opcode: Int, data: Array[Byte]): Unit = frame(out, 0x80 | opcode, data)

  private def frame(out: OutputStream, b0: Int, data: Array[Byte]): Unit = WsWire.write(out, b0, data)
}
