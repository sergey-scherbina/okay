package okay2.http

import java.io.{DataInputStream, InputStream, OutputStream}
import java.net.{ServerSocket, Socket => TcpSocket}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64

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

  private def handshake(in: InputStream, out: OutputStream): Unit = {
    val header = new StringBuilder
    var done = false
    while (!done) {
      val c = in.read()
      if (c < 0) done = true
      else {
        header.append(c.toChar)
        val n = header.length
        if (n >= 4 && header.charAt(n - 1) == '\n' && header.charAt(n - 2) == '\r'
          && header.charAt(n - 3) == '\n' && header.charAt(n - 4) == '\r') done = true
      }
    }
    val key = header.toString.linesIterator.find(_.toLowerCase.startsWith("sec-websocket-key:"))
      .map(_.split(":", 2)(1).trim).getOrElse("")
    val accept = Base64.getEncoder.encodeToString(
      MessageDigest.getInstance("SHA-1").digest((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes(UTF_8)))
    out.write(("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" +
      s"Sec-WebSocket-Accept: $accept\r\n\r\n").getBytes(UTF_8))
    out.flush()
  }

  private def loop(in: DataInputStream, out: OutputStream): Unit = {
    var open = true
    val partial = scala.collection.mutable.ArrayBuilder.make[Byte]
    var partialOp = 0
    while (open) {
      val b0 = in.read()
      if (b0 < 0) open = false
      else {
        val opcode = b0 & 0x0f
        val b1 = in.read()
        val masked = (b1 & 0x80) != 0
        var len = (b1 & 0x7f).toLong
        if (len == 126) len = ((in.read() << 8) | in.read()).toLong
        else if (len == 127) {
          len = 0L
          var i = 0
          while (i < 8) { len = (len << 8) | in.read(); i += 1 }
        }
        val mask = if (masked) { val m = new Array[Byte](4); in.readFully(m); m } else null
        val data = new Array[Byte](len.toInt)
        in.readFully(data)
        if (masked) { var i = 0; while (i < data.length) { data(i) = (data(i) ^ mask(i % 4)).toByte; i += 1 } }
        val fin = (b0 & 0x80) != 0
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

  private def frame(out: OutputStream, b0: Int, data: Array[Byte]): Unit = {
    out.write(b0)
    if (data.length < 126) out.write(data.length)
    else if (data.length < 65536) { out.write(126); out.write(data.length >> 8); out.write(data.length & 0xff) }
    else {
      out.write(127)
      var i = 7
      while (i >= 0) { out.write(((data.length.toLong >> (i * 8)) & 0xff).toInt); i -= 1 }
    }
    out.write(data)
    out.flush()
  }
}
