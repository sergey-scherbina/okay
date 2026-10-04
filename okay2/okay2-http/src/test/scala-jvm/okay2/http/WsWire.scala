package okay2.http

import java.io.{DataInputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.security.MessageDigest
import java.util.Base64

/** the server side of RFC 6455 over plain streams, for the test servers
 * (WsEcho, AcceptanceServer): the request head, the handshake answer,
 * one frame read (unmasked) and one written (unmasked, as a server
 * sends) */
private[http] object WsWire {

  /** the request head up to and including the blank line, as text */
  def readHead(in: InputStream): String = {
    val head = new StringBuilder
    var done = false
    while (!done) {
      val c = in.read()
      if (c < 0) done = true
      else {
        head.append(c.toChar)
        val n = head.length
        if (n >= 4 && head.charAt(n - 1) == '\n' && head.charAt(n - 2) == '\r'
          && head.charAt(n - 3) == '\n' && head.charAt(n - 4) == '\r') done = true
      }
    }
    head.toString
  }

  def headBytes(head: String): Array[Byte] = head.getBytes(ISO_8859_1)

  def isUpgrade(head: String): Boolean =
    head.linesIterator.exists(l => l.toLowerCase.startsWith("upgrade:") && l.toLowerCase.contains("websocket"))

  /** answer the handshake `head` asked for */
  def accept(head: String, out: OutputStream): Unit = {
    val key = head.linesIterator.find(_.toLowerCase.startsWith("sec-websocket-key:"))
      .map(_.split(":", 2)(1).trim).getOrElse("")
    val acc = Base64.getEncoder.encodeToString(
      MessageDigest.getInstance("SHA-1").digest((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes(UTF_8)))
    out.write(("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n" +
      s"Sec-WebSocket-Accept: $acc\r\n\r\n").getBytes(UTF_8))
    out.flush()
  }

  /** one frame off the wire: (fin, opcode, payload unmasked); None at EOF */
  def read(in: DataInputStream): Option[(Boolean, Int, Array[Byte])] = {
    val b0 = in.read()
    if (b0 < 0) None
    else {
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
      Some(((b0 & 0x80) != 0, b0 & 0x0f, data))
    }
  }

  /** one frame, `b0` the FIN bit and opcode */
  def write(out: OutputStream, b0: Int, data: Array[Byte]): Unit = {
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
