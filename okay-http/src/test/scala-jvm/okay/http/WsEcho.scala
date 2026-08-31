package okay.http

import java.io.{DataInputStream, InputStream, OutputStream}
import java.net.{ServerSocket, Socket as TcpSocket}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64

/**
 * A WebSocket echo server, for tests only.
 *
 * The library does not serve WebSocket — the JDK has no server-side API
 * and `HttpServer` will not surrender its socket, so specs/http.md puts
 * serving out of scope rather than half-building it. But the CLIENT
 * transport deserves to be tested against a real socket rather than a
 * mock, and RFC 6455's handshake and framing are eighty lines, so here
 * they are, in test scope where they belong.
 *
 * It speaks only what the tests need: the handshake, unmasked
 * server-to-client frames, masked client-to-server ones, text, binary,
 * ping/pong and close. No fragmentation on the way out, no extensions,
 * no TLS.
 */
final class WsEcho(val fragmentEvery: Int = 0) extends AutoCloseable {
  private val server = ServerSocket(0)
  @volatile private var client: TcpSocket = null
  private val thread = Thread.startVirtualThread(() => try serve() catch case _: Throwable => ())

  def port: Int = server.getLocalPort
  def url: String = s"ws://127.0.0.1:$port"

  def close(): Unit =
    try server.close() catch case _: Throwable => ()
    if client != null then try client.close() catch case _: Throwable => ()

  private def serve(): Unit =
    val s = server.accept()
    client = s
    val in = DataInputStream(s.getInputStream)
    val out = s.getOutputStream
    handshake(in, out)
    loop(in, out)

  /** RFC 6455 §4.2.2: echo the key, SHA-1'd with the magic GUID */
  private def handshake(in: InputStream, out: OutputStream): Unit =
    val header = new StringBuilder
    var done = false
    while !done do
      val c = in.read()
      if c < 0 then done = true
      else
        header.append(c.toChar)
        val n = header.length
        if n >= 4 && header.charAt(n - 1) == '\n' && header.charAt(n - 2) == '\r'
          && header.charAt(n - 3) == '\n' && header.charAt(n - 4) == '\r' then done = true
    val key = header.toString.linesIterator
      .find(_.toLowerCase.startsWith("sec-websocket-key:"))
      .map(_.split(":", 2)(1).trim).getOrElse("")
    val accept = Base64.getEncoder.encodeToString(
      MessageDigest.getInstance("SHA-1")
        .digest((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes(UTF_8)))
    out.write(("HTTP/1.1 101 Switching Protocols\r\n" +
      "Upgrade: websocket\r\nConnection: Upgrade\r\n" +
      s"Sec-WebSocket-Accept: $accept\r\n\r\n").getBytes(UTF_8))
    out.flush()

  private def loop(in: DataInputStream, out: OutputStream): Unit =
    var open = true
    // a client may fragment: the JDK splits a long message itself, so
    // continuations have to be joined before they can be echoed
    val partial = scala.collection.mutable.ArrayBuilder.make[Byte]
    var partialOp = 0
    while open do
      val b0 = in.read()
      if b0 < 0 then open = false
      else
        val opcode = b0 & 0x0f
        val b1 = in.read()
        val masked = (b1 & 0x80) != 0
        var len = (b1 & 0x7f).toLong
        if len == 126 then len = ((in.read() << 8) | in.read()).toLong
        else if len == 127 then
          len = 0L
          var i = 0
          while i < 8 do { len = (len << 8) | in.read(); i += 1 }
        val mask = if masked then { val m = new Array[Byte](4); in.readFully(m); m } else null
        val data = new Array[Byte](len.toInt)
        in.readFully(data)
        if masked then
          var i = 0
          while i < data.length do { data(i) = (data(i) ^ mask(i % 4)).toByte; i += 1 }

        val fin = (b0 & 0x80) != 0
        opcode match
          case 0x8 => // close: answer and stop
            send(out, 0x8, data); open = false
          case 0x9 => send(out, 0xa, data)                  // ping -> pong
          case 0xa => ()                                     // pong: nothing owed
          case 0x0 =>                                        // a continuation
            partial ++= data
            if fin then
              echo(out, partialOp, partial.result())
              partial.clear()
          case 0x1 | 0x2 =>
            if fin then echo(out, opcode, data)
            else { partialOp = opcode; partial.clear(); partial ++= data }
          case _ => ()

  private def echo(out: OutputStream, opcode: Int, data: Array[Byte]): Unit =
    if fragmentEvery > 0 && data.length > fragmentEvery then
      // deliberately split, so the CLIENT's reassembly is exercised
      sendFragmented(out, opcode, data, fragmentEvery)
    else send(out, opcode, data)

  private def send(out: OutputStream, opcode: Int, data: Array[Byte]): Unit =
    frame(out, 0x80 | opcode, data)

  private def sendFragmented(out: OutputStream, opcode: Int,
                             data: Array[Byte], every: Int): Unit =
    val parts = data.grouped(every).toVector
    parts.zipWithIndex.foreach { (p, i) =>
      val first = i == 0
      val last = i == parts.length - 1
      // first frame carries the opcode, the rest are continuations (0x0)
      frame(out, (if last then 0x80 else 0x00) | (if first then opcode else 0x00), p)
    }

  private def frame(out: OutputStream, b0: Int, data: Array[Byte]): Unit =
    out.write(b0)
    if data.length < 126 then out.write(data.length)
    else if data.length < 65536 then
      out.write(126); out.write(data.length >> 8); out.write(data.length & 0xff)
    else
      out.write(127)
      var i = 7
      while i >= 0 do { out.write(((data.length.toLong >> (i * 8)) & 0xff).toInt); i -= 1 }
    out.write(data)
    out.flush()
}
