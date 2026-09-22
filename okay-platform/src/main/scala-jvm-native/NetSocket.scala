package okay

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.Socket

/**
 * The blocking leg (specs/net.md): one file for JVM and Native —
 * both ship java.net.Socket, and blocking behind Async.Run is
 * honest on both (virtual threads on the JVM; Native blocks the
 * thread it was given, which is the platform's truth).
 */
private final class SocketConn(sock: Socket) extends NetConn:
  private val in = BufferedInputStream(sock.getInputStream)
  private val out = BufferedOutputStream(sock.getOutputStream)

  def readFully(n: Int): Array[Byte] ! Async = async {
    val buf = new Array[Byte](n)
    var at = 0
    while at < n do
      val r = in.read(buf, at, n - at)
      if r < 0 then throw NetEof(n, at)
      at += r
    buf
  }

  def write(bytes: Array[Byte]): Unit ! Async = async {
    out.write(bytes)
    out.flush()
  }

  def close(): Unit = sock.close()

given Net = new Net:
  def connect(host: String, port: Int): NetConn ! Async = async {
    val s = Socket(host, port)
    s.setTcpNoDelay(true)
    SocketConn(s)
  }
