package okay2.platform

import okay2._
import okay2.async._

/** the blocking socket behind `Async.Run`, where blocking is real (the
 * JVM's threads, Native's) — Scala.js has Node's `net` instead */
private final class SocketConn(sock: java.net.Socket) extends NetConn {
  private val in = new java.io.BufferedInputStream(sock.getInputStream)
  private val out = new java.io.BufferedOutputStream(sock.getOutputStream)

  def readFully(n: Int): Array[Byte] ! Async = Async {
    val buf = new Array[Byte](n)
    var at = 0
    while (at < n) {
      val r = in.read(buf, at, n - at)
      if (r < 0) throw NetEof(n, at)
      at += r
    }
    buf
  }

  def write(bytes: Array[Byte]): Unit ! Async = Async {
    out.write(bytes)
    out.flush()
  }

  def close(): Unit = sock.close()
}
