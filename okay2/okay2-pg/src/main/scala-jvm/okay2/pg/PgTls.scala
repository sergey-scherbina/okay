package okay2.pg

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.Socket
import okay2.!
import okay2.async.Async
import okay2.crypto.Crypto
import okay2.platform.{NetConn, NetEof}

/**
 * Postgres over TLS (okay-pg's PgTls.scala, specs/tls.md): the
 * SSLRequest dance lives HERE, in the driver. Postgres does a
 * STARTTLS-style preamble on the ordinary port: the client asks "SSL?"
 * with a magic request code, the server answers ONE byte — 'S' proceed,
 * 'N' plaintext-only — and on 'S' the TLS handshake runs over the same
 * socket before the startup message. After that, `PgSql.connectOver`
 * runs the startup + SCRAM over the encrypted `NetConn` and never learns
 * it was encrypted. JVM only: `SSLSocket`.
 */
object PgTls {

  /** the SSLRequest magic: an 8-byte message, length 8 then this code */
  private val SslRequestCode = 80877103

  /** connect over TLS; a server that answers 'N' when encryption was
   * asked for is refused BY NAME */
  def connect(host: String, port: Int, user: String, password: String, database: String,
              cfg: TlsConfig = TlsConfig())(implicit c: Crypto): PgSql ! Async =
    tlsConn(host, port, cfg).flatMap(conn => PgSql.connectOver(conn, user, password, database))

  /** the dance, then the wrap: a blocking preamble on the raw socket
   * (behind Async.Run), then the SSLSocket */
  private def tlsConn(host: String, port: Int, cfg: TlsConfig): NetConn ! Async =
    Async {
      val raw = new Socket(host, port)
      raw.setTcpNoDelay(true)
      val req = new Array[Byte](8)
      req(3) = 8
      req(4) = ((SslRequestCode >> 24) & 0xff).toByte
      req(5) = ((SslRequestCode >> 16) & 0xff).toByte
      req(6) = ((SslRequestCode >> 8) & 0xff).toByte
      req(7) = (SslRequestCode & 0xff).toByte
      val out = raw.getOutputStream
      out.write(req); out.flush()
      // exactly ONE byte answers — read it raw so nothing over-reads into
      // the TLS handshake that follows
      raw.getInputStream.read() match {
        case 'S' =>
          Tls.client(raw, host, cfg) match {
            case Right(ssl) => new SocketConn(ssl): NetConn
            case Left(e) => raw.close(); throw PgError(s"pg TLS handshake with '$host' failed: $e")
          }
        case 'N' =>
          raw.close()
          throw PgError(
            s"the server refused SSL (SSLRequest answered 'N'); sslmode=${Tls.modeName(cfg.mode)} demands encryption")
        case other =>
          raw.close()
          throw PgError(s"the server's SSLRequest reply was not 'S'/'N' but byte $other")
      }
    }

  /** a NetConn over any socket (the raw one or the SSLSocket) */
  private final class SocketConn(sock: Socket) extends NetConn {
    private val in = new BufferedInputStream(sock.getInputStream)
    private val out = new BufferedOutputStream(sock.getOutputStream)
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
    def write(bytes: Array[Byte]): Unit ! Async = Async { out.write(bytes); out.flush() }
    def close(): Unit = sock.close()
  }
}
