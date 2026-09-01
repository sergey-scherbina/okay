package okay.pg

import okay.{!, Async, NetConn, NetEof, async}
import okay.crypto.Crypto
import okay.tls.{Tls, TlsConfig}
import okay.conf.Secrets
import java.net.Socket
import java.io.{BufferedInputStream, BufferedOutputStream}

/**
 * Postgres over TLS (specs/tls.md, the pg lane): the SSLRequest dance
 * lives HERE, in the driver, exactly as the box asks — and the
 * encrypted session it hands over is the ONE transport seam's
 * (okay-tls). Postgres does not do ALPN or a TLS port; it does a
 * STARTTLS-style preamble on the ordinary port: the client asks
 * "SSL?" with a magic request code, the server answers a single byte
 * — 'S' proceed, 'N' plaintext-only — and on 'S' the TLS handshake
 * runs over the same socket before the startup message. After that,
 * everything is the plaintext driver: `PgSql.connectOver` runs the
 * startup + SCRAM over the encrypted `NetConn` and never learns it
 * was encrypted, which is the seam's whole point.
 *
 * JVM only: `okay-tls` is `SSLSocket` on the JVM. The JS leg (Node)
 * would use node:tls behind the same idea when named.
 */
object PgTls:

  /** the SSLRequest magic (PG protocol): an 8-byte message, length 8
   * then this code, and nothing else — it precedes the startup */
  private val SslRequestCode = 80877103

  /**
   * Connect to Postgres over TLS. `cfg.mode` is the sslmode ladder
   * (VerifyFull the honest default); a server that answers 'N' when
   * encryption was asked for is refused BY NAME. `secrets` resolves a
   * client-key ref only if mTLS is configured (staged); server-auth
   * TLS needs none.
   */
  def connect(host: String, port: Int, user: String, password: String,
              database: String, cfg: TlsConfig = TlsConfig(),
              secrets: Secrets = Secrets.env)(using Crypto): PgSql ! Async =
    tlsConn(host, port, cfg, secrets).flatMap(conn =>
      PgSql.connectOver(conn, user, password, database))

  /** the dance, then the wrap: a blocking preamble on the raw socket
   * (behind Async.Run, the driver's waist), then the seam's SSLSocket */
  private def tlsConn(host: String, port: Int, cfg: TlsConfig,
                      secrets: Secrets): NetConn ! Async =
    async {
      val raw = Socket(host, port)
      raw.setTcpNoDelay(true)
      // SSLRequest: Int32(8) length, Int32(80877103) code
      val req = new Array[Byte](8)
      req(3) = 8
      req(4) = ((SslRequestCode >> 24) & 0xff).toByte
      req(5) = ((SslRequestCode >> 16) & 0xff).toByte
      req(6) = ((SslRequestCode >> 8) & 0xff).toByte
      req(7) = (SslRequestCode & 0xff).toByte
      val out = raw.getOutputStream
      out.write(req); out.flush()
      // exactly ONE byte answers — read it raw so nothing over-reads
      // into the TLS handshake that follows
      raw.getInputStream.read() match
        case 'S' =>
          Tls.client(raw, host, cfg, secrets) match
            case Right(ssl) => PgTls.SocketConn(ssl)
            case Left(e) => raw.close(); throw PgError(s"pg TLS handshake with '$host' failed: $e")
        case 'N' =>
          raw.close()
          throw PgError(
            s"the server refused SSL (SSLRequest answered 'N'); sslmode=${cfg.mode.toString.toLowerCase} demands encryption")
        case other =>
          raw.close()
          throw PgError(s"the server's SSLRequest reply was not 'S'/'N' but byte $other")
    }

  /** a NetConn over any socket (the raw one or the SSLSocket) — the
   * core's own SocketConn is private, so the driver carries its own,
   * identical, blocking-behind-Async shape */
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
      out.write(bytes); out.flush()
    }
    def close(): Unit = sock.close()
