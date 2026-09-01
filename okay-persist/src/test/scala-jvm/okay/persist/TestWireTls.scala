package okay.persist

import okay.{!, Async}
import okay.given
import okay.tls.{Tls, TlsConfig}
import okay.conf.{Secret, Secrets}
import munit.FunSuite

/**
 * persist-wire over TLS (specs/tls.md, the persist-wire lane): the
 * SAME wire acceptance, but every byte encrypted by the ONE transport
 * seam. The point of the design is proven by what does NOT change —
 * the handshake, the capability grant, the frames, the refusals are
 * byte-for-byte the plaintext behaviour; TLS wraps the TRANSPORT
 * underneath. okay-persist stays dependency-free: the SSLServerSocket
 * and the client SSLSocket are built HERE (test scope) via okay-tls,
 * and handed to the wire through its injectable transport.
 *
 * Live against a locally generated identity (openssl, on every dev
 * box; skips where absent — the okay-tls suite's own pattern).
 */
object TestWireTls:
  lazy val dir = java.nio.file.Files.createTempDirectory("okay-wire-tls")
  lazy val generated: Boolean =
    try
      val cmd = Array("openssl", "req", "-x509", "-newkey", "rsa:2048",
        "-keyout", s"$dir/key.pem", "-out", s"$dir/cert.pem",
        "-days", "1", "-nodes", "-subj", "/CN=localhost",
        "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1")
      ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

class TestWireTls extends FunSuite:

  override def munitIgnore: Boolean = !TestWireTls.generated

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")
  private def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  private def cert = s"${TestWireTls.dir}/cert.pem"
  private def key = Secret(s"file:${TestWireTls.dir}/key.pem")

  /** a TLS wire server over the seam's own SSLServerSocket */
  private def tlsServer(store: Store): Wire.Server =
    val ss = Tls.serverSocket(0, cert, key, Secrets.file)
      .fold(e => throw IllegalStateException(e), identity)
    Wire.Server(store, {
      case "reader-of-events" => Some(Set("events"))
      case "the-admin" => Some(Set("events", "audit"))
      case _ => None
    }, socket = Some(ss))

  /** a TLS client: verify-full against the self-signed cert as its own
   * CA, the host the cert names */
  private def tlsConnect(port: Int, token: String): Wire.Remote =
    Wire.Remote.connect("localhost", port, token,
      wrap = s => Tls.client(s, "localhost", TlsConfig(caFile = Some(cert)), Secrets.file)
        .fold(e => throw okay.persist.Wire.WireRefused(s"TLS: $e"), identity))

  test("the encrypted handshake grants exactly the token's topics") {
    val srv = tlsServer(MemoryStore())
    try
      val c = tlsConnect(srv.port, "the-admin")
      assertEquals(c.topics, Vector("audit", "events"))
      c.close()
    finally srv.close()
  }

  test("append and read round-trip over TLS: bytes, offsets, the capability rule — all unchanged") {
    val store = MemoryStore()
    val srv = tlsServer(store)
    try
      val c = tlsConnect(srv.port, "the-admin")
      try
        val o0 = run(c.append("events", 0, bytes("k0"), bytes("v0")))
        val o1 = run(c.append("events", 0, Array.empty, bytes("v1")))
        assertEquals((o0, o1), (0L, 1L))
        assertEquals(run(c.end("events", 0)), 2L)
        run(c.read("events", 0, 0L, 10)) match
          case Topic.Read.Records(rs) =>
            assertEquals(rs.map(r => str(r.value)), Vector("v0", "v1"))
          case other => fail(s"unexpected $other")
        // the same server store saw it — one truth, encrypted in flight
        assertEquals(store.topic("events").end(0), 2L)
      finally c.close()
    finally srv.close()
  }

  test("a topic off the capability list refuses by name over TLS; the connection survives") {
    val srv = tlsServer(MemoryStore())
    try
      val c = tlsConnect(srv.port, "reader-of-events")
      try
        val e = intercept[Wire.WireRefused](
          run(c.append("audit", 0, Array.empty, bytes("sneak"))))
        assert(e.reason.contains("audit"), e.reason)
        assertEquals(run(c.append("events", 0, Array.empty, bytes("fine"))), 0L)
      finally c.close()
    finally srv.close()
  }

  test("a PLAINTEXT client is refused by the TLS server: encryption is required, not optional") {
    val srv = tlsServer(MemoryStore())
    try
      // no wrap: raw bytes into an SSL server — the handshake cannot
      // complete, so the session never opens
      intercept[Throwable] {
        val c = Wire.Remote.connect("localhost", srv.port, "the-admin")
        run(c.append("events", 0, Array.empty, bytes("plain")))
      }
    finally srv.close()
  }
