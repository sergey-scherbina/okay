package okay.tls

import okay.conf.{Secret, Secrets}
import java.net.{ServerSocket, Socket}

/**
 * The sslmode ladder, each rung proven against a LIVE handshake with
 * a locally generated identity (openssl, present on every dev box
 * this runs on; the suite skips where it is not): verify-full
 * refuses the wrong hostname and the unknown CA by name, verify-ca
 * accepts the wrong hostname AND THE TEST SAYS SO OUT LOUD, require
 * refuses plaintext, disable is the named decision it is.
 */
object TestTls:
  lazy val dir = java.nio.file.Files.createTempDirectory("okay-tls")
  lazy val generated: Boolean =
    try
      val cmd = Array("openssl", "req", "-x509", "-newkey", "rsa:2048",
        "-keyout", s"$dir/key.pem", "-out", s"$dir/cert.pem",
        "-days", "1", "-nodes", "-subj", "/CN=localhost",
        "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1")
      ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

class TestTls extends munit.FunSuite {

  override def munitIgnore: Boolean = !TestTls.generated

  def cert = s"${TestTls.dir}/cert.pem"
  def key = Secret(s"file:${TestTls.dir}/key.pem")

  /** a one-shot echo server over the seam's own server half */
  def served[A](body: Int => A): A =
    val ss = Tls.serverSocket(0, cert, key, Secrets.file)
      .fold(e => throw IllegalStateException(e), identity)
    val t = new Thread(() =>
      try
        val s = ss.accept()
        val in = s.getInputStream; val out = s.getOutputStream
        val b = new Array[Byte](64); val n = in.read(b)
        if n > 0 then { out.write(b, 0, n); out.flush() }
        s.close()
      catch case _: Exception => ())
    t.setDaemon(true); t.start()
    try body(ss.getLocalPort) finally ss.close()

  def echo(s: Socket): String =
    s.getOutputStream.write("ping".getBytes); s.getOutputStream.flush()
    val b = new Array[Byte](64); val n = s.getInputStream.read(b)
    String(b, 0, n, "UTF-8")

  test("verify-full with the CA: the handshake completes and bytes flow") {
    served { port =>
      val out = Tls.client(Socket("localhost", port), "localhost",
        TlsConfig(caFile = Some(cert)))
      assertEquals(out.map(echo), Right("ping"))
    }
  }

  test("verify-full refuses a wrong hostname, named") {
    served { port =>
      val out = Tls.client(Socket("127.0.0.1", port), "wrong.example",
        TlsConfig(caFile = Some(cert)))
      assert(out.left.exists(_.contains("wrong.example")), out.toString)
    }
  }

  test("verify-full refuses an unknown CA (the platform store does not know ours)") {
    served { port =>
      val out = Tls.client(Socket("localhost", port), "localhost", TlsConfig())
      assert(out.isLeft, out.toString)
    }
  }

  test("verify-ca accepts the wrong hostname — SAYING SO OUT LOUD: the chain is checked, the name is NOT") {
    served { port =>
      val out = Tls.client(Socket("127.0.0.1", port), "wrong.example",
        TlsConfig(mode = SslMode.VerifyCa, caFile = Some(cert)))
      assertEquals(out.map(echo), Right("ping"))
    }
  }

  test("require encrypts without identity; against PLAINTEXT it refuses") {
    served { port =>
      val out = Tls.client(Socket("localhost", port), "localhost",
        TlsConfig(mode = SslMode.Require))
      assertEquals(out.map(echo), Right("ping"))
    }
    // a plaintext server answers no handshake
    val plain = ServerSocket(0)
    val t = new Thread(() => try { val s = plain.accept(); s.getInputStream.read(); s.close() } catch { case _: Exception => () })
    t.setDaemon(true); t.start()
    val out = Tls.client(Socket("localhost", plain.getLocalPort), "localhost",
      TlsConfig(mode = SslMode.Require))
    assert(out.isLeft, out.toString)
    plain.close()
  }

  test("disable connects in the clear — the named decision it is") {
    val plain = ServerSocket(0)
    val t = new Thread(() =>
      try
        val s = plain.accept()
        val b = new Array[Byte](64); val n = s.getInputStream.read(b)
        s.getOutputStream.write(b, 0, n); s.close()
      catch case _: Exception => ())
    t.setDaemon(true); t.start()
    val out = Tls.client(Socket("localhost", plain.getLocalPort), "localhost",
      TlsConfig(mode = SslMode.Disable))
    assertEquals(out.map(echo), Right("ping"))
    plain.close()
  }

  test("a private key smuggled inline in the ref is refused, client and server") {
    val inline = Secret("-----BEGIN PRIVATE KEY-----\nMIIE...")
    val c = Tls.client(Socket(), "h", TlsConfig(clientKey = Some(inline)))
    assert(c.left.exists(_.contains("INLINE")), c.toString)
    val s = Tls.serverSocket(0, cert, inline, Secrets.file)
    assert(s.left.exists(_.contains("INLINE")), s.toString)
  }
}
