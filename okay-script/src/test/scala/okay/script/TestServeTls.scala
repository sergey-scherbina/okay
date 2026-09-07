package okay.script

import okay.*
import okay.given
import okay.conf.{Secret, Secrets}
import okay.jetty.Jetty
import okay.tls.{Tls, TlsConfig}

import java.nio.file.{Files, Path}

/** script-tls: a Site served over HTTPS -- the same pages, the one
 * transport seam (specs/tls.md) terminating TLS on Jetty's connector.
 * Live against a locally generated identity (openssl, the okay-tls
 * suite's own pattern; skips where absent).
 */
object TestServeTls:
  lazy val dir: Path = Files.createTempDirectory("okay-script-tls")
  lazy val generated: Boolean =
    try
      val cmd = Array("openssl", "req", "-x509", "-newkey", "rsa:2048",
        "-keyout", s"$dir/key.pem", "-out", s"$dir/cert.pem",
        "-days", "1", "-nodes", "-subj", "/CN=localhost",
        "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1")
      ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

class TestServeTls extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitIgnore: Boolean = !TestServeTls.generated

  private def cert = s"${TestServeTls.dir}/cert.pem"
  private def key = Secret(s"file:${TestServeTls.dir}/key.pem")

  test("Serve.parse: OKAY_TLS_CERT and OKAY_TLS_KEY are a pair, and half of one refuses by name") {
    val root = Files.createTempDirectory("okay-script-tls-pages-")
    try
      def parse(env: Map[String, String]) = Serve.parse(Array(root.toString), env.get)
      assertEquals(parse(Map.empty).map(_.tls), Right(None))
      assertEquals(parse(Map("OKAY_TLS_CERT" -> cert, "OKAY_TLS_KEY" -> key.ref)).map(_.tls.map(_._1)), Right(Some(cert)))
      assertEquals(parse(Map("OKAY_TLS_CERT" -> cert, "OKAY_TLS_KEY" -> key.ref)).map(_.scheme), Right("https"))
      assert(parse(Map("OKAY_TLS_CERT" -> cert)).left.exists(_.contains("without OKAY_TLS_KEY")))
      assert(parse(Map("OKAY_TLS_KEY" -> key.ref)).left.exists(_.contains("without OKAY_TLS_CERT")))
      // an INLINE key is refused by the seam itself, not stored and used
      val inline = Serve.Args(root, 0, None, Vector("en"), Some((cert, Secret("-----BEGIN PRIVATE KEY-----"))))
      assert(Serve.sslOf(inline).left.exists(_.contains("INLINE")), Serve.sslOf(inline).toString)
    finally Files.deleteIfExists(root): Unit
  }

  test("a page over HTTPS: the same Site, TLS on the connector") {
    val root = Files.createTempDirectory("okay-script-tls-pages-")
    Files.writeString(root.resolve("index.md"),
      "```scala\nimport okay.script.api.*\nSession.current.set(\"seen\", \"1\")\n```\nsecure hello (${Web.current.method})\n"): Unit
    val a = Serve.parse(Array(root.toString, "0"), Map("OKAY_TLS_CERT" -> cert, "OKAY_TLS_KEY" -> key.ref).get).toOption.get
    val ssl = Serve.sslOf(a, Secrets.file).fold(m => fail(m), identity)
    assert(ssl.isDefined)
    val site = Serve.site(a)
    try
      val body = Resource.run[String, Pure](site.serve(0, ssl).map { server =>
        val port = Jetty.port(server)
        // the CLIENT half of the same seam: VerifyFull against this
        // run's own CA file -- chain AND hostname, not a trust-all
        // shrug, so the certificate the server presented is checked
        val plain = new java.net.Socket("localhost", port)
        val sock = Tls.client(plain, "localhost", TlsConfig(caFile = Some(cert)), Secrets.file)
          .fold(m => fail(s"the client handshake failed: $m"), identity)
        try
          val out = sock.getOutputStream
          out.write("GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".getBytes("UTF-8"))
          out.flush()
          new String(sock.getInputStream.readAllBytes(), "UTF-8")
        finally sock.close()
      }).runWith
      assert(body.startsWith("HTTP/1.1 200"), body.take(120))
      assert(body.contains("secure hello (GET)"), body)
      // the connector terminates TLS, so the session cookie this
      // request set carries Secure without anyone claiming a header
      // (okay-script-cookie-flags)
      val cookie = body.linesIterator.find(_.toLowerCase.startsWith("set-cookie:")).getOrElse(fail(s"no Set-Cookie in\n$body"))
      assert(cookie.contains("Secure") && cookie.contains("HttpOnly") && cookie.contains("SameSite=Lax"), cookie)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
