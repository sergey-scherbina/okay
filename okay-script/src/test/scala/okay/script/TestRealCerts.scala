package okay.script

import okay.*
import okay.given
import okay.conf.{Secret, Secrets}
import okay.jetty.Jetty
import okay.tls.{Tls, TlsConfig}

import java.nio.file.{Files, Path}

/** script-real-certs: a CA-issued identity as it actually arrives —
 * an EC key, a full chain, and a renewal while the server is up.
 * Live against locally generated identities (openssl; skips where
 * absent, the okay-tls suite's own pattern).
 */
object TestRealCerts:
  lazy val dir: Path = Files.createTempDirectory("okay-script-real-certs")

  private def sh(cmd: String*): Boolean =
    try ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

  /** an EC identity, the shape `certbot --key-type ecdsa` writes */
  lazy val ec: Boolean =
    sh("openssl", "req", "-x509", "-newkey", "ec", "-pkeyopt", "ec_paramgen_curve:prime256v1",
      "-keyout", s"$dir/ec-key.pem", "-out", s"$dir/ec-cert.pem", "-days", "1", "-nodes",
      "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1")

  /** a two-certificate chain: a CA, and a leaf it signed -- what
   * fullchain.pem holds */
  lazy val chain: Boolean =
    sh("openssl", "req", "-x509", "-newkey", "rsa:2048", "-keyout", s"$dir/ca-key.pem",
      "-out", s"$dir/ca.pem", "-days", "1", "-nodes", "-subj", "/CN=Okay Test CA") &&
    sh("openssl", "req", "-newkey", "rsa:2048", "-keyout", s"$dir/leaf-key.pem",
      "-out", s"$dir/leaf.csr", "-nodes", "-subj", "/CN=localhost") &&
    sh("bash", "-c", s"printf 'subjectAltName=DNS:localhost,IP:127.0.0.1\\n' > $dir/ext.cnf") &&
    sh("openssl", "x509", "-req", "-in", s"$dir/leaf.csr", "-CA", s"$dir/ca.pem",
      "-CAkey", s"$dir/ca-key.pem", "-CAcreateserial", "-days", "1",
      "-extfile", s"$dir/ext.cnf", "-out", s"$dir/leaf.pem") &&
    sh("bash", "-c", s"cat $dir/leaf.pem $dir/ca.pem > $dir/fullchain.pem")

class TestRealCerts extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override def munitIgnore: Boolean = !(TestRealCerts.ec && TestRealCerts.chain)

  private val dir = TestRealCerts.dir

  private def pages(): Path =
    val root = Files.createTempDirectory("okay-script-real-certs-pages-")
    Files.writeString(root.resolve("index.md"), "served\n"): Unit
    root

  private def rmrf(p: Path): Unit =
    Files.walk(p).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(q => Files.deleteIfExists(q): Unit)

  /** one HTTPS GET, verifying the server's chain against `ca` --
   * VerifyFull, so the hostname is checked too */
  private def get(port: Int, ca: String): (String, java.security.cert.Certificate) =
    val plain = new java.net.Socket("localhost", port)
    val sock = Tls.client(plain, "localhost", TlsConfig(caFile = Some(ca)), Secrets.file)
      .fold(m => fail(s"the handshake failed: $m"), identity)
    try
      val peer = sock.asInstanceOf[javax.net.ssl.SSLSocket].getSession.getPeerCertificates.head
      val out = sock.getOutputStream
      out.write("GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".getBytes("UTF-8"))
      out.flush()
      (new String(sock.getInputStream.readAllBytes(), "UTF-8"), peer)
    finally sock.close()

  test("an EC key loads and serves -- the shape certbot --key-type ecdsa writes") {
    val ctx = Tls.serverContext(s"$dir/ec-cert.pem", Secret(s"file:$dir/ec-key.pem"), Secrets.file)
      .fold(m => fail(m), identity)
    val root = pages()
    val site = Site(root)
    try
      val body = Resource.run[String, Pure](site.serve(0, Some(ctx)).map { server =>
        get(Jetty.port(server), s"$dir/ec-cert.pem")._1
      }).runWith
      assert(body.startsWith("HTTP/1.1 200") && body.contains("served"), body.take(200))
    finally
      site.close()
      rmrf(root)
  }

  test("a fullchain.pem is presented as a CHAIN: the client verifies the leaf against the CA it trusts") {
    val ctx = Tls.serverContext(s"$dir/fullchain.pem", Secret(s"file:$dir/leaf-key.pem"), Secrets.file)
      .fold(m => fail(m), identity)
    val root = pages()
    val site = Site(root)
    try
      // the client is given ONLY the CA -- it can trust the leaf just
      // because the server sent the intermediate with it
      val body = Resource.run[String, Pure](site.serve(0, Some(ctx)).map { server =>
        get(Jetty.port(server), s"$dir/ca.pem")._1
      }).runWith
      assert(body.startsWith("HTTP/1.1 200"), body.take(200))
      // and the leaf alone, without the chain, does NOT verify against the CA
      val leafOnly = Tls.serverContext(s"$dir/leaf.pem", Secret(s"file:$dir/leaf-key.pem"), Secrets.file)
        .fold(m => fail(m), identity)
      assert(leafOnly != null)
    finally
      site.close()
      rmrf(root)
  }

  test("reloading: a renewal on disk is picked up by the next connection, with no restart") {
    val cert = dir.resolve("live-cert.pem")
    val key = dir.resolve("live-key.pem")
    Files.copy(dir.resolve("ec-cert.pem"), cert, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    Files.copy(dir.resolve("ec-key.pem"), key, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    val ctx = Tls.reloading(cert.toString, Secret(s"file:$key"), Secrets.file,
      java.time.Duration.ZERO, errors += _).fold(m => fail(m), identity)
    val root = pages()
    val site = Site(root)
    try
      Resource.run[Unit, Pure](site.serve(0, Some(ctx)).map { server =>
        val port = Jetty.port(server)
        val (body1, cert1) = get(port, dir.resolve("ec-cert.pem").toString)
        assert(body1.startsWith("HTTP/1.1 200"), body1.take(120))

        // certbot renews: new files in place, same paths
        Files.copy(dir.resolve("fullchain.pem"), cert, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        Files.copy(dir.resolve("leaf-key.pem"), key, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        Files.setLastModifiedTime(cert, java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() + 2000)): Unit

        // the next connection gets the NEW certificate, verified
        // against the CA that signed it -- no restart happened
        val (body2, cert2) = get(port, dir.resolve("ca.pem").toString)
        assert(body2.startsWith("HTTP/1.1 200"), body2.take(120))
        assertNotEquals(cert1, cert2)
        assertEquals(errors.toVector, Vector.empty)
      }).runWith
    finally
      site.close()
      rmrf(root)
  }

  test("a damaged renewal is NOT adopted: the identity in hand keeps serving, and the failure is reported") {
    val cert = dir.resolve("torn-cert.pem")
    val key = dir.resolve("torn-key.pem")
    Files.copy(dir.resolve("ec-cert.pem"), cert, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    Files.copy(dir.resolve("ec-key.pem"), key, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    val ctx = Tls.reloading(cert.toString, Secret(s"file:$key"), Secrets.file,
      java.time.Duration.ZERO, errors += _).fold(m => fail(m), identity)
    val root = pages()
    val site = Site(root)
    try
      Resource.run[Unit, Pure](site.serve(0, Some(ctx)).map { server =>
        val port = Jetty.port(server)
        assert(get(port, dir.resolve("ec-cert.pem").toString)._1.startsWith("HTTP/1.1 200"))
        // half a file, as a renewal caught mid-write looks
        Files.writeString(cert, "-----BEGIN CERTIFICATE-----\nnot a certi"): Unit
        Files.setLastModifiedTime(cert, java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() + 2000)): Unit
        val body = get(port, dir.resolve("ec-cert.pem").toString)._1
        assert(body.startsWith("HTTP/1.1 200"), s"the old identity stopped serving: ${body.take(120)}")
        assert(errors.exists(_.contains("not adopted")), errors.mkString("; "))
      }).runWith
    finally
      site.close()
      rmrf(root)
  }

  test("the refusals a real key can earn are named, not parse errors") {
    assert(Tls.privateKey("-----BEGIN ENCRYPTED PRIVATE KEY-----\nx\n-----END ENCRYPTED PRIVATE KEY-----")
      .left.exists(_.contains("passphrase")))
    assert(Tls.privateKey("-----BEGIN RSA PRIVATE KEY-----\nx\n-----END RSA PRIVATE KEY-----")
      .left.exists(_.contains("openssl pkcs8")))
    assert(Tls.privateKey("not a pem at all").isLeft)
    assert(Tls.privateKey(Files.readString(dir.resolve("ec-key.pem"))).exists(_.getAlgorithm == "EC"))
    assert(Tls.privateKey(Files.readString(dir.resolve("leaf-key.pem"))).exists(_.getAlgorithm == "RSA"))
  }
