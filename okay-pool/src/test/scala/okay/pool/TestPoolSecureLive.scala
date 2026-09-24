package okay.pool

import okay.cluster.{Req, Resp, Served}
import okay.conf.{Secret, Secrets}

/**
 * mTLS between two real pool members, over a real socket
 * (specs/cluster-pool.md, stage 4) — the same openssl-generated
 * identity `okay-tls`'s own `TestTls` already trusts, `Live` for the
 * same reason: a subprocess builds the certificate.
 */
object TestPoolSecureLive:
  lazy val dir = java.nio.file.Files.createTempDirectory("okay-pool-tls")

  private def gen(name: String): Boolean =
    try
      val cmd = Array("openssl", "req", "-x509", "-newkey", "rsa:2048",
        "-keyout", s"$dir/$name-key.pem", "-out", s"$dir/$name-cert.pem",
        "-days", "1", "-nodes", "-subj", s"/CN=$name")
      ProcessBuilder(cmd*).redirectErrorStream(true).start().waitFor() == 0
    catch case _: Exception => false

  lazy val generated: Boolean = gen("pool") && gen("stranger")

class TestPoolSecureLive extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !TestPoolSecureLive.generated

  CountJobs.install()

  private val dir = TestPoolSecureLive.dir
  private def cert(name: String) = s"$dir/$name-cert.pem"
  private def key(name: String) = Secret(s"file:$dir/$name-key.pem")

  private def server(): (java.net.ServerSocket, Int) =
    val ss = okay.tls.Tls.mutualServerSocket(0, cert("pool"), key("pool"), Secrets.file)
      .fold(e => throw IllegalStateException(e), identity)
    val t = new Thread(() => Served.serve(ss, Pool.fingerprinted("build-x")))
    t.setDaemon(true); t.start()
    (ss, ss.getLocalPort)

  test("two members holding the SAME certificate: the worker protocol round-trips") {
    val (ss, port) = server()
    try
      val serve = Served.reconnecting("localhost", port, connect = (h, p) =>
        val plain = Served.plainSocket(h, p)
        okay.tls.Tls.mutualClient(plain, h, cert("pool"), key("pool"), Secrets.file)
          .fold(e => throw java.io.IOException(e), identity))
      serve(Req.Known) match
        case Resp.Names(names, build) =>
          assert(names.contains(CountJob.name), names.toString)
          assertEquals(build, "build-x")
        case other => fail(s"expected Resp.Names, got $other")
    finally ss.close()
  }

  test("a DIFFERENT certificate is refused at the handshake, not at the request") {
    val (ss, port) = server()
    try
      val serve = Served.reconnecting("localhost", port, connect = (h, p) =>
        val plain = Served.plainSocket(h, p)
        okay.tls.Tls.mutualClient(plain, h, cert("stranger"), key("stranger"), Secrets.file)
          .fold(e => throw java.io.IOException(e), identity))
      intercept[java.io.IOException](serve(Req.Known)): Unit
    finally ss.close()
  }

  test("plain TLS with no client identity at all is refused: this server REQUIRES one") {
    val (ss, port) = server()
    try
      // NOT observable at the client's own handshake: under TLS 1.3
      // (RFC 8446 §4.4.2) a client presenting no certificate still
      // completes ITS side of the handshake, and this JDK's SunJSSE
      // does exactly that -- measured here first, before
      // `Tls.mutualServerSocket` grew its own `getPeerCertificates`
      // check. The refusal only shows up once the server, having
      // verified there is no peer certificate, closes rather than
      // answers -- which the worker protocol's own round-trip proves,
      // the same way the wrong-certificate case above does.
      val serve = Served.reconnecting("localhost", port, connect = (h, p) =>
        val plain = Served.plainSocket(h, p)
        okay.tls.Tls.client(plain, h,
          okay.tls.TlsConfig(mode = okay.tls.SslMode.VerifyCa, caFile = Some(cert("pool"))))
          .fold(e => throw java.io.IOException(e), identity))
      intercept[java.io.IOException](serve(Req.Known)): Unit
    finally ss.close()
  }
