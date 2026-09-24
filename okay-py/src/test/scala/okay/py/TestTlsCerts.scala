package okay.py

import java.nio.file.{Files, Path}

/** certificates for the TLS suites (wire-tls), made by openssl once per run */
object TestTlsCerts:
  final case class Pair(cert: Path, key: Path)

  private def openssl(args: String*): Boolean =
    scala.util.Try(ProcessBuilder(("openssl" +: args)*).redirectErrorStream(true).start())
      .map { p => p.getInputStream.readAllBytes(); p.waitFor() == 0 }.getOrElse(false)

  private lazy val dir = Files.createTempDirectory("okay-tls")

  private def make(name: String, san: String): Option[Pair] =
    val cert = dir.resolve(s"$name.pem")
    val key = dir.resolve(s"$name-key.pem")
    Option.when(openssl("req", "-x509", "-newkey", "ec", "-pkeyopt", "ec_paramgen_curve:P-256", "-nodes",
      "-keyout", key.toString, "-out", cert.toString, "-days", "1", "-subj", s"/CN=$name",
      "-addext", s"subjectAltName=$san"))(Pair(cert, key))

  /** the server's: good for 127.0.0.1 and localhost */
  lazy val server: Option[Pair] = make("server", "IP:127.0.0.1,DNS:localhost")
  /** another self-signed certificate: a trust that does not cover the server */
  lazy val stranger: Option[Pair] = make("stranger", "IP:127.0.0.1,DNS:localhost")
  /** a certificate for ANOTHER name: a trusted chain whose name is wrong */
  lazy val elsewhere: Option[Pair] = make("elsewhere", "DNS:elsewhere.invalid")

  def available: Boolean = server.isDefined && stranger.isDefined && elsewhere.isDefined

/**
 * The TLS suite for one server binary (wire-tls): the whole conformance suite
 * over TLS, then each way TLS refuses, by name.
 */
abstract class TlsConformance extends WireConformance:
  import okay.codec.{WireAuth, WireSecurity}
  import okay.given
  import scala.concurrent.duration.*

  /** the binary serving TCP with `env` */
  def listen(env: Map[String, String]): (Int, Process)
  def serverAvailable: Boolean

  override def munitIgnore: Boolean = !serverAvailable || !TestTlsCerts.available
  private lazy val pair = TestTlsCerts.server.get
  private lazy val tlsEnv = Map("OKAY_TLS_CERT" -> pair.cert.toString, "OKAY_TLS_KEY" -> pair.key.toString)
  private lazy val served = listen(tlsEnv)

  given WireSecurity = WireSecurity.tls(WireSecurity.Trust.pem(TestTlsCerts.server.get.cert))
  lazy val engine: ForeignWorker = ForeignWorker.connect("127.0.0.1", served._1)
  override def afterAll(): Unit = if !munitIgnore then { engine.close(); served._2.destroy() }

  private def refusal(host: String, port: Int, security: WireSecurity): String =
    intercept[IllegalStateException](ForeignWorker.connect(host, port)(using summon[WireFormat], summon[WireCompression],
      WireAuth.Off, WireDeadline.after(2.seconds), security)).getMessage

  test("the server's NAME is checked, not only its chain: localhost is in its certificate") {
    val byName = ForeignWorker.connect("localhost", served._1)
    try assertEquals(byName.wire, engine.wire) finally byName.close()
  }

  test("a trust that does not cover the server's certificate is refused by name") {
    val e = refusal("127.0.0.1", served._1, WireSecurity.tls(WireSecurity.Trust.pem(TestTlsCerts.stranger.get.cert)))
    assert(e.contains("did not complete a TLS handshake"), e)
    assert(e.contains(s"the PEM file ${TestTlsCerts.stranger.get.cert}"), e)
  }

  test("a trusted certificate for ANOTHER name is refused: the name is checked") {
    val (port, p) = listen(Map("OKAY_TLS_CERT" -> TestTlsCerts.elsewhere.get.cert.toString,
      "OKAY_TLS_KEY" -> TestTlsCerts.elsewhere.get.key.toString))
    try
      val e = refusal("127.0.0.1", port, WireSecurity.tls(WireSecurity.Trust.pem(TestTlsCerts.elsewhere.get.cert)))
      assert(e.contains("did not complete a TLS handshake"), e)
    finally p.destroy()
  }

  test("a PLAIN host meeting a TLS server is told the server may speak TLS") {
    val e = refusal("127.0.0.1", served._1, WireSecurity.Plain)
    assert(e.contains("said nothing for 2000ms"), e)
    assert(e.contains("does it serve TLS? (this host's given WireSecurity is plain)"), e)
  }

  test("a TLS host meeting a PLAIN server is refused by name") {
    val (port, p) = listen(Map.empty)
    try
      val e = refusal("127.0.0.1", port, summon[WireSecurity])
      assert(e.contains("did not complete a TLS handshake"), e)
    finally p.destroy()
  }

  test("TLS and WireAuth compose: an encrypted wire that only a secret's holder may speak on") {
    val (port, p) = listen(tlsEnv + ("OKAY_WIRE_SECRET" -> "tea for two"))
    try
      given WireAuth = WireAuth.secret("tea for two".getBytes)
      val both = ForeignWorker.connect("127.0.0.1", port)
      try
        given okay.Handler[ForeignEval] = both.handler
        val priceOf = Foreign.callback[String, Double]("price_of")(_ => okay.Free.pure(4.0))
        val discount = Foreign.callback[Double, Double]("discount")(a => okay.Free.pure(a / 2))
        assertEquals(Foreign.fn[Double](address("quote")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L).runWith, Right(6.0))
      finally both.close()
    finally p.destroy()
  }
