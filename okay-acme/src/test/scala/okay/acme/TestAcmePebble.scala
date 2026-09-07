package okay.acme

import okay.*
import okay.given
import okay.http.{Http, Method, Request, Response}
import okay.jetty.Jetty
import okay.security.given

import java.nio.file.{Files, Path}

/**
 * The interop a test double cannot give: our client against PEBBLE,
 * Let's Encrypt's own small ACME server (acme-pebble).
 *
 * `FakeCa` proves the state machine — the order of the steps, the
 * nonce discipline, the poll — but it is OUR reading of the protocol
 * checking OUR writing of it. Pebble is someone else's
 * implementation, deliberately strict (it rejects a reused nonce, an
 * unknown JWS field, a bad signature, a CSR whose names do not match
 * the order), and running against it is the only way to learn that
 * our JWS, our POST-as-GET and our CSR are what the protocol says
 * rather than what we thought it said.
 *
 * Live-tagged and docker-dependent; skipped where docker is absent.
 * The domain is `host.docker.internal` so that Pebble, inside the
 * container, can reach the challenge server on this host — with
 * `--add-host` so Linux behaves the way Docker Desktop already does.
 */
object Pebble:
  val image = "ghcr.io/letsencrypt/pebble:latest"
  val domain = "host.docker.internal"

  /** the MAC key Pebble's own config ships for `kid-1` -- the pair a
   * commercial CA would hand an operator out of band */
  val eabKid = "kid-1"
  val eabKey = "zWNDZM6eQGHWpSRTPal5eIUYFTu7EajVIoguysqZ9wG44nMEtx3MUAsUDkMTQ12W"

  private def shell(cmd: Seq[String]): (Int, String) =
    val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes(), "UTF-8")
    (p.waitFor(), out)

  def available: Boolean = shell(Vector("docker", "info"))._1 == 0

  def freePort(): Int =
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()

  /**
   * ONE Pebble per test, on ITS OWN ports and under its own name.
   *
   * The first version shared a name and the fixed ports, and the
   * suite flaked: `docker rm -f` returns before the container is
   * gone, so the next test's client could reach the PREVIOUS Pebble
   * and present it a nonce the new one never issued — a badNonce that
   * looked like a client bug and was a fixture bug. Ports come from
   * the OS, the config is written per instance (Pebble reaches the
   * host's challenge server at `httpPort`, so that number has to be
   * ours too), and readiness is the API actually answering.
   */
  /**
   * pebble-challtestsrv: a DNS server Pebble can be pointed at, plus
   * an HTTP API to set records in it. It plays BOTH parts dns-01
   * needs -- the resolver the CA asks, and the "provider" our test
   * `Dns` writes to -- which is the only way to prove the challenge
   * without owning a real zone.
   */
  final class ChallTestSrv(val dnsPort: Int, val managePort: Int):
    val name = s"okay-acme-challtestsrv-$managePort"

    def start(): Boolean =
      val (code, out) = shell(Vector("docker", "run", "-d", "--rm", "--name", name,
        "-p", s"$dnsPort:8053/udp", "-p", s"$dnsPort:8053/tcp", "-p", s"$managePort:8055",
        "ghcr.io/letsencrypt/pebble-challtestsrv:latest"))
      if code != 0 then
        println(s"challtestsrv did not start: $out")
        false
      else (1 to 40).exists { _ =>
        Thread.sleep(300)
        try { val s = new java.net.Socket("127.0.0.1", managePort); s.close(); true }
        catch case _: Exception => false
      }

    def stop(): Unit = shell(Vector("docker", "rm", "-f", name)): Unit

  final class Instance(dir: Path, val eabRequired: Boolean = false, val dnsServer: Option[Int] = None):
    val apiPort: Int = freePort()
    val challengePort: Int = freePort()
    val name = s"okay-acme-pebble-$apiPort"
    val ca: Path = dir.resolve(s"pebble-ca-$apiPort.pem")

    private def config(): Path =
      val f = dir.resolve(s"pebble-$apiPort.json")
      val eab =
        if !eabRequired then ""
        else s""","externalAccountBindingRequired":true,"externalAccountMACKeys":{"$eabKid":"$eabKey"}"""
      Files.writeString(f,
        s"""{"pebble":{"listenAddress":"0.0.0.0:$apiPort","managementListenAddress":"0.0.0.0:15000",
           |"certificate":"test/certs/localhost/cert.pem","privateKey":"test/certs/localhost/key.pem",
           |"httpPort":$challengePort,"tlsPort":5001,"ocspResponderURL":""$eab}}""".stripMargin): Unit
      f

    def start(): Boolean =
      val cfg = config()
      // pointed at a DNS server, Pebble resolves EVERY name through it
      // -- which is what makes a dns-01 (and so a wildcard) test
      // possible without owning a zone
      val resolver = dnsServer.toVector.flatMap(p => Vector("-dnsserver", s"host.docker.internal:$p"))
      val (code, out) = shell(Vector("docker", "run", "-d", "--rm", "--name", name,
        "-p", s"$apiPort:$apiPort",
        "-e", "PEBBLE_VA_NOSLEEP=1",
        "--add-host=host.docker.internal:host-gateway",
        "-v", s"${cfg.toAbsolutePath}:/test/config/okay.json",
        image, "-config", "/test/config/okay.json") ++ resolver)
      if code != 0 then
        println(s"pebble did not start: $out")
        false
      else
        // its API is HTTPS under a CA it generates per run: pull that
        // out, and wait for the API to ANSWER -- a container that is
        // "running" is not yet a CA that is listening
        (1 to 40).exists { _ =>
          Thread.sleep(300)
          shell(Vector("docker", "cp", s"$name:/test/certs/pebble.minica.pem", ca.toString))._1 == 0 &&
            Files.isRegularFile(ca) && Files.size(ca) > 0 && answering()
        }

    private def answering(): Boolean =
      try
        val s = new java.net.Socket("127.0.0.1", apiPort)
        s.close()
        shell(Vector("docker", "logs", name))._2.contains("ACME directory available")
      catch case _: Exception => false

    def directory: String = s"https://localhost:$apiPort/dir"

    def stop(): Unit =
      shell(Vector("docker", "rm", "-f", name)): Unit

class TestAcmePebble extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private def rmrf(p: Path): Unit =
    Files.walk(p).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(q => Files.deleteIfExists(q): Unit)

  /** an Http that trusts ONE certificate authority -- Pebble's, for
   * this run. Test scope on purpose: production code keeps its own
   * transports, and this proves `Acme` works over any `Http`. */
  private def trusting(ca: Path): Http =
    val cf = java.security.cert.CertificateFactory.getInstance("X.509")
    val in = Files.newInputStream(ca)
    val certs = try cf.generateCertificates(in) finally in.close()
    val ks = java.security.KeyStore.getInstance(java.security.KeyStore.getDefaultType)
    ks.load(null, null)
    val it = certs.iterator
    var i = 0
    while it.hasNext do
      ks.setCertificateEntry(s"ca$i", it.next)
      i += 1
    val tmf = javax.net.ssl.TrustManagerFactory.getInstance(javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks)
    val ctx = javax.net.ssl.SSLContext.getInstance("TLS")
    ctx.init(null, tmf.getTrustManagers, null)
    val client = java.net.http.HttpClient.newBuilder().sslContext(ctx).build()
    new Http:
      def send(r: Request): Response ! Async = okay.async {
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(r.url))
        r.headers.foreach((k, v) => b.header(k, v): Unit)
        val body = r.method match
          case Method.Head => b.method("HEAD", java.net.http.HttpRequest.BodyPublishers.noBody())
          case Method.Get => b.GET()
          case _ => b.method(r.method.name, java.net.http.HttpRequest.BodyPublishers.ofByteArray(r.body.bytes))
        val res = client.send(body.build(), java.net.http.HttpResponse.BodyHandlers.ofByteArray())
        val headers = res.headers.map.entrySet.toArray.toVector.flatMap { e =>
          val entry = e.asInstanceOf[java.util.Map.Entry[String, java.util.List[String]]]
          entry.getValue.toArray.toVector.map(v => (entry.getKey, v.asInstanceOf[String]))
        }
        Response(res.statusCode, headers, Http.one(res.body))
      }

  test("Pebble issues our certificate: a real ACME server accepts our JWS, our nonces and our CSR") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-")
    val pebble = Pebble.Instance(dir)
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      val out = Resource.run[Either[String, Acme.Outcome], Pure](
        // the challenge server on the port Pebble's config validates
        Jetty.serve(pebble.challengePort)(challenges.routes)().map { _ =>
          Acme.ensure(Acme.Config(
            email = "ops@example.com",
            domains = Vector(Pebble.domain),
            accountKey = dir.resolve("account.pem"),
            certFile = dir.resolve("cert.pem"),
            keyFile = dir.resolve("key.pem"),
            directory = pebble.directory,
            timeout = java.time.Duration.ofSeconds(60)),
            trusting(pebble.ca), challenges)
        }).runWith

      out match
        case Left(msg) => fail(s"pebble refused: $msg")
        case Right(Acme.Outcome.Current(_)) => fail("nothing was on disk, so it cannot have been current")
        case Right(Acme.Outcome.Issued(domains, notAfter)) =>
          assertEquals(domains, Vector(Pebble.domain))
          assert(notAfter.isAfter(java.time.Instant.now()), notAfter.toString)

      // the certificate is REAL: a chain, for the name we asked for,
      // signed by an issuer that is not us
      val pem = Files.readString(dir.resolve("cert.pem"))
      assert(pem.startsWith("-----BEGIN CERTIFICATE-----"), pem.take(60))
      val cf = java.security.cert.CertificateFactory.getInstance("X.509")
      val in = Files.newInputStream(dir.resolve("cert.pem"))
      val chain = try cf.generateCertificates(in).toArray.toVector finally in.close()
      assert(chain.length >= 2, s"a leaf without its chain: ${chain.length}")
      val leaf = chain.head.asInstanceOf[java.security.cert.X509Certificate]
      assert(leaf.getIssuerX500Principal.getName.contains("Pebble"), leaf.getIssuerX500Principal.getName)
      val names = Option(leaf.getSubjectAlternativeNames).map(_.toArray.toVector.map(_.toString)).getOrElse(Vector.empty)
      assert(names.exists(_.contains(Pebble.domain)), names.toString)
      // and the same identity serves TLS: the point of the exercise
      assertEquals(challenges.size, 0, "the token was not cleaned up")
    finally
      pebble.stop()
      rmrf(dir)
  }

  test("revocation: Pebble takes the certificate back, and refuses a second one by name") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-revoke-")
    val pebble = Pebble.Instance(dir)
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      val cfg = Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        pebble.directory, timeout = java.time.Duration.ofSeconds(60))
      val http = trusting(pebble.ca)
      val issued = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(pebble.challengePort)(challenges.routes)().map(_ => Acme.ensure(cfg, http, challenges))).runWith
      assert(issued.exists(_.isInstanceOf[Acme.Outcome.Issued]), issued.toString)

      // the leaf alone is what a CA revokes, not the bundle on disk
      assert(Acme.leafDer(cfg.certFile).exists(_.nonEmpty))

      val first = Acme.revoke(cfg, http, Acme.Reason.KeyCompromise)
      assert(first.isRight, first.toString)

      // twice is not an error we invent: the CA's own sentence comes back
      val second = Acme.revoke(cfg, http, Acme.Reason.KeyCompromise)
      assert(second.isLeft, "a second revoke was accepted")
      assert(second.left.exists(m => m.toLowerCase.contains("already")), second.toString)
    finally
      pebble.stop()
      rmrf(dir)
  }

  test("external account binding: a CA that requires one refuses without it and issues with it") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-eab-")
    val pebble = Pebble.Instance(dir, eabRequired = true)
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      def cfg(eab: Option[(String, String)]) = Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        pebble.directory, timeout = java.time.Duration.ofSeconds(60), eab = eab)
      val http = trusting(pebble.ca)

      val without = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(pebble.challengePort)(challenges.routes)().map(_ =>
          Acme.ensure(cfg(None), http, challenges))).runWith
      assert(without.isLeft, "an account was opened without the binding the CA requires")
      assert(without.left.exists(m => m.toLowerCase.contains("external") || m.toLowerCase.contains("binding")),
        without.toString)
      assert(!Files.exists(dir.resolve("cert.pem")))

      val with_ = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(pebble.challengePort)(challenges.routes)().map(_ =>
          Acme.ensure(cfg(Some((Pebble.eabKid, Pebble.eabKey))), http, challenges))).runWith
      assert(with_.exists(_.isInstanceOf[Acme.Outcome.Issued]), with_.toString)
      assert(Files.readString(dir.resolve("cert.pem")).startsWith("-----BEGIN CERTIFICATE-----"))
    finally
      pebble.stop()
      rmrf(dir)
  }

  test("ARI: the CA publishes a window, we build the certID from the certificate itself, and a closed window changes nothing") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-ari-")
    val pebble = Pebble.Instance(dir)
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      // a LONG countdown: on our own rule this certificate is nowhere
      // near due, so anything that renews it came from the CA
      val cfg = Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        pebble.directory, renewBefore = java.time.Duration.ofSeconds(1),
        timeout = java.time.Duration.ofSeconds(60))
      val http = trusting(pebble.ca)
      val issued = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(pebble.challengePort)(challenges.routes)().map(_ => Acme.ensure(cfg, http, challenges))).runWith
      assert(issued.exists(_.isInstanceOf[Acme.Outcome.Issued]), issued.toString)

      // the id is built from the certificate's own AKI and serial
      val id = Acme.certId(cfg.certFile)
      assert(id.isRight, id.toString)
      assert(id.exists(_.contains(".")), id.toString)

      Acme.renewalWindow(cfg, http) match
        case None => fail("Pebble publishes renewalInfo, so a window was expected")
        case Some((start, end)) =>
          assert(start.isBefore(end), s"$start .. $end")
          // Pebble suggests a window inside the certificate's life
          val notAfter = Acme.notAfterOf(cfg.certFile).get
          assert(end.isBefore(notAfter.plusSeconds(1)), s"$end vs $notAfter")

      // a certificate that is NOT due by our countdown and whose
      // window has not opened stays put
      val far = cfg.copy(renewBefore = java.time.Duration.ofSeconds(1))
      val again = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(pebble.challengePort)(challenges.routes)().map(_ => Acme.ensure(far, http, challenges))).runWith
      assert(again.exists(_.isInstanceOf[Acme.Outcome.Current]), s"renewed with neither rule due: $again")
    finally
      pebble.stop()
      rmrf(dir)
  }

  /** the test's "DNS provider": challtestsrv's own HTTP API, which is
   * what the CA will then resolve against */
  private def challTestSrvDns(managePort: Int): Acme.Dns = new Acme.Dns:
    private def post(path: String, body: String): Either[String, Unit] =
      try
        val c = java.net.URI.create(s"http://127.0.0.1:$managePort$path").toURL.openConnection()
          .asInstanceOf[java.net.HttpURLConnection]
        c.setRequestMethod("POST")
        c.setDoOutput(true)
        c.getOutputStream.write(body.getBytes("UTF-8"))
        val code = c.getResponseCode
        c.disconnect()
        Either.cond(code == 200, (), s"challtestsrv answered $code")
      catch case e: Exception => Left(s"challtestsrv: ${e.getMessage}")

    def putTxt(name: String, value: String): Either[String, Unit] =
      post("/set-txt", s"""{"host":"$name.","value":"$value"}""")

    def removeTxt(name: String): Unit = post("/clear-txt", s"""{"host":"$name."}"""): Unit

    // the record is in memory next door; there is nothing to propagate
    override def propagation: java.time.Duration = java.time.Duration.ZERO

  test("dns-01, and a WILDCARD: the proof goes into DNS and the CA resolves it") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-dns-")
    val dns = Pebble.ChallTestSrv(Pebble.freePort(), Pebble.freePort())
    assume(dns.start(), "challtestsrv did not start")
    val pebble = Pebble.Instance(dir, dnsServer = Some(dns.dnsPort))
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      // a wildcard: http-01 cannot prove one at all, which is the
      // whole reason dns-01 exists
      val cfg = Acme.Config("ops@example.com", Vector("*.okay.example"),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        pebble.directory, timeout = java.time.Duration.ofSeconds(60))
      val http = trusting(pebble.ca)

      // without a Dns the run refuses BEFORE placing an order, and says why
      val refused = Acme.ensure(cfg, http, challenges)
      assert(refused.left.exists(_.contains("wildcard")), refused.toString)

      val issued = Acme.ensure(cfg, http, challenges, Some(challTestSrvDns(dns.managePort)))
      assert(issued.exists(_.isInstanceOf[Acme.Outcome.Issued]), issued.toString)

      val leaf = Acme.leafCert(cfg.certFile).fold(m => fail(m), identity)
      val names = Option(leaf.getSubjectAlternativeNames).map(_.toArray.toVector.map(_.toString)).getOrElse(Vector.empty)
      assert(names.exists(_.contains("*.okay.example")), names.toString)
      assert(leaf.getIssuerX500Principal.getName.contains("Pebble"), leaf.getIssuerX500Principal.getName)
      // the http-01 store was never touched: this was proven in DNS
      assertEquals(challenges.size, 0)
    finally
      pebble.stop()
      dns.stop()
      rmrf(dir)
  }

  test("a name Pebble cannot reach is refused with the CA's own words, not a timeout") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-bad-")
    val pebble = Pebble.Instance(dir)
    assume(pebble.start(), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      // no challenge server at all: the validation must fail, and the
      // failure must arrive as a sentence
      val out = Acme.ensure(Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        pebble.directory,
        timeout = java.time.Duration.ofSeconds(30)), trusting(pebble.ca), challenges)
      assert(out.isLeft, out.toString)
      val msg = out.left.getOrElse("")
      assert(msg.contains("refused") || msg.contains("invalid") || msg.contains("did not settle"), msg)
      assert(!Files.exists(dir.resolve("cert.pem")), "a certificate appeared for a name that never validated")
    finally
      pebble.stop()
      rmrf(dir)
  }
