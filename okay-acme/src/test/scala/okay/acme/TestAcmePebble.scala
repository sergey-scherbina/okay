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
  val name = "okay-acme-pebble-test"
  val domain = "host.docker.internal"
  /** the port Pebble's own config validates HTTP-01 against */
  val challengePort = 5002
  val apiPort = 14000

  private def sh(cmd: String*): (Int, String) =
    val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes(), "UTF-8")
    (p.waitFor(), out)

  def available: Boolean = sh("docker", "info")._1 == 0

  def start(caOut: Path): Boolean =
    sh("docker", "rm", "-f", name): Unit
    val (code, out) = sh("docker", "run", "-d", "--rm", "--name", name,
      "-p", s"$apiPort:$apiPort", "-p", "15000:15000",
      "-e", "PEBBLE_VA_NOSLEEP=1",
      "--add-host=host.docker.internal:host-gateway", image)
    if code != 0 then
      println(s"pebble did not start: $out")
      false
    else
      // its API is HTTPS under a CA it generates per run: pull that out,
      // or nothing can talk to it
      val ready = (1 to 30).exists { _ =>
        Thread.sleep(500)
        sh("docker", "cp", s"$name:/test/certs/pebble.minica.pem", caOut.toString)._1 == 0 &&
          Files.isRegularFile(caOut) && Files.size(caOut) > 0
      }
      ready

  def stop(): Unit = sh("docker", "rm", "-f", name): Unit

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
    val ca = dir.resolve("pebble-ca.pem")
    assume(Pebble.start(ca), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      val out = Resource.run[Either[String, Acme.Outcome], Pure](
        // the challenge server on the port Pebble's config validates
        Jetty.serve(Pebble.challengePort)(challenges.routes)().map { _ =>
          Acme.ensure(Acme.Config(
            email = "ops@example.com",
            domains = Vector(Pebble.domain),
            accountKey = dir.resolve("account.pem"),
            certFile = dir.resolve("cert.pem"),
            keyFile = dir.resolve("key.pem"),
            directory = s"https://localhost:${Pebble.apiPort}/dir",
            timeout = java.time.Duration.ofSeconds(60)),
            trusting(ca), challenges)
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
      Pebble.stop()
      rmrf(dir)
  }

  test("revocation: Pebble takes the certificate back, and refuses a second one by name") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-revoke-")
    val ca = dir.resolve("pebble-ca.pem")
    assume(Pebble.start(ca), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      val cfg = Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        s"https://localhost:${Pebble.apiPort}/dir", timeout = java.time.Duration.ofSeconds(60))
      val http = trusting(ca)
      val issued = Resource.run[Either[String, Acme.Outcome], Pure](
        Jetty.serve(Pebble.challengePort)(challenges.routes)().map(_ => Acme.ensure(cfg, http, challenges))).runWith
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
      Pebble.stop()
      rmrf(dir)
  }

  test("a name Pebble cannot reach is refused with the CA's own words, not a timeout") {
    assume(Pebble.available, "docker is not available")
    val dir = Files.createTempDirectory("okay-acme-pebble-bad-")
    val ca = dir.resolve("pebble-ca.pem")
    assume(Pebble.start(ca), "pebble did not start")
    val challenges = Acme.Challenges.Memory()
    try
      // no challenge server at all: the validation must fail, and the
      // failure must arrive as a sentence
      val out = Acme.ensure(Acme.Config("ops@example.com", Vector(Pebble.domain),
        dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
        s"https://localhost:${Pebble.apiPort}/dir",
        timeout = java.time.Duration.ofSeconds(30)), trusting(ca), challenges)
      assert(out.isLeft, out.toString)
      val msg = out.left.getOrElse("")
      assert(msg.contains("refused") || msg.contains("invalid") || msg.contains("did not settle"), msg)
      assert(!Files.exists(dir.resolve("cert.pem")), "a certificate appeared for a name that never validated")
    finally
      Pebble.stop()
      rmrf(dir)
  }
