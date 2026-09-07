package okay.acme

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.security.given

import java.nio.file.{Files, Path}

/**
 * The client against a FAKE certificate authority run in this
 * process: a real HTTP server speaking RFC 8555's shapes, which
 * fetches the challenge back over a real socket and signs a
 * certificate with its own CA key. It proves the protocol machine —
 * the nonces, the JWS with jwk then kid, the authorization poll, the
 * finalize, the download — and the HTTP-01 round trip.
 *
 * What it cannot prove is INTEROP with a real CA; that is Pebble's
 * job, filed as its own task (acme-pebble). Live-tagged because it
 * binds ports and shells out to openssl for the CSR.
 */
class TestAcme extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private def rmrf(p: Path): Unit =
    Files.walk(p).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(q => Files.deleteIfExists(q): Unit)

  test("the whole flow against a fake CA: account, order, http-01, finalize, a certificate on disk") {
    val dir = Files.createTempDirectory("okay-acme-")
    val ca = FakeCa(dir.resolve("ca"))
    assume(ca.ready, "openssl is needed for the fake CA and the CSR")
    val challenges = Acme.Challenges.Memory()
    try
      val result = Resource.run[Either[String, Acme.Outcome], Pure](
        for
          challengeServer <- Jetty.serve(0)(challenges.routes)()
          caServer <- Jetty.serve(0)(ca.routes(Jetty.port(challengeServer)))()
          http <- Jetty.http()
        yield
          val cfg = Acme.Config(
            email = "ops@example.com",
            domains = Vector("localhost"),
            accountKey = dir.resolve("account.pem"),
            certFile = dir.resolve("cert.pem"),
            keyFile = dir.resolve("key.pem"),
            directory = s"http://127.0.0.1:${Jetty.port(caServer)}/directory",
            timeout = java.time.Duration.ofSeconds(20))
          Acme.ensure(cfg, http, challenges)).runWith

      result match
        case Left(msg) => fail(msg)
        case Right(Acme.Outcome.Current(_)) => fail("nothing was on disk, so it cannot have been current")
        case Right(Acme.Outcome.Issued(domains, notAfter)) =>
          assertEquals(domains, Vector("localhost"))
          assert(notAfter.isAfter(java.time.Instant.now()), notAfter.toString)

      // what the CA saw, and what is on disk
      assertEquals(ca.accounts, 1)
      assert(ca.fetchedTokens.nonEmpty, "the CA never fetched the challenge")
      val pem = Files.readString(dir.resolve("cert.pem"))
      assert(pem.startsWith("-----BEGIN CERTIFICATE-----"), pem.take(60))
      assert(Files.isRegularFile(dir.resolve("key.pem")))
      assert(Files.isRegularFile(dir.resolve("account.pem")))
      assertEquals(challenges.size, 0, "the token was not cleaned up")
      // the certificate is the one the CA signed, for the name asked
      val x = Acme.notAfterOf(dir.resolve("cert.pem"))
      assert(x.isDefined)
    finally rmrf(dir)
  }

  test("a second run with a valid certificate on disk asks the CA for nothing") {
    val dir = Files.createTempDirectory("okay-acme-current-")
    val ca = FakeCa(dir.resolve("ca"))
    assume(ca.ready, "openssl is needed for the fake CA")
    val challenges = Acme.Challenges.Memory()
    try
      // the fake CA signs for ONE DAY, so the renewal window has to be
      // shorter than that for "still current" to be the right answer --
      // with the 30-day default this test would be asserting that a
      // certificate expiring tomorrow needs no renewal, which is false
      def run(renewBefore: java.time.Duration): Either[String, Acme.Outcome] =
        Resource.run[Either[String, Acme.Outcome], Pure](
          for
            challengeServer <- Jetty.serve(0)(challenges.routes)()
            caServer <- Jetty.serve(0)(ca.routes(Jetty.port(challengeServer)))()
            http <- Jetty.http()
          yield Acme.ensure(Acme.Config("ops@example.com", Vector("localhost"),
            dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
            s"http://127.0.0.1:${Jetty.port(caServer)}/directory",
            renewBefore = renewBefore,
            timeout = java.time.Duration.ofSeconds(20)), http, challenges)).runWith

      val first = run(java.time.Duration.ofHours(1))
      assert(first.exists(_.isInstanceOf[Acme.Outcome.Issued]), first.toString)
      val orders = ca.orders
      val second = run(java.time.Duration.ofHours(1))
      assert(second.exists(_.isInstanceOf[Acme.Outcome.Current]), s"the second run re-issued: $second")
      assertEquals(ca.orders, orders, "the second run asked the CA for a new order")

      // and the other side of the same rule: a window WIDER than the
      // certificate's life means renew now
      val third = run(java.time.Duration.ofDays(30))
      assert(third.exists(_.isInstanceOf[Acme.Outcome.Issued]), s"a certificate inside the window was not renewed: $third")
    finally rmrf(dir)
  }

  test("the CA's refusal is the sentence it sent, not a status code") {
    val dir = Files.createTempDirectory("okay-acme-refused-")
    val ca = FakeCa(dir.resolve("ca"), refuse = Some("the account is not authorized for this name"))
    assume(ca.ready, "openssl is needed for the fake CA")
    val challenges = Acme.Challenges.Memory()
    try
      val out = Resource.run[Either[String, Acme.Outcome], Pure](
        for
          caServer <- Jetty.serve(0)(ca.routes(0))()
          http <- Jetty.http()
        yield Acme.ensure(Acme.Config("ops@example.com", Vector("localhost"),
          dir.resolve("account.pem"), dir.resolve("cert.pem"), dir.resolve("key.pem"),
          s"http://127.0.0.1:${Jetty.port(caServer)}/directory",
          timeout = java.time.Duration.ofSeconds(5)), http, challenges)).runWith
      assert(out.left.exists(_.contains("not authorized for this name")), out.toString)
    finally rmrf(dir)
  }

  test("the account key is generated once and reused; the thumbprint is stable") {
    val dir = Files.createTempDirectory("okay-acme-keys-")
    try
      val a = Acme.accountKeyOf(dir.resolve("acct.pem")).fold(m => fail(m), identity)
      val b = Acme.accountKeyOf(dir.resolve("acct.pem")).fold(m => fail(m), identity)
      assertEquals(a.getPublic, b.getPublic)
      assertEquals(Acme.canonicalJwk(a.getPublic), Acme.canonicalJwk(b.getPublic))
      // RFC 7638's canonical form: exactly these members, in this order
      val jwk = Acme.canonicalJwk(a.getPublic)
      assert(jwk.startsWith("""{"crv":"P-256","kty":"EC","x":"""), jwk)
      assert(jwk.endsWith("\"}"), jwk)
    finally rmrf(dir)
  }
