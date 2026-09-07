package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}
import okay.jetty.Jetty

import java.nio.file.{Files, Path}

/** script-https-default: https with nothing to obtain first, and the
 * three headers-and-redirects a deployment needs either side of a
 * proxy. See specs/okay-script.md "HTTPS out of the box".
 */
class TestHttpsDefault extends munit.FunSuite:

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def header(r: HttpResponse, n: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }

  private def withRoot[A](build: Path => Site)(body: Site => A): A =
    val root = Files.createTempDirectory("okay-script-https-")
    Files.writeString(root.resolve("index.md"), "home\n"): Unit
    Files.writeString(root.resolve("style.css"), "body{}\n"): Unit
    val site = build(root)
    try body(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("httpsOnly: an insecure request is 301'd to the same URL on https; a secure one is served") {
    withRoot(Site(_, httpsOnly = true, trustForwarded = true)) { site =>
      val r = site.handle(Request.get("/shop?a=1", Seq("Host" -> "shop.example.com")))
      assertEquals(r.status, 301)
      assertEquals(header(r, "location"), Some("https://shop.example.com/shop?a=1"))
      // the redirect happens before routing: an unknown path is still
      // sent to https rather than told it does not exist over http
      assertEquals(site.handle(Request.get("/nope", Seq("Host" -> "x"))).status, 301)
      // a request with no Host cannot be redirected anywhere honest
      assertEquals(site.handle(Request.get("/")).status, 400)
      // the forwarded claim makes it secure, so the page is served
      val ok = site.handle(Request.get("/", Seq("Host" -> "x", "X-Forwarded-Proto" -> "https")))
      assertEquals(ok.status, 200)
      assert(text(ok).contains("home"))
    }
  }

  test("HSTS rides only on a secure response, and only when asked for") {
    withRoot(Site(_, hsts = Some(31536000), trustForwarded = true)) { site =>
      val secure = Seq("X-Forwarded-Proto" -> "https")
      assertEquals(header(site.handle(Request.get("/", secure)), "strict-transport-security"), Some("max-age=31536000"))
      assertEquals(header(site.handle(Request.get("/style.css", secure)), "strict-transport-security"), Some("max-age=31536000"))
      // over plaintext it must NOT be announced -- that is how a site
      // locks itself out of a browser it cannot yet serve
      assertEquals(header(site.handle(Request.get("/")), "strict-transport-security"), None)
    }
    withRoot(Site(_, trustForwarded = true)) { site =>
      assertEquals(header(site.handle(Request.get("/", Seq("X-Forwarded-Proto" -> "https"))), "strict-transport-security"), None)
    }
  }

  test("Serve.parse reads the four switches") {
    val root = Files.createTempDirectory("okay-script-https-args-")
    try
      def parse(env: Map[String, String]) = Serve.parse(Array(root.toString), env.get)
      assertEquals(parse(Map.empty).map(a => (a.selfSigned, a.hsts, a.httpsOnly, a.httpPort)), Right((false, None, false, None)))
      assertEquals(parse(Map("OKAY_TLS" -> "self")).map(_.selfSigned), Right(true))
      assertEquals(parse(Map("OKAY_TLS" -> "SELF")).map(_.selfSigned), Right(true))
      assertEquals(parse(Map("OKAY_HSTS" -> "600")).map(_.hsts), Right(Some(600)))
      // was silently None before script-config; a value that is not a
      // number is now a REFUSAL naming the variable, because "the HSTS
      // I set is not in effect" is not a thing to discover in production
      assert(parse(Map("OKAY_HSTS" -> "soon")).left.exists(_.contains("OKAY_HSTS is not a whole number: 'soon'")),
        parse(Map("OKAY_HSTS" -> "soon")).toString)
      // zero and below still mean "off", which is a VALUE, not a typo
      assertEquals(parse(Map("OKAY_HSTS" -> "0")).map(_.hsts), Right(None))
      assertEquals(parse(Map("OKAY_HTTPS_ONLY" -> "1")).map(_.httpsOnly), Right(true))
      assertEquals(parse(Map("OKAY_HTTP_PORT" -> "8080")).map(_.httpPort), Right(Some(8080)))
      // OKAY_TLS=self makes the scheme https without a certificate file
      assertEquals(parse(Map("OKAY_TLS" -> "self")).map(_.scheme), Right("http"))
    finally Files.deleteIfExists(root): Unit
  }

  test("the redirect port answers every path with a 301 to the https one") {
    val to = Serve.redirectTo(8443)
    val r = Async.run[HttpResponse, Pure](to(Request.get("/a/b?c=d", Seq("Host" -> "shop.example.com:80")))).runWith
    assertEquals(r.status, 301)
    assertEquals(header(r, "location"), Some("https://shop.example.com:8443/a/b?c=d"))
    // the standard port is not spelled out
    val std = Async.run[HttpResponse, Pure](Serve.redirectTo(443)(Request.get("/", Seq("Host" -> "shop.example.com")))).runWith
    assertEquals(header(std, "location"), Some("https://shop.example.com/"))
  }

  test("a self-signed certificate: generated once, reused, and it actually serves https".tag(new munit.Tag("Live"))) {
    val dir = Files.createTempDirectory("okay-script-selfsigned-")
    val root = Files.createTempDirectory("okay-script-selfsigned-pages-")
    Files.writeString(root.resolve("index.md"), "hello over tls\n"): Unit
    try
      val a = Serve.parse(Array(root.toString, "0"), Map("OKAY_TLS" -> "self", "OKAY_DATA" -> dir.toString).get)
        .fold(m => fail(m), identity)
      val ssl = Serve.sslOf(a).fold(m => fail(m), identity).getOrElse(fail("no context"))
      val keystore = dir.resolve("okay-script-tls.p12")
      assert(Files.isRegularFile(keystore), s"no keystore at $keystore")
      val stamp = Files.getLastModifiedTime(keystore)

      val site = Site(root)
      try
        val body = Resource.run[String, Pure](site.serve(0, Some(ssl)).map { server =>
          val sock = javax.net.ssl.SSLContext.getInstance("TLS") match
            case c =>
              // a client that trusts THIS certificate and nothing else:
              // the point under test is that the server presents one
              val ks = java.security.KeyStore.getInstance("PKCS12")
              val in = Files.newInputStream(keystore)
              try ks.load(in, "okay-script".toCharArray) finally in.close()
              val tmf = javax.net.ssl.TrustManagerFactory.getInstance(javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm)
              tmf.init(ks)
              c.init(null, tmf.getTrustManagers, null)
              c.getSocketFactory.createSocket("localhost", Jetty.port(server))
          try
            val out = sock.getOutputStream
            out.write("GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".getBytes("UTF-8"))
            out.flush()
            new String(sock.getInputStream.readAllBytes(), "UTF-8")
          finally sock.close()
        }).runWith
        assert(body.startsWith("HTTP/1.1 200"), body.take(120))
        assert(body.contains("hello over tls"), body)
      finally site.close()

      // a second start reuses the identity rather than minting a new one
      Serve.sslOf(a).fold(m => fail(m), identity): Unit
      assertEquals(Files.getLastModifiedTime(keystore), stamp)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }

  test("Serve.parse reads the ACME switches, and the pair is only a pair with domains") {
    val root = Files.createTempDirectory("okay-script-acme-args-")
    try
      def parse(env: Map[String, String]) = Serve.parse(Array(root.toString), env.get)
      assertEquals(parse(Map.empty).map(_.acme), Right(None))
      // an email without domains asks for nothing: there is no name to
      // prove control of, so this is a misconfiguration, not a request
      assertEquals(parse(Map("OKAY_ACME" -> "ops@example.com")).map(_.acme), Right(None))
      assertEquals(
        parse(Map("OKAY_ACME" -> "ops@example.com", "OKAY_ACME_DOMAINS" -> "a.example.com, b.example.com")).map(_.acme),
        Right(Some(("ops@example.com", Vector("a.example.com", "b.example.com"), false))))
      assertEquals(
        parse(Map("OKAY_ACME" -> "o@e.com", "OKAY_ACME_DOMAINS" -> "a", "OKAY_ACME_PROD" -> "1")).map(_.acme.map(_._3)),
        Right(Some(true)))
      // staging is the default: a first run cannot burn a production limit
      assertEquals(parse(Map("OKAY_ACME" -> "o@e.com", "OKAY_ACME_DOMAINS" -> "a")).map(_.acme.map(_._3)), Right(Some(false)))
      // and the files live beside the data, so an account survives a restart
      val withData = parse(Map("OKAY_ACME" -> "o@e.com", "OKAY_ACME_DOMAINS" -> "a", "OKAY_DATA" -> "/tmp/d")).toOption.get
      val ((cert, key), account) = Serve.acmeFiles(withData)
      assertEquals(cert.toString, "/tmp/d/acme/cert.pem")
      assertEquals(key.toString, "/tmp/d/acme/key.pem")
      assertEquals(account.toString, "/tmp/d/acme/account.pem")
    finally Files.deleteIfExists(root): Unit
  }

  test("Serve.parse reads OKAY_ACME_EAB as a kid:key pair, and ignores half of one") {
    val root = Files.createTempDirectory("okay-script-eab-args-")
    try
      def parse(env: Map[String, String]) = Serve.parse(Array(root.toString), env.get)
      assertEquals(parse(Map.empty).map(_.acmeEab), Right(None))
      assertEquals(parse(Map("OKAY_ACME_EAB" -> "kid-1:zWNDZM6e")).map(_.acmeEab), Right(Some(("kid-1", "zWNDZM6e"))))
      // the key is base64url and carries no colon, so the FIRST one splits
      assertEquals(parse(Map("OKAY_ACME_EAB" -> "a:b:c")).map(_.acmeEab), Right(Some(("a", "b:c"))))
      assertEquals(parse(Map("OKAY_ACME_EAB" -> "kid-only")).map(_.acmeEab), Right(None))
      assertEquals(parse(Map("OKAY_ACME_EAB" -> "kid:")).map(_.acmeEab), Right(None))
    finally Files.deleteIfExists(root): Unit
  }
