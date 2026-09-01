package okay.security

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Body, Http, Method, Request, Response, Server, Transports}

/**
 * The two halves that touch a wire, against real okay-http servers on
 * real ports: a protected route challenging and admitting, and the
 * OAuth2 code+PKCE flow against a stub authorization server — which
 * is a TEST; this module does not issue codes to strangers.
 */
class TestFlows extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  val now = 1_700_000_000L
  val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
  def token(scopes: Set[String]): String =
    Jwt.sign(Claims(subject = Some("u1"), audience = Vector("api"),
      expires = Some(now + 600), scopes = scopes), Jwt.Key.Hmac(secret))

  val http = Transports.http()

  def served[A](route: PartialFunction[Request, Response ! Async])(body: Int => A): A =
    Resource.run[A, Pure](Server.serve(0)(route).map(s => body(Server.port(s)))).runWith

  def get(port: Int, path: String, headers: (String, String)*): Response =
    Async.run[Response, Pure](http.send(
      Request.get(s"http://127.0.0.1:$port$path", headers))).runWith

  test("a protected route: 401 with a challenge, 401, 403, and through") {
    val route = Secure.bearer(
      verify = t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now),
      policy = Policy.scoped("read")) { principal =>
      { case r if r.url.startsWith("/hello") =>
          pure(Response(200, Nil, Http.one(s"hello ${principal.name}".getBytes("UTF-8")))) }
    }

    served(route) { port =>
      val none = get(port, "/hello")
      assertEquals(none.status, 401)
      assert(none.header("www-authenticate").exists(_.contains("Bearer")), none.headers.toString)

      assertEquals(get(port, "/hello", ("authorization", "Bearer garbage")).status, 401)
      assertEquals(get(port, "/hello",
        ("authorization", s"Bearer ${token(Set("write"))}")).status, 403)

      val ok = get(port, "/hello", ("authorization", s"Bearer ${token(Set("read"))}"))
      assertEquals(ok.status, 200)
      assertEquals(Async.run[String, Pure](Http.text(ok)).runWith, "hello u1")
    }
  }

  test("OAuth2 code+PKCE against a stub AS: challenge out, verifier back, tokens; errors are Lefts") {
    // the stub AS: /authorize is not exercised (no browser here); the
    // token endpoint checks the S256 relation and the grant
    var seenChallenge = ""
    val as: PartialFunction[Request, Response ! Async] = {
      case r if r.url.startsWith("/token") && r.method == Method.Post =>
        val form = OAuth2.form(r.body match
          case Body.Bytes(b) => String(b.toArray, "UTF-8")
          case Body.Text(t) => t
          case _ => "")
        def json(s: String): Response ! Async = pure(Response(200,
          Seq(("content-type", "application/json")), Http.one(s.getBytes("UTF-8"))))
        form.get("grant_type") match
          case Some("authorization_code") =>
            val verifier = form.getOrElse("code_verifier", "")
            val challenge = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
              summon[Crypto].sha256(verifier.getBytes("US-ASCII")))
            if form.get("code") != Some("c0de") then
              pure(Response(400, Nil, Http.one("""{"error":"invalid_grant"}""".getBytes)))
            else if challenge != seenChallenge then
              pure(Response(400, Nil, Http.one("""{"error":"invalid_request"}""".getBytes)))
            else json("""{"access_token":"at1","refresh_token":"rt1","expires_in":3600}""")
          case Some("refresh_token") if form.get("refresh_token") == Some("rt1") =>
            json("""{"access_token":"at2","expires_in":3600}""")
          case Some("client_credentials") =>
            json("""{"access_token":"cc1"}""")
          case _ =>
            pure(Response(400, Nil, Http.one("""{"error":"unsupported_grant_type"}""".getBytes)))
    }

    served(as) { port =>
      val client = OAuth2.Client("app", Some("shhh"),
        s"http://127.0.0.1:$port/authorize", s"http://127.0.0.1:$port/token",
        "http://localhost/cb", Seq("read"))

      val (verifier, challenge) = OAuth2.pkce()
      seenChallenge = challenge
      val url = OAuth2.authorizationUrl(client, "st4te", challenge)
      assert(url.contains("code_challenge=" + challenge), url)
      assert(url.contains("code_challenge_method=S256"))

      def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

      val tokens = run(OAuth2.exchange(http, client, "c0de", verifier))
      assertEquals(tokens.map(_.access), Right("at1"))
      assertEquals(run(OAuth2.refresh(http, client, "rt1")).map(_.access), Right("at2"))
      assertEquals(run(OAuth2.clientCredentials(http, client)).map(_.access), Right("cc1"))

      // a wrong code is the SERVER'S error, surfaced as a Left
      assertEquals(run(OAuth2.exchange(http, client, "wrong", verifier)),
        Left("invalid_grant"))
      // a wrong verifier fails the S256 relation at the AS
      assert(run(OAuth2.exchange(http, client, "c0de", "not-the-verifier")).isLeft)
    }
  }
}
