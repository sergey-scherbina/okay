package okay.security

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Body, Http, Method, Request, Response, Server, Transports}

/**
 * OIDC against a stub IdP on a real port: discovery, the login url,
 * the callback that answers a Principal — and every refusal by name,
 * because an id_token is exactly the input an attacker crafts.
 */
class TestOidc extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  val now = 1_700_000_000L
  val http = Transports.http()

  lazy val rsa =
    val g = java.security.KeyPairGenerator.getInstance("RSA")
    g.initialize(2048)
    g.generateKeyPair()

  def idToken(issuer: String, aud: String, nonce: String, access: String,
              tweak: Claims => Claims = identity): String =
    Jwt.sign(tweak(Claims(issuer = Some(issuer), subject = Some("u1"),
      audience = Vector(aud), expires = Some(now + 600),
      json = Json.JObj(Vector(
        "name" -> Json.JStr("Ada"),
        "nonce" -> Json.JStr(nonce),
        "at_hash" -> Json.JStr(Oidc.atHash(access)))))),
      Keys.rsaPair(rsa.getPublic, rsa.getPrivate), kid = Some("k1"))

  /** the stub IdP: discovery, jwks, and a token endpoint that mints a
   * proper id_token echoing the nonce the auth url carried */
  def idp[A](body: (String, () => String) => A): A =
    var base = ""
    var lastNonce = ""
    val route: PartialFunction[Request, Response ! Async] = {
      case r if r.url.startsWith("/.well-known/openid-configuration") =>
        json(s"""{"issuer":"$base","authorization_endpoint":"$base/auth",
          "token_endpoint":"$base/token","jwks_uri":"$base/jwks"}""")
      case r if r.url.startsWith("/jwks") =>
        val enc = java.util.Base64.getUrlEncoder.withoutPadding
        val pub = rsa.getPublic.asInstanceOf[java.security.interfaces.RSAPublicKey]
        def uint(b: java.math.BigInteger) =
          val raw = b.toByteArray
          enc.encodeToString(if raw(0) == 0 then raw.drop(1) else raw)
        json(s"""{"keys":[{"kty":"RSA","kid":"k1",
          "n":"${uint(pub.getModulus)}","e":"${uint(pub.getPublicExponent)}"}]}""")
      case r if r.url.startsWith("/auth") =>
        // the browser step, abbreviated: remember the nonce
        lastNonce = r.url.split("nonce=").last.split('&').head
        pure(Response(302, Seq(("location", "cb?code=c0de")), Http.one(Array.emptyByteArray)))
      case r if r.url.startsWith("/token") && r.method == Method.Post =>
        val access = "acc3ss"
        json(s"""{"access_token":"$access","id_token":"${
          idToken(base, "app", lastNonce, access)}"}""")
    }
    okay.Resource.run[A, Pure](Server.serve(0)(route).map { s =>
      base = s"http://127.0.0.1:${Server.port(s)}"
      body(base, () => lastNonce)
    }).runWith

  def run[A](p: A ! Async): A = Async.run[A, Pure](p).runWith

  test("the whole login: discover, url with nonce, callback answers the Principal") {
    idp { (base, _) =>
      val p = run(Oidc.discover(http, base)).toOption.get
      assertEquals(p.jwksUri, s"$base/jwks")

      val attempt = Oidc.login(p, "app", "http://localhost/cb")
      assert(attempt.url.contains("scope=openid"), attempt.url)
      assert(attempt.url.contains("nonce=" + attempt.nonce))

      // "the browser": hit /auth so the IdP learns the nonce
      run(http.send(Request.get(attempt.url)))

      run(Oidc.callback(http, p, "app", "http://localhost/cb",
        attempt, "c0de", now)) match
        case Right((principal, tokens)) =>
          assertEquals(principal.id, "u1")
          assertEquals(principal.name, "Ada")
          assertEquals(tokens.access, "acc3ss")
        case Left(e) => fail(e)
    }
  }

  test("every forgery refuses by name") {
    val keys: String => Option[Jwt.Key] = _ => Some(Keys.rsaPublic(rsa.getPublic))
    val good = idToken("https://idp", "app", "n0nce", "acc3ss")
    assert(Oidc.validate(good, keys, "https://idp", "app", "n0nce", "acc3ss", now).isRight)

    def why(t: String, issuer: String = "https://idp", aud: String = "app",
            nonce: String = "n0nce", access: String = "acc3ss"): String =
      Oidc.validate(t, keys, issuer, aud, nonce, access, now).left.getOrElse("PASSED")

    assert(why(good, issuer = "https://evil").contains("issuer"))
    assert(why(good, nonce = "other").contains("nonce"))
    assert(why(good, access = "stolen").contains("at_hash"))
    assert(why(good, aud = "other-app").contains("audience"))
    val expired = idToken("https://idp", "app", "n0nce", "acc3ss",
      tweak = _.copy(expires = Some(now - 3600)))
    assertEquals(why(expired), "expired")
    // a token signed by someone else entirely
    val stranger = Jwt.sign(Claims(issuer = Some("https://idp"),
      audience = Vector("app"), expires = Some(now + 600)),
      Jwt.Key.Hmac("not the idp".getBytes("UTF-8")))
    assert(why(stranger).contains("signature") || why(stranger).contains("key"),
      why(stranger))
  }

  private def json(s: String): Response ! Async =
    pure(Response(200, Seq(("content-type", "application/json")),
      Http.one(s.replaceAll("\n\\s*", "").getBytes("UTF-8"))))
}
