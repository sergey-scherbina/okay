package okay.security

import okay.{!, Async, provide, pure}
import okay.given
import okay.codec.Json
import okay.http.{Http, Method, Request, Response}

/** each door delegates to its explicit form — one assertion per
 * door, against stubs where a wire would be */
class TestDoors extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** an Http stub answering a fixed response — the wire, removed */
  def stub(status: Int, body: String): Http = new Http:
    def send(r: Request): Response ! Async =
      pure(Response(status, Nil, okay.http.Http.one(body.getBytes("UTF-8"))))

  val client = OAuth2.Client("cid", None, "http://as/a", "http://as/t", "http://app/cb", Nil)

  test("OAuth2 doors: exchange/refresh/clientCredentials under an ambient Http") {
    val ok = stub(200, """{"access_token":"tok","token_type":"Bearer"}""")
    provide(ok) {
      assertEquals(run(OAuth2.exchange(client, "code", "ver")).map(_.access), Right("tok"))
      assertEquals(run(OAuth2.refresh(client, "r")).map(_.access), Right("tok"))
      assertEquals(run(OAuth2.clientCredentials(client)).map(_.access), Right("tok"))
    }
    // and the refusal path flows the same
    provide(stub(400, """{"error":"invalid_grant"}""")) {
      assert(run(OAuth2.exchange(client, "code", "ver")).isLeft)
    }
  }

  test("Jwks door: fetch under an ambient Http parses the set") {
    provide(stub(200, """{"keys":[]}""")) {
      assertEquals(run(Jwks.fetch("http://issuer/jwks")), Map.empty)
    }
  }

  test("McpAuth.granted: the principal ambient, the ladder protect's") {
    val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
    val now = 1_700_000_000L
    val tok = Jwt.sign(Claims(subject = Some("u1"), audience = Vector("api"),
      expires = Some(now + 600), scopes = Set("read"),
      json = Json.JObj(Vector("name" -> Json.JStr("Ada")))), Jwt.Key.Hmac(secret))
    val route = McpAuth.granted(
      t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now),
      "http://x/.well-known/oauth-protected-resource") { r =>
      pure(Response(200, Nil,
        okay.http.Http.one(s"mcp for ${summon[Principal].name}".getBytes("UTF-8"))))
    }
    val ok = run(route(Request(Method.Post, "/mcp",
      Seq("authorization" -> s"Bearer $tok"))))
    assertEquals(ok.status, 200)
    assertEquals(run(okay.http.Http.text(ok)), "mcp for Ada")
    val no = run(route(Request(Method.Post, "/mcp", Nil)))
    assertEquals(no.status, 401)
    assert(no.header("www-authenticate").exists(_.contains("resource_metadata")))
  }
}
