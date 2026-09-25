package okay.security

import okay.{!, Async, pure}
import okay.given
import okay.codec.Json
import okay.http.{Method, Request, Response}

/** McpAuth's door, which was TestDoors' last assertion until McpAuth
 * moved to okay-mcp-http (http-mcp-agent-edge, 2026-09-25) */
class TestMcpDoor extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  test("McpAuth.granted: the principal ambient, the ladder protect's") {
    val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
    val now = 1_700_000_000L
    val tok = Jwt.sign(Claims(subject = Some("u1"), audience = Vector("api"),
      expires = Some(now + 600), scopes = Set("read"),
      json = Json.JObj(Vector("name" -> Json.JStr("Ada")))), Jwt.Key.Hmac(secret))
    val route = McpAuth.granted(
      t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now),
      "http://x/.well-known/oauth-protected-resource") { _ =>
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
