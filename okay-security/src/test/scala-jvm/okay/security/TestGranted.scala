package okay.security

import okay.{!, Async, pure}
import okay.given
import okay.codec.Json
import okay.http.{Http, Method, Request, Response}

/** the capability form of the route wrapper: the principal ambient,
 * the 401/403 ladder byte-identical to bearer's, additive */
class TestGranted extends munit.FunSuite {

  val now = 1_700_000_000L
  val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
  def token(scopes: Set[String]): String =
    Jwt.sign(Claims(subject = Some("u1"), audience = Vector("api"),
      expires = Some(now + 600), scopes = scopes,
      json = Json.JObj(Vector("name" -> Json.JStr("Ada")))), Jwt.Key.Hmac(secret))
  val verify: String => Verified =
    t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now)

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  val route = Secure.granted(verify, Policy.scoped("read")) {
    // no lambda parameter: the principal is ambient
    case r if r.url.startsWith("/hello") =>
      pure(Response(200, Nil,
        Http.one(s"hello ${summon[Principal].name}".getBytes("UTF-8"))))
  }

  def get(url: String, headers: (String, String)*): Response =
    run(route(Request(Method.Get, url, headers)))

  test("the handler reads the ambient principal") {
    val ok = get("/hello", "authorization" -> s"Bearer ${token(Set("read"))}")
    assertEquals(ok.status, 200)
    assertEquals(run(Http.text(ok)), "hello Ada")
  }

  test("the 401/403 ladder is bearer's, byte for byte") {
    val none = get("/hello")
    assertEquals(none.status, 401)
    assert(none.header("www-authenticate").exists(_.contains("no token")))
    assertEquals(get("/hello", "authorization" -> "Bearer garbage").status, 401)
    assertEquals(get("/hello",
      "authorization" -> s"Bearer ${token(Set("write"))}").status, 403)
  }

  test("definedness is the route's, protection does not widen it") {
    assert(!route.isDefinedAt(Request(Method.Get, "/other", Nil)))
  }
}
