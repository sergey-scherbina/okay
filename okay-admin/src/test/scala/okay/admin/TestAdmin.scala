package okay.admin

import okay.{!, Async}
import okay.given
import okay.http.{Http, Method, Request, Response}
import okay.security.{Claims, Jwt, Verified}
import okay.security.given

/**
 * specs/admin.md — the 401/403 ladder (delegated to Secure.granted,
 * proven identical there; re-asserted here at the route's own shape)
 * and the replay/onReplayed wiring.
 */
class TestAdmin extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  var replayed = 0
  var pinged = 0
  def route = Admin.routes(Admin.Issuer.verify)(
    replay = () => { replayed += 1; 3L },
    onReplayed = () => { pinged += 1 })

  def post(url: String, headers: (String, String)*): Response =
    run(route(Request(Method.Post, url, headers)))

  test("an unauthenticated request is refused, 401, naming no token") {
    val r = post("/admin/replay")
    assertEquals(r.status, 401)
    assert(r.header("www-authenticate").exists(_.contains("no token")))
  }

  test("a garbage token is refused, 401") {
    assertEquals(post("/admin/replay", "authorization" -> "Bearer garbage").status, 401)
  }

  test("Policy.scoped(\"admin\") is enforced, not just any valid token: 403 without the scope") {
    // the SAME key the route trusts, an HMAC secret this time (the
    // scope check is key-agnostic — TestGranted's own pattern) so a
    // "read"-scoped and an "admin"-scoped token differ in nothing
    // but the claim under test
    val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
    val key = Jwt.Key.Hmac(secret)
    val now = System.currentTimeMillis() / 1000
    def token(scopes: Set[String]): String =
      Jwt.sign(Claims(subject = Some("u1"), scopes = scopes, expires = Some(now + 600)), key)
    val verify: String => Verified = t => Jwt.verify(t, _ => Some(key), None, now)
    val r = Admin.routes(verify)(replay = () => 0L, onReplayed = () => ())(
      Request(Method.Post, "/admin/replay", Seq("authorization" -> s"Bearer ${token(Set("read"))}")))
    assertEquals(run(r).status, 403)
  }

  test("a token WITH the admin scope succeeds: replay runs, the count answers") {
    val ok = post("/admin/replay", "authorization" -> s"Bearer ${Admin.Issuer.issue()}")
    assertEquals(ok.status, 200)
    assertEquals(replayed, 1)
    assertEquals(pinged, 1)
    assert(run(Http.text(ok)).contains("3"))
  }

  test("Admin.Issuer: a token it issues is a token it verifies, carrying the admin scope") {
    val t = Admin.Issuer.issue()
    Admin.Issuer.verify(t) match
      case Verified.Ok(p) => assert(p.claims.scopes("admin"))
      case Verified.No(reason) => fail(s"expected Ok, got No($reason)")
  }

  test("definedness is the route's own — protection does not widen it") {
    assert(!route.isDefinedAt(Request(Method.Get, "/other", Nil)))
  }
}
