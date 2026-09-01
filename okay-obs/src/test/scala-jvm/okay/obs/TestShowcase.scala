package okay.obs

import okay.{!, Async, provide, providing, wire}
import okay.given
import okay.codec.Json
import okay.http.{Http, Method, Request, Response}
import okay.persist.{MemoryStore, Policy as TopicPolicy}
import okay.security.{Claims, Jwt, Policy, Principal, Secure, Verified}
import okay.security.given

/**
 * The payoff of the context-function arc, on one page
 * (specs/context-functions.md, E1-E19): needs are TYPES, doors
 * defer them, provide/providing install them, wire consumes them —
 * and ONE value lives in two worlds without changing a letter.
 */
class TestShowcase extends munit.FunSuite {

  // ---- the program: its needs are its TYPE, nothing else --------
  val api: (Principal, Tracer) ?=> Traced.Route = {
    case r if r.url.contains("/quote") =>
      okay.async {
        wire[Tracer].span("db.lookup") { () }
        Response(200, Nil,
          Http.one(s"for:${wire[Principal].name}".getBytes("UTF-8")))
      }
  }

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  // ---- world one: the production edge ---------------------------
  // the doors install the capabilities from the wire — a verified
  // JWT becomes the Principal, a traceparent becomes the Tracer
  val now = 1_700_000_000L
  val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
  def token: String = Jwt.sign(Claims(subject = Some("u1"),
    audience = Vector("api"), expires = Some(now + 600), scopes = Set("read"),
    json = Json.JObj(Vector("name" -> Json.JStr("Ada")))), Jwt.Key.Hmac(secret))
  val verify: String => Verified =
    t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now)

  test("production: the same value behind JWT and tracing") {
    val topic = MemoryStore().topic("__trace", 1, TopicPolicy())
    val edge: Traced.Route =
      Traced.route(() => Tracer(topic))(
        Secure.granted(verify, Policy.scoped("read"))(api))
    val ok = run(edge(Request(Method.Get, "http://x/quote",
      Seq("authorization" -> s"Bearer $token"))))
    assertEquals(run(Http.text(ok)), "for:Ada")
    assertEquals(run(edge(Request(Method.Get, "http://x/quote", Nil))).status, 401)
  }

  // ---- world two: the unit test ---------------------------------
  // provide installs the SAME needs directly — no token, no wire
  // format, no HTTP machinery; a missing capability would not compile
  test("unit: the same value under provide — no tokens anywhere") {
    val ada = Principal("u1", "Ada", Claims(subject = Some("u1")))
    val tracer = Tracer(MemoryStore().topic("__t", 1, TopicPolicy()))
    val route = provide(ada, tracer)(api)
    val ok = run(route(Request(Method.Get, "http://x/quote", Nil)))
    assertEquals(run(Http.text(ok)), "for:Ada")
  }

  // ---- and environments are VALUES: build once, override one ----
  test("providing: one base, one overridden layer") {
    val topic = MemoryStore().topic("__t2", 1, TopicPolicy())
    val base = providing[Principal](Principal("u1", "Ada", Claims())) and
      providing[Tracer](Tracer(topic))
    val asBob = base and
      providing[Principal](Principal("u2", "Bob", Claims()))
    val ok = run((asBob { api })(Request(Method.Get, "http://x/quote", Nil)))
    assertEquals(run(Http.text(ok)), "for:Bob")
  }
}
