package okay.obs

import okay.{!, Async, pure}
import okay.given
import okay.codec.{Cbor, Json}
import okay.http.{Http, Method, Request, Response}
import okay.persist.{MemoryStore, Policy as TopicPolicy, Topic}
import okay.security.{Claims, Jwt, Policy, Principal, Secure, Verified}
import okay.security.given

/**
 * The composition crown (specs/context-functions.md, ctx-principal):
 * ONE stored (Principal, Tracer) ?=> Route — protected AND traced —
 * self-wiring where both capabilities are installed. Deferred
 * requirements compose as arrows.
 */
class TestComposed extends munit.FunSuite {

  val now = 1_700_000_000L
  val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
  def token: String = Jwt.sign(Claims(subject = Some("u1"),
    audience = Vector("api"), expires = Some(now + 600), scopes = Set("read"),
    json = Json.JObj(Vector("name" -> Json.JStr("Ada")))), Jwt.Key.Hmac(secret))
  val verify: String => Verified =
    t => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now)

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** the stored value: needs BOTH capabilities, uses both */
  val api: (Principal, Tracer) ?=> Traced.Route = {
    case r if r.url.contains("/quote") =>
      okay.async {
        summon[Tracer].span("db.lookup") { () }
        Response(200, Nil,
          Http.one(s"for:${summon[Principal].name}".getBytes("UTF-8")))
      }
  }

  test("protected AND traced, one value: the principal answers, the spans land") {
    val topic = MemoryStore().topic("__trace", 1, TopicPolicy())
    val wired: Traced.Route =
      Traced.route(() => Tracer(topic))(Secure.granted(verify, Policy.scoped("read"))(api))
    val ok = run(wired(Request(Method.Get, "http://x/quote",
      Seq("authorization" -> s"Bearer $token",
        "traceparent" -> "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
    assertEquals(ok.status, 200)
    assertEquals(run(Http.text(ok)), "for:Ada")
    val spans = topic.read(0, 0, 100) match
      case Topic.Read.Records(rs) => rs.flatMap(r => Cbor.read[Span](r.value).toOption)
      case _ => Vector.empty
    val root = spans.find(_.name == "GET /quote").get
    assertEquals(root.traceId, "4bf92f3577b34da6a3ce929d0e0e4736")
    assertEquals(spans.find(_.name == "db.lookup").get.parentId, Some(root.spanId))
    // and the door still holds: no token, no handler — but the span
    // of the REFUSED request still lands (the route ran, refusing)
    assertEquals(run(wired(Request(Method.Get, "http://x/quote", Nil))).status, 401)
  }
}
