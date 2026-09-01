package okay.obs

import okay.{!, Async, pure}
import okay.given
import okay.codec.Cbor
import okay.http.{Http, Method, Request, Response}
import okay.persist.{MemoryStore, Policy, Topic}

/** Tracer ?=> Route: the per-request root from the inbound header,
 * ambient children, and the stored route self-wiring twice */
class TestTraced extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  def spansOf(topic: Topic): Vector[Span] =
    topic.read(0, 0, 100) match
      case Topic.Read.Records(rs) => rs.flatMap(r => Cbor.read[Span](r.value).toOption)
      case _ => Vector.empty

  /** a route that opens a CHILD span through the ambient tracer */
  val q: Tracer ?=> Traced.Route =
    (t: Tracer) ?=> {
      case r if r.url.contains("/q") =>
        okay.async {
          t.span("db.lookup") { () }
          Response(200, Nil, Http.one("ok".getBytes))
        }
    }

  val inbound = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

  test("the root carries the inbound trace id and the route's name; children parent to it") {
    val topic = MemoryStore().topic("__trace", 1, Policy())
    val route = Traced.route(() => Tracer(topic))(q)
    val req = Request(Method.Get, "http://x/q?n=1", Seq("traceparent" -> inbound))
    assert(route.isDefinedAt(req))
    assertEquals(run(route(req)).status, 200)
    val ss = spansOf(topic)
    val root = ss.find(_.name == "GET /q").get
    val child = ss.find(_.name == "db.lookup").get
    assertEquals(root.traceId, "4bf92f3577b34da6a3ce929d0e0e4736")
    assertEquals(root.parentId, Some("00f067aa0ba902b7"))
    assertEquals(child.traceId, root.traceId)
    assertEquals(child.parentId, Some(root.spanId))
  }

  test("a STORED route value installs at two tracers and parents to each") {
    val t1 = MemoryStore().topic("__trace", 1, Policy())
    val t2 = MemoryStore().topic("__trace", 1, Policy())
    val r1 = Traced.route(() => Tracer(t1))(q)
    val r2 = Traced.route(() => Tracer(t2))(q)
    val req = Request(Method.Get, "http://x/q", Nil)
    assertEquals(run(r1(req)).status, 200)
    assertEquals(run(r2(req)).status, 200)
    // each topic holds its own pair, each child under its own root
    for topic <- Seq(t1, t2) do
      val ss = spansOf(topic)
      assertEquals(ss.map(_.name).toSet, Set("GET /q", "db.lookup"))
      assertEquals(ss.find(_.name == "db.lookup").get.parentId,
        Some(ss.find(_.name == "GET /q").get.spanId))
  }

  test("an untraced route is untouched — additive") {
    val plain: Traced.Route = {
      case r if r.url.contains("/p") => pure(Response(204, Nil, Http.one(Array.empty)))
    }
    assertEquals(run(plain(Request(Method.Get, "http://x/p", Nil))).status, 204)
  }
}
