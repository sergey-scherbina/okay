package okay.obs

import okay.{!, Async, Pure, Resource, pure}
import okay.given
import okay.codec.Json
import okay.http.{Http, Request, Response, Server, Transports}
import okay.persist.{MemoryStore, Policy}

/**
 * Export is a consumer, proven against a RECORDING fake collector:
 * the OTLP shape a real ingester expects, offsets as resume tokens,
 * nothing-new ships nothing, and a refusing collector leaves the
 * batch unconsumed for the retry — at-least-once, as trace
 * ingestion expects.
 */
class TestOtlp extends munit.FunSuite {
  // nio-port-scope (2026-09-03): this suite BINDS a real port, so its
  // result depends on what else on the machine is binding them — the
  // class of failure netty-ws-matrix-flake and nio-port-scope-flake
  // both were. Out of the default gate; `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  def fixture(): (okay.persist.Topic, Tracer) =
    val topic = MemoryStore().topic("__trace", 1, Policy())
    (topic, Tracer(topic, clock = {
      var t = 1000L
      () => { t += 1; t }
    }))

  /** a recording collector; `status` controls its mood */
  def collector[A](status: Int = 200)(body: (Int, () => Vector[String]) => A): A =
    var seen = Vector.empty[String]
    val route: PartialFunction[Request, Response ! Async] = {
      case r if r.url.contains("/v1/traces") =>
        seen = seen :+ String(r.body.bytes, "UTF-8")
        pure(Response(status, Nil, Http.one(Array.empty)))
    }
    Resource.run[A, Pure](Server.serve(0)(route).map(s =>
      body(Server.port(s), () => seen))).runWith

  val http = Transports.http()

  test("the wire shape: ids, parentage, nanos-as-strings, attributes, both statuses") {
    val (topic, tracer) = fixture()
    tracer.root("GET /q") {
      tracer.span("sql", Attr("db.system", "h2")) { () }
      intercept[RuntimeException](tracer.span("boom") { throw RuntimeException("nope") })
    }: Unit
    collector() { (port, seen) =>
      val out = run(OtlpPush.push(http, topic, s"http://127.0.0.1:$port", "svc", 0))
      assertEquals(out, Right(3L))
      val text = seen().head
      assert(Json.parse(text).isInstanceOf[Json.JObj])   // it IS json, then the shape by content
      assert(text.contains("\"service.name\""), text)
      assert(text.contains("\"okay-obs\""))
      assert(text.contains("\"GET /q\"") && text.contains("\"sql\"") && text.contains("\"boom\""))
      // nanos as strings: start 1001ms -> "1001000000"
      assert(text.contains("\"1001000000\"") || text.contains("\"1002000000\""), text)
      assert(text.contains("\"parentSpanId\""))
      assert(text.contains("\"db.system\""))
      assert(text.contains("\"message\":\"nope\""), text)   // status code 2 carries it
      assert(text.contains("\"code\":1") && text.contains("\"code\":2"))
    }
  }

  test("offsets are resume tokens: nothing new ships nothing; new spans ship from `next`") {
    val (topic, tracer) = fixture()
    tracer.root("first") { () }
    collector() { (port, seen) =>
      val ep = s"http://127.0.0.1:$port"
      val Right(next) = run(OtlpPush.push(http, topic, ep, "svc", 0)): @unchecked
      assertEquals(run(OtlpPush.push(http, topic, ep, "svc", next)), Right(next))
      assertEquals(seen().length, 1)                        // the quiet push sent nothing
      tracer.root("second") { () }
      val Right(after) = run(OtlpPush.push(http, topic, ep, "svc", next)): @unchecked
      assert(after > next)
      assertEquals(seen().length, 2)
      assert(seen().last.contains("\"second\"") && !seen().last.contains("\"first\""))
    }
  }

  test("a refusing collector leaves the batch unconsumed — the retry re-ships it") {
    val (topic, tracer) = fixture()
    tracer.root("kept") { () }
    collector(status = 503) { (port, seen) =>
      val out = run(OtlpPush.push(http, topic, s"http://127.0.0.1:$port", "svc", 0))
      assert(out.left.exists(_.contains("503")), out.toString)
      assertEquals(seen().length, 1)   // it TRIED — and the offset did not advance
    }
    // the retry against a healthy collector ships the same batch
    collector() { (port, seen) =>
      val out = run(OtlpPush.push(http, topic, s"http://127.0.0.1:$port", "svc", 0))
      assertEquals(out.map(_ > 0), Right(true))
      assert(seen().head.contains("\"kept\""))
    }
  }
}
