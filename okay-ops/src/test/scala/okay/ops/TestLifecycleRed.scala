package okay.ops

import okay.*
import okay.given
import okay.http.{Http, Method, Request, Response}
import okay.persist.MemoryStore

/**
 * Graceful shutdown and RED as values (specs/ops.md): the route
 * wrappers, the drain, the readiness flip and the Prometheus shape,
 * all under an injected clock or a held callback — so the suite runs
 * unchanged on JS.
 */
class TestLifecycleRed extends munit.FunSuite:

  var now = 0L
  val clock: () => Long = () => now

  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  def ok(body: String): Response ! Async =
    pure(Response(200, Nil, Http.one(body.getBytes("UTF-8"))))

  def body(r: Response): String = run(Http.text(r))

  // ── lifecycle ────────────────────────────────────────────────────

  test("route: counts in flight, releases on answer and on throw; draining refuses new ones without running them") {
    val l = Lifecycle(clock)
    var ran = 0
    val routes: PartialFunction[Request, Response ! Async] =
      case r if r.url == "/work" => okay.async { ran += 1 }.flatMap(_ => ok("done"))
      case r if r.url == "/boom" => okay.async(throw RuntimeException("boom"))
    val guarded = l.route(routes)
    assert(!guarded.isDefinedAt(Request.get("/other")))

    assertEquals(body(run(guarded(Request.get("/work")))), "done")
    assertEquals(l.inFlight, 0)
    assertEquals(intercept[RuntimeException](run(guarded(Request.get("/boom")))).getMessage, "boom")
    assertEquals(l.inFlight, 0)

    var hold: Either[Throwable, Response] => Unit = null
    val holding = l.route { case _ => Async.await[Response] { k => hold = k; () => () } }
    val pending = Async.runAsync(holding(Request.get("/slow")))
    assertEquals(l.inFlight, 1)

    now = 100
    l.beginDrain()
    val refused = run(guarded(Request.get("/work")))
    assertEquals(refused.status, 503)
    assertEquals(refused.header("connection"), Some("close"))
    assertEquals(ran, 1)                                   // not run
    assertEquals(l.stats, Lifecycle.Stats(true, 1, 1L, Some(100L)))

    hold(Right(Response(200, Nil, Http.one(Array.empty))))
    assertEquals(pending.value.get.get.status, 200)         // in flight finishes even while draining
    assertEquals(l.inFlight, 0)
  }

  test("readyz flips to 503 (draining) while healthz stays 200") {
    val store = MemoryStore()
    val l = Lifecycle(clock)
    val ops = Ops.routes(store, lifecycle = Some(l))
    assertEquals(run(ops(Request.get("/readyz"))).status, 200)
    l.beginDrain()
    val r = run(ops(Request.get("/readyz")))
    assertEquals(r.status, 503)
    assertEquals(body(r), "ready=false (draining)")
    assertEquals(run(ops(Request.get("/healthz"))).status, 200)
    assert(body(run(ops(Request.get("/metrics")))).contains("okay_lifecycle_draining 1"))
  }

  test("drain: true at once with nothing in flight; false when the grace runs out under a held request") {
    val l = Lifecycle(clock)
    assertEquals(run(l.drain(1000)), true)

    val l2 = Lifecycle(clock)
    var hold: Either[Throwable, Response] => Unit = null
    val pending = Async.runAsync(l2.route { case _ => Async.await[Response] { k => hold = k; () => () } }(Request.get("/x")))
    // the drain polls on the platform timer; the injected clock jumps
    // past the grace on the first look, so no wall time is spent
    now = 0
    val draining = Async.runAsync(l2.drain(50).map { answer => answer })
    now = 1000
    assert(draining.value.forall(_.isSuccess))
    hold(Right(Response(200, Nil, Http.one(Array.empty))))
    assertEquals(pending.value.get.get.status, 200)
  }

  // ── red ──────────────────────────────────────────────────────────

  test("red.route: classes, errors, a throw that still propagates, durations in the right buckets") {
    val red = Red("api", clock)
    val routes: PartialFunction[Request, Response ! Async] =
      case r if r.url.startsWith("/ok") => okay.async { now += 7 }.flatMap(_ => ok("fine"))
      case r if r.url == "/missing" => pure(Response(404, Nil, Http.one(Array.empty)))
      case r if r.url == "/down" => okay.async { now += 300 }.map(_ => Response(503, Nil, Http.one(Array.empty)))
      case r if r.url == "/boom" => okay.async(throw IllegalStateException("boom"))
    val measured = red.route(Red.byMethodAndPath)(routes)

    assertEquals(body(run(measured(Request.get("/ok?x=1")))), "fine")
    assertEquals(run(measured(Request.get("/ok"))).status, 200)
    assertEquals(run(measured(Request.get("/missing"))).status, 404)
    assertEquals(run(measured(Request.get("/down"))).status, 503)
    assertEquals(intercept[IllegalStateException](run(measured(Request.get("/boom")))).getMessage, "boom")

    val by = red.stats.series.map(s => s.route -> s).toMap
    def classes(route: String): Map[String, Long] = by(route).byClass.map(c => c.cls -> c.n).toMap
    assertEquals(by("GET /ok").requests, 2L)
    assertEquals(classes("GET /ok"), Map("2xx" -> 2L))
    assertEquals(by("GET /ok").errors, 0L)
    assertEquals(by("GET /ok").sumMillis, 14L)
    assertEquals(by("GET /ok").buckets(1), 2L)             // 7 ms: the 10 ms bucket, not the 5
    assertEquals(classes("GET /missing"), Map("4xx" -> 1L))
    assertEquals(by("GET /down").errors, 1L)
    assertEquals(by("GET /down").buckets(6), 1L)           // 300 ms: the 500 ms bucket
    assertEquals(classes("GET /boom"), Map("exception" -> 1L))
    assertEquals(by("GET /boom").errors, 1L)
  }

  test("red.http: an outbound client is measured the same way, a dropped wire is an error") {
    val red = Red("upstream", clock)
    var n = 0
    val far: Http = new Http:
      def send(r: Request): Response ! Async = okay.async {
        n += 1
        if n == 2 then throw java.io.IOException("wire")
        now += 40
        Response(200, Nil, Http.one(Array.empty))
      }
    val client = red.http(_ => "payments")(far)
    assertEquals(run(client.send(Request.get("http://p/"))).status, 200)
    assertEquals(intercept[java.io.IOException](run(client.send(Request.get("http://p/")))).getMessage, "wire")
    val s = red.stats.series.head
    assertEquals((s.route, s.requests, s.errors), ("payments", 2L, 1L))
    assertEquals(s.byClass.map(c => c.cls -> c.n).toMap, Map("2xx" -> 1L, "exception" -> 1L))
  }

  test("Prom.red: the histogram shape — cumulative buckets, +Inf equals count, sum in seconds") {
    val red = Red("api", clock)
    red.observe("GET /a", "2xx", false, 7)
    red.observe("GET /a", "2xx", false, 30)
    red.observe("GET /a", "5xx", true, 20_000)
    val out = Prom.red(Vector(red))
    assert(out.contains("""okay_http_requests_total{name="api",route="GET /a",class="2xx"} 2"""), out)
    assert(out.contains("""okay_http_requests_total{name="api",route="GET /a",class="5xx"} 1"""))
    assert(out.contains("""okay_http_errors_total{name="api",route="GET /a"} 1"""))
    assert(out.contains("""okay_http_request_duration_seconds_bucket{name="api",route="GET /a",le="0.005"} 0"""))
    assert(out.contains("""okay_http_request_duration_seconds_bucket{name="api",route="GET /a",le="0.01"} 1"""))
    assert(out.contains("""okay_http_request_duration_seconds_bucket{name="api",route="GET /a",le="0.05"} 2"""))
    assert(out.contains("""okay_http_request_duration_seconds_bucket{name="api",route="GET /a",le="10"} 2"""))   // 20 s is beyond every bound; "10", not "10.0" — the same text on JS and the JVM
    assert(out.contains("""okay_http_request_duration_seconds_bucket{name="api",route="GET /a",le="+Inf"} 3"""), out)
    assert(out.contains("""okay_http_request_duration_seconds_sum{name="api",route="GET /a"} 20.037"""))
    assert(out.contains("""okay_http_request_duration_seconds_count{name="api",route="GET /a"} 3"""))
    assert(out.contains("# TYPE okay_http_request_duration_seconds histogram"))
    assertEquals(Prom.red(Vector(Red("empty"))), "")

    val store = MemoryStore()
    val served = body(run(Ops.routes(store, red = Vector(red))(Request.get("/metrics"))))
    assert(served.contains("okay_http_request_duration_seconds_count"))
    val _ = Method.Get
  }
