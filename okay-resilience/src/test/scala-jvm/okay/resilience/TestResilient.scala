package okay.resilience

import okay.*
import okay.given
import okay.http.{Http, Method, Request, Response}
import java.util.concurrent.atomic.AtomicInteger

/**
 * The Http layer (specs/resilience.md, stage 1): the client in its
 * fixed order, the server mapping refusals to statuses, and a
 * deadline shrinking across two hops under a controlled clock.
 */
class TestResilient extends munit.FunSuite {

  var now = 0L
  val clock: () => Long = () => now

  def run[A](prog: A ! Async): A = Async.run(prog).runWith

  /** an Http answering from a function, counting calls */
  final class Fake(answer: Request => Response ! Async) extends Http:
    val calls = AtomicInteger(0)
    var last: Request = Request.get("http://none/")
    def send(r: Request): Response ! Async = okay.async { calls.incrementAndGet(); last = r }.flatMap(_ => answer(r))

  def header(r: Request, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  def status(code: Int, body: String = ""): Response =
    Response(code, Nil, Http.one(body.getBytes("UTF-8")))

  test("client: every piece sees the request once; a 5xx trips the breaker, a 404 does not") {
    val far = Fake(r => okay.async(if r.url.endsWith("/bad") then status(503) else if r.url.endsWith("/missing") then status(404) else status(200, "ok")))
    val breaker = Breaker("far", failures = 2, openMillis = 1000, clock)
    val bulkhead = Bulkhead("far", permits = 4)
    val limiter = Limiter("far", ratePerSecond = 100, burst = 100, clock = clock)
    val http = Resilient.http(far, budgetMillis = Some(5000), breaker = Some(breaker),
      bulkhead = Some(bulkhead), limiter = Some((limiter, _ => "one")), clock = clock)

    assertEquals(run(http.send(Request.get("http://x/ok")).flatMap(Http.text)), "ok")
    assertEquals(far.calls.get, 1)
    assertEquals(limiter.stats.admitted, 1L)
    assertEquals(breaker.stats.calls, 1L)
    assertEquals(header(far.last, Deadline.header), Some("5000"))   // the budget travelled

    assertEquals(run(http.send(Request.get("http://x/missing"))).status, 404)
    assertEquals(breaker.stats.consecutiveFailures, 0)             // the far end answered
    assertEquals(run(http.send(Request.get("http://x/bad"))).status, 503)
    assertEquals(run(http.send(Request.get("http://x/bad"))).status, 503)
    assertEquals(breaker.stats.state, Breaker.State.Open)

    // open: refused locally, the wire untouched, no token spent
    val before = (far.calls.get, limiter.stats.admitted)
    val e = intercept[Refused.BreakerOpen](run(http.send(Request.get("http://x/ok"))))
    assertEquals(e.retryAfterMillis, Some(1000L))
    assertEquals((far.calls.get, limiter.stats.admitted), before)
    assertEquals(bulkhead.stats.inFlight, 0)
  }

  test("client: a Get is hedged, a Post is not") {
    val starts = AtomicInteger(0)
    val far = Fake { _ =>
      okay.async(starts.incrementAndGet()).flatMap { n =>
        if n == 1 then Async.await[Response](_ => () => ()) else okay.async(status(200, s"attempt $n"))
      }
    }
    val http = Resilient.http(far, hedge = Some((20L, 2)))
    assertEquals(run(http.send(Request.get("http://x/")).flatMap(Http.text)), "attempt 2")
    assertEquals(starts.get, 2)

    val slow = Fake(_ => Async.sleep(40).map(_ => status(200, "posted")))
    val http2 = Resilient.http(slow, hedge = Some((10L, 2)))
    assertEquals(run(http2.send(Request.post("http://x/", okay.http.Body.Text("x"))).flatMap(Http.text)), "posted")
    assertEquals(slow.calls.get, 1)
  }

  test("client: a carried deadline earlier than the budget wins, and the header says the remaining") {
    now = 1000
    val far = Fake(_ => okay.async(status(200)))
    val http = Resilient.http(far, budgetMillis = Some(500), clock = clock)
    val r = Request.get("http://x/", Seq((Deadline.header, "200")))
    assertEquals(run(http.send(r)).status, 200)
    assertEquals(header(far.last, Deadline.header), Some("200"))
    // an expired carried deadline refuses before the wire
    val e = intercept[Refused.DeadlineExceeded](run(http.send(Request.get("http://x/", Seq((Deadline.header, "0"))))))
    assert(e.remainingMillis <= 0)
    assertEquals(far.calls.get, 1)
  }

  test("server: 429 with Retry-After for an exhausted limiter keyed by peer, 503 for a full bulkhead, defined where the route is") {
    val limiter = Limiter("in", ratePerSecond = 1, burst = 1, clock = clock)
    val bulkhead = Bulkhead("in", permits = 1)
    val routes: PartialFunction[Request, Response ! Async] =
      case r if r.url == "/hello" => okay.async(status(200, "hi"))
    val guarded = Resilient.route(limiter = Some((limiter, Resilient.byPeer)), bulkhead = Some(bulkhead), clock = clock)(routes)

    assert(!guarded.isDefinedAt(Request.get("/other")))
    def from(peer: String): Request = Request(Method.Get, "/hello", peer = Some(peer))

    assertEquals(run(guarded(from("10.0.0.1"))).status, 200)
    val r2 = run(guarded(from("10.0.0.1")))
    assertEquals(r2.status, 429)
    assertEquals(r2.header("retry-after"), Some("1"))
    assertEquals(run(guarded(from("10.0.0.2"))).status, 200)   // another peer, its own bucket

    // the bulkhead: one in flight, the next refused with 503
    var hold: Either[Throwable, Response] => Unit = null
    val holding: PartialFunction[Request, Response ! Async] =
      case r if r.url == "/slow" => Async.await[Response] { k => hold = k; () => () }
    val g2 = Resilient.route(bulkhead = Some(bulkhead))(holding)
    val first = Async.spawn(g2(Request.get("/slow")))
    var spins = 0
    while hold == null && spins < 10_000_000 do spins += 1   // the permit is taken before the park
    assertEquals(bulkhead.stats.inFlight, 1)
    val r3 = run(g2(Request.get("/slow")))
    assertEquals(r3.status, 503)
    assertEquals(r3.header("retry-after"), None)
    hold(Right(status(200)))
    assertEquals(first.join().status, 200)
  }

  test("server: a route that throws something other than a refusal still throws") {
    val routes: PartialFunction[Request, Response ! Async] =
      case _ => okay.async(throw IllegalStateException("mine"))
    val e = intercept[IllegalStateException](run(Resilient.route()(routes)(Request.get("/"))))
    assertEquals(e.getMessage, "mine")
  }

  test("two hops: hop 1's work consumes the budget, and the call to hop 2 is refused before the wire") {
    // the header is RELATIVE (gRPC's model): transit is not charged,
    // only what a hop spends before calling on — so the budget shrinks
    // by work, and a hop that has spent it all refuses to call further
    val far = Fake(_ => okay.async(status(200, "deep")))
    val hop2 = Resilient.route(clock = clock) { case r => far.send(r) }
    val toHop2: Http = new Http:
      def send(r: Request): Response ! Async = hop2(r)

    def hop1(work: Long): PartialFunction[Request, Response ! Async] =
      Resilient.route(clock = clock) { case r =>
        val inbound = Deadline.read(r, clock).get
        okay.async { now += work }.flatMap { _ =>
          Resilient.http(toHop2, clock = clock).send(Deadline.carry(Request.get("http://hop2/"), inbound, clock))
        }
      }

    now = 0
    val fast = run(hop1(20)(Request.get("http://hop1/", Seq((Deadline.header, "100")))))
    assertEquals(fast.status, 200)
    assertEquals(header(far.last, Deadline.header), Some("80"))   // what was left after the work

    now = 0
    val slow = run(hop1(120)(Request.get("http://hop1/", Seq((Deadline.header, "100")))))
    assertEquals(slow.status, 504)
    assertEquals(far.calls.get, 1)                                 // hop 2 was never asked
  }
}
