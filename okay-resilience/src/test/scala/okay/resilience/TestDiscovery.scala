package okay.resilience

import okay.*
import okay.http.{Http, Request, Response}

/**
 * Discovery sources and the balancer (specs/discovery.md), all under
 * an injected clock and a fake far end — the suite runs on JS too.
 */
class TestDiscovery extends munit.FunSuite:

  var now = 0L
  val clock: () => Long = () => now

  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  def ep(s: String): Endpoint = Discovery.parse(s).head

  test("parse: a list, a default port, a damaged entry dropped") {
    assertEquals(Discovery.parse("a:1, b:2 ,c", defaultPort = 80),
      Vector(Endpoint("a", 1), Endpoint("b", 2), Endpoint("c", 80)))
    assertEquals(Discovery.parse("a:x,b:2,:3,d:70000"), Vector(Endpoint("b", 2)))
    assertEquals(Discovery.parse(""), Vector.empty)
  }

  test("env: the Kubernetes pair first, the comma list second, nothing set is empty") {
    val vars = Map("ORDERS_SERVICE_HOST" -> "10.0.0.5", "ORDERS_SERVICE_PORT" -> "8080",
      "OKAY_SERVICE_ORDERS" -> "ignored:1", "OKAY_SERVICE_PAY_API" -> "p1:9000,p2:9000")
    val d = Discovery.env(vars.get)
    assertEquals(run(d.resolve("orders")), Vector(Endpoint("10.0.0.5", 8080)))
    assertEquals(run(d.resolve("pay-api")), Vector(Endpoint("p1", 9000), Endpoint("p2", 9000)))
    assertEquals(run(d.resolve("nothing")), Vector.empty)
  }

  test("chain takes the first non-empty answer; cached asks once per ttl") {
    var asked = 0
    val counting = new Discovery:
      def resolve(s: String) = okay.async { asked += 1; Vector(Endpoint(s, 1)) }
    val d = Discovery.chain(Discovery.static(Map("a" -> Vector(Endpoint("static", 9)))), counting)
    assertEquals(run(d.resolve("a")), Vector(Endpoint("static", 9)))
    assertEquals(run(d.resolve("b")), Vector(Endpoint("b", 1)))
    assertEquals(asked, 1)

    val c = Discovery.cached(counting, ttlMillis = 100, clock)
    now = 0
    assertEquals(run(c.resolve("x")), Vector(Endpoint("x", 1)))
    assertEquals(run(c.resolve("x")), Vector(Endpoint("x", 1)))
    assertEquals(asked, 2)
    now = 100
    assertEquals(run(c.resolve("x")), Vector(Endpoint("x", 1)))
    assertEquals(asked, 3)
  }

  /** a far end that records the URL it was asked and fails for hosts in `dead` */
  final class Far(var dead: Set[String] = Set.empty) extends Http:
    var asked = Vector.empty[String]
    def send(r: Request): Response ! Async = okay.async {
      asked :+= r.url
      val host = Url.split(r.url).get.host
      if dead(host) then throw java.io.IOException(s"$host down")
      Response(if host == "sick" then 503 else 200, Nil, Http.one(Array.empty))
    }

  test("balanced: round-robin over the service's endpoints, path and query kept, unknown hosts pass through") {
    val far = Far()
    val d = Discovery.static(Map("orders" -> Vector(Endpoint("a", 1), Endpoint("b", 2))))
    val http = Balanced(d, clock = clock).http(far)
    for _ <- 1 to 3 do run(http.send(Request.get("http://orders/v1/x?q=1")))
    assertEquals(run(http.send(Request.get("https://api.example.com/z"))).status, 200)
    assertEquals(far.asked, Vector("http://a:1/v1/x?q=1", "http://b:2/v1/x?q=1", "http://a:1/v1/x?q=1", "https://api.example.com/z"))
  }

  test("balanced: a thrown wire error cools the endpoint down; a 503 does not; after the cool-down it is tried again") {
    val far = Far(dead = Set("a"))
    val d = Discovery.static(Map("svc" -> Vector(Endpoint("a", 1), Endpoint("sick", 2), Endpoint("c", 3))))
    val b = Balanced(d, cooldownMillis = 1000, clock)
    val http = b.http(far)
    now = 0
    assertEquals(intercept[java.io.IOException](run(http.send(Request.get("http://svc/")))).getMessage, "a down")
    // the cursor keeps counting over the LIVING pool [sick, c]: c, sick, c
    assertEquals(run(http.send(Request.get("http://svc/"))).status, 200)          // c
    assertEquals(run(http.send(Request.get("http://svc/"))).status, 503)          // sick answered: not marked
    assertEquals(run(http.send(Request.get("http://svc/"))).status, 200)          // c again: a is skipped
    assertEquals(b.stats.down, Vector("a:1"))
    assertEquals((b.stats.picked, b.stats.failed), (4L, 1L))

    now = 1000
    far.dead = Set.empty
    for _ <- 1 to 3 do assert(run(http.send(Request.get("http://svc/"))).status > 0)
    assert(far.asked.takeRight(3).exists(_.startsWith("http://a:1")), far.asked.toString)  // a is back
    assertEquals(b.stats.down, Vector.empty)
  }

  test("balanced: all endpoints down → the least recently failed is tried; an empty resolution refuses") {
    val far = Far(dead = Set("a", "b"))
    val d = Discovery.static(Map("svc" -> Vector(Endpoint("a", 1), Endpoint("b", 2)), "empty" -> Vector.empty))
    val b = Balanced(d, cooldownMillis = 1000, clock)
    val http = b.http(far)
    now = 0
    assertEquals(intercept[java.io.IOException](run(http.send(Request.get("http://svc/")))).getMessage, "a down")   // at 0
    now = 10
    assertEquals(intercept[java.io.IOException](run(http.send(Request.get("http://svc/")))).getMessage, "b down")   // at 10
    now = 20
    far.dead = Set.empty
    assertEquals(run(http.send(Request.get("http://svc/"))).status, 200)
    assert(far.asked.last.startsWith("http://a:1"), far.asked.last)              // a failed earlier than b

    val e = intercept[Refused.NoEndpoint](run(http.send(Request.get("http://empty/"))))
    assertEquals(e.service, "empty")
    assertEquals(b.stats.refused, 1L)
    assertEquals(Resilient.status(e), 503)
  }
