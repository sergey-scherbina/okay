package okay.jetty

import okay.*
import okay.given
import okay.http.{Http, Request, Response, Transports}

/**
 * specs/http.md, http-peer-address — over a REAL socket, because the
 * claim is about what a transport knows and a hand-built Request knows
 * nothing.
 */
class TestPeerAddress extends munit.FunSuite {
  // nio-port-scope (2026-09-03), applied here 2026-09-18 after this
  // suite failed in a full matrix and passed alone on the same tree:
  // it BINDS a real port and runs a loopback request against it, so
  // its result depends on what else the machine is doing — which is
  // the definition AGENTS.md gives for a suite that belongs in
  // `integrationTest`. The policy is explicit that a flake in an
  // untagged suite gets the TAG, not a retry loop and not a widened
  // assertion; the assertion below was sharpened instead of loosened.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("a served request carries the peer's host, and a built one does not") {
    val seen = java.util.concurrent.atomic.AtomicReference[Option[String]](None)
    val served = java.util.concurrent.atomic.AtomicBoolean(false)
    val routes: PartialFunction[Request, Response ! Async] =
      case r =>
        served.set(true)
        seen.set(r.peer)
        pure(Response(200, Nil, Http.one("ok".getBytes("UTF-8"))))

    Resource.run[Unit, Pure](Jetty.serve(0)(routes)().map { server =>
      val port = Jetty.port(server)
      val t = Transports.http()
      val _ = Async.run[Response, Pure](t.send(Request.get(s"http://127.0.0.1:$port/x"))).runWith

      // TWO DIFFERENT FAILURES WORE ONE MESSAGE, and the full-matrix
      // flake could not be told apart because of it: the handler may
      // never have run (the request did not arrive), or it ran and
      // Jetty reported no remote address. `served` distinguishes them
      // for whoever reads the next failure.
      val peer = seen.get()
      assert(served.get(), "the handler never ran — the request did not arrive")
      assert(peer.isDefined,
        "the handler ran but the transport reported no peer address")
      assertEquals(peer, Some("127.0.0.1"))
      // the HOST, not host:port — a port changes per connection and
      // would hand every connection a fresh rate-limit budget
      assert(!peer.get.contains(":"), s"the port leaked in: $peer")

      // a client builds its own request and knows nothing about where
      // it will arrive
      assertEquals(Request.get("http://x/y").peer, None)
    }).runWith
  }
}
