package okay.jetty

import okay.*
import okay.given
import okay.http.{Body, Http, Method, Request, Response, Transports}

/**
 * specs/http.md, http-peer-address — over a REAL socket, because the
 * claim is about what a transport knows and a hand-built Request knows
 * nothing.
 */
class TestPeerAddress extends munit.FunSuite {

  test("a served request carries the peer's host, and a built one does not") {
    val seen = java.util.concurrent.atomic.AtomicReference[Option[String]](None)
    val routes: PartialFunction[Request, Response ! Async] =
      case r =>
        seen.set(r.peer)
        pure(Response(200, Nil, Http.one("ok".getBytes("UTF-8"))))

    Resource.run[Unit, Pure](Jetty.serve(0)(routes)().map { server =>
      val port = Jetty.port(server)
      val t = Transports.http()
      val _ = Async.run[Response, Pure](t.send(Request.get(s"http://127.0.0.1:$port/x"))).runWith

      val peer = seen.get()
      assert(peer.isDefined, "the server saw no peer address")
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
