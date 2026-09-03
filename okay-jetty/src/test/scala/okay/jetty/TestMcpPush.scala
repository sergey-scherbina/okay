package okay.jetty

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.http.{McpHttp, Request, Response, Transports}
import okay.mcp.{Client, Mcp, Duplex, Server as McpServer}
import okay.agent.{ToolCall, ToolSpec}

/**
 * MCP over streamable HTTP, with the half that needed a server able
 * to hold a stream open: a subscription, a push, and the notification
 * arriving on the client's own channel — over HTTP, with nothing in
 * okay-mcp changed.
 *
 * It runs on Jetty because Jetty is the backend that writes a body
 * incrementally; the same route on a buffering server serves POSTs
 * and delivers no pushes, which is a property of that server rather
 * than of the protocol.
 */
class TestMcpPush extends munit.FunSuite {
  // nio-port-scope (2026-09-03): this suite BINDS a real port, so its
  // result depends on what else on the machine is binding them — the
  // class of failure netty-ws-matrix-flake and nio-port-scope-flake
  // both were. Out of the default gate; `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-push", "0.1")
  val docs = Map("okay://a" -> "alpha", "okay://b" -> "beta")

  def serving = McpServer.Serving(info,
    tools = Seq(ToolSpec[Add]("add", "add two numbers")),
    call = Map("add" -> (_ => "3")),
    resources = docs.keys.toSeq.sorted.map(u => Mcp.Resource(u, u)),
    read = docs.get)

  /** a jetty server on a free port, for the body of the test */
  def served[A](route: Request => Response ! Async)(body: String => A): A =
    Resource.run[A, Pure](Jetty.serve(0)({ case r if true => route(r) })()
      .map(s => body(s"http://127.0.0.1:${Jetty.port(s)}/mcp"))).runWith

  test("a subscription over HTTP delivers a push on the GET stream") {
    val s = serving
    val (route, pushes) = McpHttp.routed(s)
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("test", "1")).runWith
      assert(session.has("resources"))
      assert(session.subscribe("okay://a").runWith)

      // the stream the server pushes on
      link.open(): Unit
      Thread.sleep(150)          // let the GET arrive before the push

      pushes.resourceUpdated("okay://a")
      val n = session.notifications.receiveBlocking()
      assertEquals(n.flatMap(Duplex.updatedUri), Some("okay://a"))
    }
  }

  test("a push is written before the stream ends — it is a STREAM") {
    val s = serving
    val (route, pushes) = McpHttp.routed(s)
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("test", "1")).runWith
      assert(session.subscribe("okay://a").runWith)
      assert(session.subscribe("okay://b").runWith)
      link.open(): Unit
      Thread.sleep(150)

      // two pushes, read one at a time: the second cannot have been
      // buffered behind an ended body, because the body has not ended
      pushes.resourceUpdated("okay://a")
      assertEquals(session.notifications.receiveBlocking().flatMap(Duplex.updatedUri),
        Some("okay://a"))
      pushes.resourceUpdated("okay://b")
      assertEquals(session.notifications.receiveBlocking().flatMap(Duplex.updatedUri),
        Some("okay://b"))
    }
  }

  test("a client that never opens the stream still works") {
    val s = serving
    val (route, pushes) = McpHttp.routed(s)
    served(route) { url =>
      val session = Client.connect(McpHttp.link(Transports.http(), url),
        Mcp.Info("test", "1")).runWith
      assert(session.subscribe("okay://a").runWith)
      pushes.resourceUpdated("okay://a")     // nowhere to go, and no fault
      assertEquals(session.call(ToolCall("c", "add", Json.JObj(Vector.empty))).runWith, "3")
    }
  }

  test("a GET without a known session is 404, not a stream") {
    val (route, _) = McpHttp.routed(serving)
    served(route) { url =>
      val r = Transports.http().send(Request.get(url,
        Seq(("accept", "text/event-stream"),
          (McpHttp.SessionHeader, "nobody")))).runWith
      assertEquals(r.status, 404)
    }
  }
}
