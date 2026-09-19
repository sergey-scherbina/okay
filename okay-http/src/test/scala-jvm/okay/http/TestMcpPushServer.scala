package okay.http

import okay.*
import okay.given
import okay.codec.Schema
import okay.mcp.{Client, Mcp, Duplex, Server as McpServer}
import okay.agent.ToolSpec

/**
 * MCP push over the JDK backend (http-streaming-responses): the same
 * TestMcpPush okay-jetty already ran, proving the gap `Server.scala`
 * had (it drained the whole body before sending headers) is closed —
 * a GET `text/event-stream` route now delivers a push before its
 * source ends, on `com.sun.net.httpserver` too.
 */
class TestMcpPushServer extends munit.FunSuite {
  // nio-port-scope (2026-09-03): a real port, see okay-jetty's own
  // TestMcpPush for the class of flake this is out of the default
  // gate for.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-push-jdk", "0.1")
  val docs = Map("okay://a" -> "alpha", "okay://b" -> "beta")

  def serving = McpServer.Serving(info,
    tools = Seq(ToolSpec[Add]("add", "add two numbers")),
    call = Map("add" -> (_ => "3")),
    resources = docs.keys.toSeq.sorted.map(u => Mcp.Resource(u, u)),
    read = docs.get)

  /** a JDK server on a free port, for the body of the test */
  def served[A](route: Request => Response ! Async)(body: String => A): A =
    Resource.run[A, Pure](Server.serve(0)(route)
      .map(s => body(s"http://127.0.0.1:${Server.port(s)}/mcp"))).runWith

  test("a subscription over HTTP delivers a push on the GET stream") {
    val s = serving
    val (route, pushes) = McpHttp.routed(s)
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("test", "1")).runWith
      assert(session.has("resources"))
      assert(session.subscribe("okay://a").runWith)

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
}
