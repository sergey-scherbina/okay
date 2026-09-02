package okay.jetty

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{McpHttp, Request, Response, Transports}
import okay.mcp.{Client, Duplex, Mcp, Server as McpServer}

import okay.persist.MemoryStore

/**
 * The resumable GET stream (specs/mcp.md v7): pushes journaled into a
 * topic, frames carrying `id: <offset>`, `Last-Event-ID` replaying
 * exactly what a dropped stream missed — over a real Jetty server on
 * a real port.
 */
class TestResumable extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-sse", "0.1")
  def serving = McpServer.Serving(info,
    resources = Seq(Mcp.Resource("okay://a", "a"), Mcp.Resource("okay://b", "b")),
    read = Map("okay://a" -> "alpha", "okay://b" -> "beta").get)

  def served[A](route: Request => Response ! Async)(body: String => A): A =
    Resource.run[A, Pure](Jetty.serve(0)({ case r if true => route(r) })()
      .map(s => body(s"http://127.0.0.1:${Jetty.port(s)}/mcp"))).runWith

  def push(pushes: McpServer.Pushes, uri: String): Unit =
    pushes.resourceUpdated(uri)

  test("a dropped stream resumes: the missed pushes replay, in order, then live ones") {
    val topic = MemoryStore().topic("mcp-pushes", 2)
    val (route, pushes) = McpHttp.routed(serving, journal = Some(topic))
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("t", "1")).runWith
      assert(session.subscribe("okay://a").runWith)
      assert(session.subscribe("okay://b").runWith)

      // stream up; receive one push
      val f1 = link.open()
      Thread.sleep(150)
      push(pushes, "okay://a")
      assertEquals(Duplex.updatedUri(session.notifications.receiveBlocking().get), Some("okay://a"))
      assert(link.lastEventId.isDefined, "no id: arrived on the frame")

      // the stream DROPS (the fiber is killed, not the link)
      f1.cancel()
      Thread.sleep(50)
      push(pushes, "okay://b")
      push(pushes, "okay://a")

      // re-open: Last-Event-ID replays the two missed, in order
      link.open(): Unit
      assertEquals(session.notifications.receiveBlocking().flatMap(Duplex.updatedUri), Some("okay://b"))
      assertEquals(session.notifications.receiveBlocking().flatMap(Duplex.updatedUri), Some("okay://a"))

      // and the stream is LIVE again, not just a replay
      Thread.sleep(150)
      push(pushes, "okay://b")
      assertEquals(session.notifications.receiveBlocking().flatMap(Duplex.updatedUri), Some("okay://b"))
    }
  }

  test("a fresh GET sees only what happens after it opened") {
    val topic = MemoryStore().topic("mcp-pushes", 2)
    val (route, pushes) = McpHttp.routed(serving, journal = Some(topic))
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("t", "1")).runWith
      assert(session.subscribe("okay://a").runWith)

      push(pushes, "okay://a")     // history, before any stream
      Thread.sleep(50)
      link.open(): Unit
      Thread.sleep(150)
      push(pushes, "okay://a")     // live

      // exactly ONE arrives — the live one; history is for resumers
      assertEquals(Duplex.updatedUri(session.notifications.receiveBlocking().get), Some("okay://a"))
      pushes.listChanged(Mcp.ResourcesChanged)   // a sentinel to bound the wait
      assertEquals(session.notifications.receiveBlocking().map(_.method), Some(Mcp.ResourcesChanged))
    }
  }

  test("two sessions do not bleed on resume: keys filter the replay") {
    val topic = MemoryStore().topic("mcp-pushes", 1)   // one partition: forced sharing
    val (route, pushes) = McpHttp.routed(serving, journal = Some(topic))
    served(route) { url =>
      val la = McpHttp.link(Transports.http(), url)
      val sa = Client.connect(la, Mcp.Info("a", "1")).runWith
      val lb = McpHttp.link(Transports.http(), url)
      val sb = Client.connect(lb, Mcp.Info("b", "1")).runWith
      assert(sa.subscribe("okay://a").runWith)
      assert(sb.subscribe("okay://a").runWith)

      // both streams up, then both drop after one push
      val fa = la.open(); val fb = lb.open()
      Thread.sleep(150)
      push(pushes, "okay://a")
      assertEquals(Duplex.updatedUri(sa.notifications.receiveBlocking().get), Some("okay://a"))
      assertEquals(Duplex.updatedUri(sb.notifications.receiveBlocking().get), Some("okay://a"))
      fa.cancel(); fb.cancel(); Thread.sleep(50)

      push(pushes, "okay://b")     // missed by both; irrelevant to their subscription filter? both subscribed only to a — resourceUpdated(b) reaches nobody
      push(pushes, "okay://a")     // missed by both

      la.open(): Unit
      // a's replay: exactly its own missed push, once
      assertEquals(Duplex.updatedUri(sa.notifications.receiveBlocking().get), Some("okay://a"))
      // and nothing of b's stream leaked into a's channel
      pushes.listChanged(Mcp.ResourcesChanged)
      assertEquals(sa.notifications.receiveBlocking().map(_.method), Some(Mcp.ResourcesChanged))
    }
  }

  test("without a journal the route is v6, unchanged: live-only, no ids") {
    val (route, pushes) = McpHttp.routed(serving)
    served(route) { url =>
      val link = McpHttp.link(Transports.http(), url)
      val session = Client.connect(link, Mcp.Info("t", "1")).runWith
      assert(session.subscribe("okay://a").runWith)
      link.open(): Unit
      Thread.sleep(150)
      push(pushes, "okay://a")
      assertEquals(Duplex.updatedUri(session.notifications.receiveBlocking().get), Some("okay://a"))
      assertEquals(link.lastEventId, None, "a v6 stream carried ids")
    }
  }
}
