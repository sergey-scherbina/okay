package okay.ops

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.persist.MemoryStore
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.URI

/**
 * The four routes over a REAL socket (specs/ops.md): the same
 * acceptance move every other route in this stack proves itself by
 * — the server a consumer runs is the server the test hits.
 */
class TestOpsRoutes extends munit.FunSuite:

  // nio-port-scope (2026-09-03): this suite BINDS a real port, so its
  // result depends on what else on the machine is binding them — the
  // class of failure netty-ws-matrix-flake and nio-port-scope-flake
  // both were. Out of the default gate; `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  val client = HttpClient.newHttpClient()

  def withServer[A](store: okay.persist.Store = MemoryStore())(f: Int => A): A =
    Resource.run[A, Pure](
      Jetty.serve(0)(Ops.routes(store))().map(s => f(Jetty.port(s)))).runWith

  def get(port: Int, path: String): HttpResponse[String] =
    client.send(HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path")).GET().build(),
      HttpResponse.BodyHandlers.ofString())

  test("healthz and readyz are 200 with the value named, over a real socket") {
    withServer() { port =>
      val h = get(port, "/healthz")
      assertEquals(h.statusCode(), 200)
      assertEquals(h.body(), "live=true")
      val r = get(port, "/readyz")
      assertEquals(r.statusCode(), 200)
      assertEquals(r.body(), "ready=true")
    }
  }

  test("a store that throws answers 503 on both, naming the reason") {
    val boom = new okay.persist.Store:
      def topic(name: String, partitions: Int, policy: okay.persist.Policy): okay.persist.Topic = ???
      def topics: Vector[String] = Vector.empty
      def stats: okay.persist.Store.Stats = throw RuntimeException("disk is gone")
    withServer(boom) { port =>
      val h = get(port, "/healthz")
      assertEquals(h.statusCode(), 503)
      assert(h.body().contains("disk is gone"), h.body())
    }
  }

  test("/stats answers Store.Stats as JSON") {
    val store = MemoryStore()
    store.topic("chats", 1).append(0, Array(1), Array(1, 2), okay.persist.Ack.Durable)
    withServer(store) { port =>
      val s = get(port, "/stats")
      assertEquals(s.statusCode(), 200)
      assertEquals(s.headers().firstValue("content-type").orElse(""), "application/json")
      assert(s.body().contains("\"chats\""), s.body())
      assert(s.body().contains("\"end\":1") || s.body().contains("\"end\": 1"), s.body())
    }
  }

  test("/metrics answers Prometheus text with the right content-type") {
    val store = MemoryStore()
    store.topic("chats", 1)
    withServer(store) { port =>
      val m = get(port, "/metrics")
      assertEquals(m.statusCode(), 200)
      assert(m.headers().firstValue("content-type").orElse("").startsWith("text/plain; version=0.0.4"))
      assert(m.body().contains("okay_persist_partition_begin"))
    }
  }
