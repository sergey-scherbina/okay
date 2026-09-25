package okay2.http

import java.util.concurrent.atomic.AtomicReference
import okay2.{!, Pure, Resource, Writer, pure}
import okay2.async.Async
import okay2.platform._

/** the transports for real: a server and a client in one process
 * speaking the same two types (okay-http's TestHttp) */
class TestHttp extends Live {

  val client = Transports.http()

  def run[A](p: A ! Async): A = !.run(Async.run[A, Pure](p))

  /** run `use` while a server answers with `route` */
  def serving[A](route: Request => Response ! Async)(use: Int => A): A =
    !.run(Resource.run[A, Pure](Server.serve(0)(route).map(s => use(Server.port(s)))))

  def get[A](port: Int, path: String)(f: Response => A ! Async): A =
    run(client.send(Request.get(s"http://127.0.0.1:$port$path")).flatMap(f))

  test("the server fills the peer's host") {
    val seen = new AtomicReference[Option[String]](None)
    serving { r => seen.set(r.peer); pure[Async, Response](Response(200, Nil, Http.one("ok".getBytes("UTF-8")))) } { port =>
      get(port, "/x")(r => pure[Async, Int](r.status))
    }: Unit
    assertEquals(seen.get(), Some("127.0.0.1"))
  }

  test("a route answers, and the client reads status, headers and body") {
    serving { r =>
      if (Server.path(r) == "/hello") Server.text(200, "hello " + r.url.dropWhile(_ != '?').drop(1))
      else Server.notFound
    } { port =>
      assertEquals(get(port, "/hello?ann")(r => pure[Async, Int](r.status)), 200)
      assertEquals(get(port, "/hello?ann")(Http.text), "hello ann")
      assertEquals(get(port, "/hello?x")(r => pure[Async, Option[String]](r.header("content-type"))), Some("text/plain; charset=utf-8"))
    }
  }

  test("a 404 is a Response, not a failure: status is data") {
    serving(_ => Server.notFound) { port =>
      assertEquals(get(port, "/nothing")(r => pure[Async, (Int, Boolean)]((r.status, r.ok))), (404, false))
      assertEquals(get(port, "/nothing")(Http.text), "not found")
    }
  }

  test("a value round-trips through the schema, over the wire") {
    val ann = Person("ann", 41)
    serving(_ => Server.json(200, ann)) { port =>
      assertEquals(get(port, "/p")(r => Http.json[Person](r)), Right(ann))
    }
  }

  test("a POST body arrives, decoded by the same schema that sent it") {
    serving { r =>
      Http.json[Person](Response(200, Nil, Http.one(r.body.bytes))).flatMap {
        case Right(p) => Server.json(200, p.copy(age = p.age + 1))
        case Left(e) => Server.text(400, e)
      }
    } { port =>
      assertEquals(run(client.send(Request.json(s"http://127.0.0.1:$port/", Person("bo", 7))).flatMap(r => Http.json[Person](r))),
        Right(Person("bo", 8)))
    }
  }

  test("a body STREAMS: it is folded chunk by chunk, never materialized") {
    val big = "x" * 400000
    serving(_ => Server.text(200, big)) { port =>
      val n = run(client.send(Request.get(s"http://127.0.0.1:$port/big")).flatMap { r =>
        Writer.loopWith[okay2.stream.Chunk[Byte], Long, Unit, Long, Async](r.body)(0L)((n, c) => n + c.length)((n, _) => n)
      })
      assertEquals(n, big.length.toLong)
    }
  }

  test("lines stream out of a body, and SSE events out of those") {
    val body = "data: one\n\ndata: two\n\nignored\n\n"
    serving(_ => Server.text(200, body)) { port =>
      val ls = run(client.send(Request.get(s"http://127.0.0.1:$port/s")).flatMap(r => Writer.collect[String, Unit, Async](Http.lines(r)).map(_._1)))
      assertEquals(ls, Vector("data: one", "", "data: two", "", "ignored", ""))
      val es = run(client.send(Request.get(s"http://127.0.0.1:$port/s")).flatMap(r => Writer.collect[String, Unit, Async](Http.sse(r)).map(_._1)))
      assertEquals(es, Vector("one", "two"))
    }
  }

  test("a route that throws is a 500 with its message: damage as data") {
    serving(_ => throw new RuntimeException("boom")) { port =>
      assertEquals(get(port, "/x")(r => pure[Async, Int](r.status)), 500)
      assertEquals(get(port, "/x")(Http.text), "boom")
    }
  }

  test("every verb reaches the route with its own name") {
    serving(r => Server.text(200, r.method.name)) { port =>
      for (m <- Seq(Method.Get, Method.Put, Method.Delete, Method.Patch))
        assertEquals(run(client.send(Request(m, s"http://127.0.0.1:$port/v")).flatMap(Http.text)), m.name)
    }
  }

  test("a body can be let go UNREAD: discard, not drain") {
    serving(_ => Server.text(200, "x" * 200000)) { port =>
      for (_ <- 1 to 20) assertEquals(get(port, "/big")(r => Http.discard(r).map(_ => r.status)), 200)
      assertEquals(get(port, "/big")(Http.text).length, 200000)
    }
  }

  test("Resource stops the server: the port is refused after the scope") {
    val port = serving(_ => Server.text(200, "up"))(p => p)
    val failed =
      try { run(client.send(Request.get(s"http://127.0.0.1:$port/")).map(_.status)): Unit; false }
      catch { case _: Throwable => true }
    assert(failed, "the server outlived its Resource scope")
  }
}
