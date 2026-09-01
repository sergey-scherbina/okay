package okay.http

import okay.*
import okay.given
import okay.codec.{Json, Schema}

/**
 * The transports, for real: a server and a client in one process,
 * speaking the same two types.
 *
 * That is the point of the shape — `Request => Response ! Async` is
 * what a route is AND what a client sends, so this suite writes a
 * handler once and calls it over a socket, with no mock in between.
 */
class TestHttp extends munit.FunSuite {

  final case class Person(name: String, age: Int)
  given Schema[Person] = Schema.derived

  val client = Transports.http()

  /** run a program that needs a running server */
  def serving[A](route: Request => Response ! Async)(use: Int => A): A =
    Resource.run[A, Pure](
      Server.serve(0)(route).map(s => use(Server.port(s)))).runWith

  def get[A](port: Int, path: String)(f: Response => A ! Async): A =
    Async.run[A, Pure](client.send(Request.get(s"http://127.0.0.1:$port$path"))
      .flatMap(f)).runWith

  test("a route answers, and the client reads status, headers and body") {
    serving {
      case r if Server.path(r) == "/hello" =>
        Server.text(200, "hello " + r.url.dropWhile(_ != '?').drop(1))
      case _ => Server.notFound
    } { port =>
      assertEquals(get(port, "/hello?ann")(r => pure(r.status)), 200)
      assertEquals(get(port, "/hello?ann")(Http.text), "hello ann")
      assertEquals(
        get(port, "/hello?x")(r => pure(r.header("content-type"))),
        Some("text/plain; charset=utf-8"))
    }
  }

  test("a 404 is a Response, not a failure — status is data") {
    serving(_ => Server.notFound) { port =>
      val r = get(port, "/nothing")(r => pure((r.status, r.ok)))
      assertEquals(r, (404, false))
      assertEquals(get(port, "/nothing")(Http.text), "not found")
    }
  }

  test("a value round-trips through the schema, over the wire") {
    val ann = Person("ann", 41)
    serving(_ => Server.json(200, ann)) { port =>
      assertEquals(get(port, "/p")(Http.json[Person]), Right(ann))
    }
  }

  test("a POST body arrives, decoded by the same schema that sent it") {
    serving { r =>
      Http.json[Person](Response(200, Nil, Http.one(r.body.bytes))).flatMap {
        case Right(p) => Server.json(200, p.copy(age = p.age + 1))
        case Left(e) => Server.text(400, e)
      }
    } { port =>
      val got = Async.run[Either[String, Person], Pure](
        client.send(Request.json(s"http://127.0.0.1:$port/", Person("bo", 7)))
          .flatMap(Http.json[Person])).runWith
      assertEquals(got, Right(Person("bo", 8)))
    }
  }

  test("a body STREAMS: it is folded chunk by chunk, never materialized") {
    // 400KB in, counted through the source without a single full copy
    val big = "x" * 400_000
    serving(_ => Server.text(200, big)) { port =>
      val n = Async.run[Long, Pure](
        client.send(Request.get(s"http://127.0.0.1:$port/big")).flatMap { r =>
          given Fold[Chunk[Byte], Long] = Fold.long[Chunk[Byte]](0L)((n, c) => n + c.length)
          given scala.reflect.Typeable[Chunk[Byte]] = new:
            def unapply(x: Any): Option[x.type & Chunk[Byte]] = x match
              case _: scala.collection.immutable.ArraySeq[?] =>
                Some(x.asInstanceOf[x.type & Chunk[Byte]])
              case _ => None
          Writer.fold[Chunk[Byte], Long, Unit, Async](r.body).map(_._1)
        }).runWith
      assertEquals(n, big.length.toLong)
    }
  }

  test("lines stream out of a body, and SSE events out of those") {
    val body = "data: one\n\ndata: two\n\nignored\n\n"
    serving(_ => Server.text(200, body)) { port =>
      val ls = Async.run[Seq[String], Pure](
        client.send(Request.get(s"http://127.0.0.1:$port/s"))
          .flatMap(r => Writer.run[String, Unit, Async](Http.lines(r)).map(_._1))).runWith
      assertEquals(ls, Seq("data: one", "", "data: two", "", "ignored", ""))

      val es = Async.run[Seq[String], Pure](
        client.send(Request.get(s"http://127.0.0.1:$port/s"))
          .flatMap(r => Writer.run[String, Unit, Async](Http.sse(r)).map(_._1))).runWith
      assertEquals(es, Seq("one", "two"))
    }
  }

  test("a route that throws is a 500 with its message — damage as data") {
    serving(_ => throw RuntimeException("boom")) { port =>
      assertEquals(get(port, "/x")(r => pure(r.status)), 500)
      assertEquals(get(port, "/x")(Http.text), "boom")
    }
  }

  test("every verb reaches the route with its own name") {
    serving(r => Server.text(200, r.method.name)) { port =>
      for m <- Seq(Method.Get, Method.Put, Method.Delete, Method.Patch) do
        val got = Async.run[String, Pure](
          client.send(Request(m, s"http://127.0.0.1:$port/v"))
            .flatMap(Http.text)).runWith
        assertEquals(got, m.name)
    }
  }

  test("retry from P2 applies as-is — nothing resilience-shaped is re-invented") {
    var attempts = 0
    val flaky = new Http:
      def send(r: Request): Response ! Async = async {
        attempts += 1
        if attempts == 1 then throw RuntimeException("wire down")
        Response(200, Nil, Http.one("second time".getBytes("UTF-8")))
      }

    val got = okay.retry(okay.Retry.immediate(2))(
      okay.async(Async.run[String, Pure](
        flaky.send(Request.get("http://x/")).flatMap(Http.text)).runWith)).runWith
    assertEquals(got, "second time")
    assertEquals(attempts, 2)   // one failure, one clean pass
  }

  test("a body can be let go UNREAD — discard, not drain") {
    // the JDK documents that an unconsumed ofInputStream body can keep
    // an HttpClient from shutting down, and draining a large body only
    // to throw it away is the wrong fix. `release` closes it.
    //
    // Twenty of them, unread, and then an ordinary request: if
    // abandoning wedged anything, the last one would not answer.
    serving(_ => Server.text(200, "x" * 200_000)) { port =>
      for _ <- 1 to 20 do
        val status = get(port, "/big")(r => Http.discard(r).map(_ => r.status))
        assertEquals(status, 200)

      assertEquals(get(port, "/big")(Http.text).length, 200_000)
    }
  }

  test("Resource stops the server: the port is free again after the scope") {
    val port = serving(_ => Server.text(200, "up"))(p => p)
    // the scope ended, so a connect must now fail rather than answer
    val failed =
      try
        Async.run[Int, Pure](
          client.send(Request.get(s"http://127.0.0.1:$port/"))
            .map(_.status)).runWith
        false
      catch case _: Throwable => true
    assert(failed, "the server outlived its Resource scope")
  }
}
