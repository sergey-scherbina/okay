package okay.jetty

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{Frame, Http, Request, Response, Server as OkayServer, Transports, Ws}

/**
 * Jetty behind the same two seams — and the gap closed.
 *
 * The last test is why this module exists: `specs/http.md` put serving
 * WebSocket out of scope because the JDK cannot, and here a server runs
 * the SAME `Stage[Frame, Frame, Unit]` a client runs, with the JDK's
 * own client on the other end. One session type, both ends, two
 * different implementations underneath.
 */
class TestJetty extends munit.FunSuite {
  // nio-port-scope (2026-09-03): this suite BINDS a real port, so its
  // result depends on what else on the machine is binding them — the
  // class of failure netty-ws-matrix-flake and nio-port-scope-flake
  // both were. Out of the default gate; `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  final case class Person(name: String, age: Int)
  given Schema[Person] = Schema.derived

  /** the echo session — written once, run on either end */
  def echo: Stage[Frame, Frame, Unit] =
    val framed: Stage[Frame, Frame, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text("echo:" + t))
          case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
          case _ => pure(()),
        _ => pure(()))
    framed

  test("Jetty's client behind the Http seam: the same program, different engine") {
    val ann = Person("ann", 41)
    val got = Resource.run[Either[String, Person], Pure](
      OkayServer.serve(0)(_ => OkayServer.json(200, ann)).flatMap { server =>
        Jetty.http().map { jetty =>
          Async.run[Either[String, Person], Pure](
            jetty.send(Request.get(
              s"http://127.0.0.1:${OkayServer.port(server)}/p"))
              .flatMap(Http.json[Person])).runWith
        }
      }).runWith
    assertEquals(got, Right(ann))
  }

  test("a Jetty server answers the JDK client — routes as a partial function") {
    val got = Resource.run[(Int, String), Pure](
      Jetty.serve(0) {
        case r if r.url == "/hello" => OkayServer.text(200, "hello from jetty")
      }().map { server =>
        val c = Transports.http()
        Async.run[(Int, String), Pure](
          c.send(Request.get(s"http://127.0.0.1:${Jetty.port(server)}/hello"))
            .flatMap(r => Http.text(r).map(t => (r.status, t)))).runWith
      }).runWith
    assertEquals(got, (200, "hello from jetty"))
  }

  test("a request's query string reaches the route, not just the path (http-request-query)") {
    val got = Resource.run[(Int, String), Pure](
      Jetty.serve(0) {
        case r if r.url.startsWith("/search") => OkayServer.text(200, r.url)
      }().map { server =>
        val c = Transports.http()
        Async.run[(Int, String), Pure](
          c.send(Request.get(s"http://127.0.0.1:${Jetty.port(server)}/search?q=okay&limit=5"))
            .flatMap(r => Http.text(r).map(t => (r.status, t)))).runWith
      }).runWith
    assertEquals(got, (200, "/search?q=okay&limit=5"))
  }

  test("a route that throws is a 500 with its message, as on the built-in server") {
    val got = Resource.run[(Int, String), Pure](
      Jetty.serve(0) {
        case _ => throw RuntimeException("boom")
      }().map { server =>
        val c = Transports.http()
        Async.run[(Int, String), Pure](
          c.send(Request.get(s"http://127.0.0.1:${Jetty.port(server)}/x"))
            .flatMap(r => Http.text(r).map(t => (r.status, t)))).runWith
      }).runWith
    assertEquals(got, (500, "boom"))
  }

  test("a body still streams: a large response is folded chunk by chunk") {
    val big = "x" * 300_000
    val n = Resource.run[Long, Pure](
      Jetty.serve(0) { case _ => OkayServer.text(200, big) }().map { server =>
        Async.run[Long, Pure](
          Transports.http().send(Request.get(
            s"http://127.0.0.1:${Jetty.port(server)}/big")).flatMap { r =>
            given Fold[Chunk[Byte], Long] =
              Fold.long[Chunk[Byte]](0L)((n, c) => n + c.length)
            given scala.reflect.Typeable[Chunk[Byte]] = new:
              def unapply(x: Any): Option[x.type & Chunk[Byte]] = x match
                case _: scala.collection.immutable.ArraySeq[?] =>
                  Some(x.asInstanceOf[x.type & Chunk[Byte]])
                case _ => None
            Writer.fold[Chunk[Byte], Long, Unit, Async](r.body).map(_._1)
          }).runWith
      }).runWith
    assertEquals(n, big.length.toLong)
  }

  // ---- the gap okay-http could not close

  test("a WebSocket SERVER runs the same Stage a client runs") {
    val got = Resource.run[Seq[Frame], Pure](
      Jetty.serve(0)(PartialFunction.empty)({ case _ => echo }).map { server =>
        val sockets = Transports.sockets()
        Async.run[Seq[Frame], Pure](
          sockets.connect(s"ws://127.0.0.1:${Jetty.port(server)}/ws").flatMap { sock =>
            // the client side of the very same session type
            val say: Stage[Frame, Frame, Seq[Frame]] =
              Stage.tell[Frame, Frame](Frame.Text("ping")).flatMap(_ =>
                Stage.await[Frame, Frame].map {
                  case Some(f) => Seq(f)
                  case None => Seq.empty
                })
            Ws.over(sock)(say).flatMap(fs => sock.close().map(_ => fs))
          }).runWith
      }).runWith
    assertEquals(got, Seq(Frame.Text("echo:ping")))
  }

  test("Jetty's own WebSocket client talks to Jetty's own server") {
    val got = Resource.run[Seq[Frame], Pure](
      Jetty.serve(0)(PartialFunction.empty)({ case _ => echo }).flatMap { server =>
        Jetty.sockets().map { sockets =>
          Async.run[Seq[Frame], Pure](
            sockets.connect(s"ws://127.0.0.1:${Jetty.port(server)}/ws").flatMap { sock =>
              val say: Stage[Frame, Frame, Seq[Frame]] =
                Stage.tell[Frame, Frame](Frame.Text("both ends")).flatMap(_ =>
                  Stage.await[Frame, Frame].map {
                    case Some(f) => Seq(f)
                    case None => Seq.empty
                  })
              Ws.over(sock)(say).flatMap(fs => sock.close().map(_ => fs))
            }).runWith
        }
      }).runWith
    assertEquals(got, Seq(Frame.Text("echo:both ends")))
  }

  test("everything is a Resource: after the scope the port is free") {
    val port = Resource.run[Int, Pure](
      Jetty.serve(0) { case _ => OkayServer.text(200, "up") }().map(Jetty.port)).runWith
    val failed =
      try
        val _ = Async.run[Int, Pure](Transports.http()
          .send(Request.get(s"http://127.0.0.1:$port/")).map(_.status)).runWith
        false
      catch case _: Throwable => true
    assert(failed, "the Jetty server outlived its Resource scope")
  }
}
