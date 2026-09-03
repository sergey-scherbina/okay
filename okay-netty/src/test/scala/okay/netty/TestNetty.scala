package okay.netty

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{Frame, Http, Request, Response, Server as OkayServer, Transports, Ws}

/**
 * Netty behind the same two seams, checked with the same assertions
 * Jetty's suite makes — which is the point of having a seam at all: a
 * program written against `Http` and `Sockets` does not know what is
 * underneath, and this suite is the same program.
 */
class TestNetty extends munit.FunSuite {
  // netty-integration (2026-09-03): real sockets and real ports, so
  // the RESULT depends on things `sbt test` cannot control — this
  // suite failed the default gate twice with the same signature
  // (jetty StaticException: Closed, 2026-09-01 and 2026-09-03) and
  // was green in isolation both times. `sbt integrationTest` runs it.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))


  final case class Person(name: String, age: Int)
  given Schema[Person] = Schema.derived

  /** the echo session — the same one Jetty's suite runs */
  def echo: Stage[Frame, Frame, Unit] =
    val framed: Stage[Frame, Frame, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Text(t) => Stage.tell[Frame, Frame](Frame.Text("echo:" + t))
          case Frame.Binary(b) => Stage.tell[Frame, Frame](Frame.Binary(b))
          case _ => pure(()),
        _ => pure(()))
    framed

  test("Netty's client behind the Http seam: the same program, different engine") {
    val ann = Person("ann", 41)
    val got = Resource.run[Either[String, Person], Pure](
      OkayServer.serve(0)(_ => OkayServer.json(200, ann)).flatMap { server =>
        Netty.http().map { netty =>
          Async.run[Either[String, Person], Pure](
            netty.send(Request.get(
              s"http://127.0.0.1:${OkayServer.port(server)}/p"))
              .flatMap(Http.json[Person])).runWith
        }
      }).runWith
    assertEquals(got, Right(ann))
  }

  test("a Netty server answers the JDK client — routes as a partial function") {
    val got = Resource.run[(Int, String), Pure](
      Netty.serve(0) {
        case r if r.url == "/hello" => OkayServer.text(200, "hello from netty")
      }().map { server =>
        Async.run[(Int, String), Pure](
          Transports.http().send(
            Request.get(s"http://127.0.0.1:${Netty.port(server)}/hello"))
            .flatMap(r => Http.text(r).map(t => (r.status, t)))).runWith
      }).runWith
    assertEquals(got, (200, "hello from netty"))
  }

  test("an unrouted path is a 404, and a route that throws is a 500") {
    val got = Resource.run[(Int, Int, String), Pure](
      Netty.serve(0) {
        case r if r.url == "/boom" => throw RuntimeException("boom")
      }().map { server =>
        val base = s"http://127.0.0.1:${Netty.port(server)}"
        Async.run[(Int, Int, String), Pure](
          Transports.http().send(Request.get(s"$base/nothing")).flatMap { a =>
            Transports.http().send(Request.get(s"$base/boom")).flatMap { b =>
              Http.text(b).map(t => (a.status, b.status, t))
            }
          }).runWith
      }).runWith
    assertEquals(got, (404, 500, "boom"))
  }

  test("a body still streams: content arrives chunk by chunk, no aggregator") {
    val big = "x" * 300_000
    val n = Resource.run[Long, Pure](
      OkayServer.serve(0)(_ => OkayServer.text(200, big)).flatMap { server =>
        Netty.http().map { netty =>
          Async.run[Long, Pure](
            netty.send(Request.get(
              s"http://127.0.0.1:${OkayServer.port(server)}/big")).flatMap { r =>
              given Fold[Chunk[Byte], Long] =
                Fold.long[Chunk[Byte]](0L)((n, c) => n + c.length)
              given scala.reflect.Typeable[Chunk[Byte]] = new:
                def unapply(x: Any): Option[x.type & Chunk[Byte]] = x match
                  case _: scala.collection.immutable.ArraySeq[?] =>
                    Some(x.asInstanceOf[x.type & Chunk[Byte]])
                  case _ => None
              Writer.fold[Chunk[Byte], Long, Unit, Async](r.body).map(_._1)
            }).runWith
        }
      }).runWith
    assertEquals(n, big.length.toLong)
  }

  test("a POST body reaches a Netty route and comes back through the schema") {
    val got = Resource.run[Either[String, Person], Pure](
      Netty.serve(0) { r =>
        Http.json[Person](Response(200, Nil, Http.one(r.body.bytes))).flatMap {
          case Right(p) => OkayServer.json(200, p.copy(age = p.age + 1))
          case Left(e) => OkayServer.text(400, e)
        }
      }().map { server =>
        Async.run[Either[String, Person], Pure](
          Transports.http().send(Request.json(
            s"http://127.0.0.1:${Netty.port(server)}/", Person("bo", 7)))
            .flatMap(Http.json[Person])).runWith
      }).runWith
    assertEquals(got, Right(Person("bo", 8)))
  }

  // ---- WebSocket, both ends

  test("a Netty WebSocket server runs the same Stage a client runs") {
    val got = Resource.run[Seq[Frame], Pure](
      Netty.serve(0)(PartialFunction.empty)({ case _ => echo }).map { server =>
        Async.run[Seq[Frame], Pure](
          Transports.sockets().connect(
            s"ws://127.0.0.1:${Netty.port(server)}/ws").flatMap { sock =>
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

  test("Netty's own WebSocket client talks to Netty's own server") {
    val got = Resource.run[Seq[Frame], Pure](
      Netty.serve(0)(PartialFunction.empty)({ case _ => echo }).flatMap { server =>
        Netty.sockets().map { sockets =>
          Async.run[Seq[Frame], Pure](
            sockets.connect(s"ws://127.0.0.1:${Netty.port(server)}/ws").flatMap { sock =>
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
      Netty.serve(0) { case _ => OkayServer.text(200, "up") }().map(Netty.port)).runWith
    val failed =
      try
        val _ = Async.run[Int, Pure](Transports.http()
          .send(Request.get(s"http://127.0.0.1:$port/")).map(_.status)).runWith
        false
      catch case _: Throwable => true
    assert(failed, "the Netty server outlived its Resource scope")
  }
}
