package okay.graphql.http

import okay.{Async, Stage, StreamCont}
import okay.freer.*
import okay.std.*
import okay.given
import okay.freer.given
import okay.codec.Json
import okay.graphql.*
import okay.http.{Body, Frame, Http, Method, Request, Ws}
import okay.jetty.Jetty

/** the whole road on a real server: HTTP and graphql-transport-ws on one Jetty, Jetty's own clients. Binds a
 * port: Live */
class TestGraphQLJetty extends okay.testkit.Munit.Diagnosed:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val schema = Schema(ObjectType[Unit]("Query")(List(Field("hello", Scalar.string)(_ => "world"))),
    subscription = Some(SubscriptionType("Subscription")(List(
      Subscription("count", Scalar.int, Arg("to", InScalar.int))(a => StreamCont.fromList((1 to a(Arg("to", InScalar.int))).toList))))))

  test("POST /graphql and a subscription over the WebSocket, the subprotocol negotiated") {
    val http = GraphQLHttp.router(schema)
    val ws = GraphQLWs.route(schema, "/graphql")
    val (answer, frames) = Resource.run[(String, Seq[Frame]), Pure](
      Jetty.serve(0)(http.routes)(ws.sessions, ws.pushes, subprotocol = ws.subprotocol).flatMap { server =>
        val base = s"127.0.0.1:${Jetty.port(server)}/graphql"
        Jetty.http().flatMap { client =>
          Jetty.sockets().map { sockets =>
            val answer = Async.run[String, Pure](client.send(Request(Method.Post, s"http://$base",
              Seq("content-type" -> "application/json"), Body.Text("""{"query":"{ hello }"}""")))
              .flatMap(Http.text)).runWith
            def next(n: Int, acc: Seq[Frame]): Stage[Frame, Frame, Seq[Frame]] =
              if n == 0 then pure(acc)
              else Stage.await[Frame, Frame].flatMap {
                case Some(f) => next(n - 1, acc :+ f)
                case None => pure(acc)
              }
            val session: Stage[Frame, Frame, Seq[Frame]] =
              Stage.tell[Frame, Frame](Frame.Text("""{"type":"connection_init"}""")).flatMap(_ =>
                next(1, Nil).flatMap(ack =>
                  Stage.tell[Frame, Frame](Frame.Text(
                    """{"id":"c","type":"subscribe","payload":{"query":"subscription { count(to: 3) }"}}""")).flatMap(_ =>
                    next(4, ack))))
            val frames = Async.run[Seq[Frame], Pure](
              sockets.connect(s"ws://$base", Nil, Seq("graphql-transport-ws")).flatMap(sock =>
                Ws.over(sock)(session).flatMap(fs => sock.close().map(_ => fs)))).runWith
            (answer, frames)
          }
        }
      }).runWith
    note(s"$answer $frames")
    assertEquals(Json.parse(answer), Json.parse("""{"data":{"hello":"world"}}"""))
    assertEquals(frames.toList, List(
      Frame.Text("""{"type":"connection_ack"}"""),
      Frame.Text("""{"id":"c","type":"next","payload":{"data":{"count":1}}}"""),
      Frame.Text("""{"id":"c","type":"next","payload":{"data":{"count":2}}}"""),
      Frame.Text("""{"id":"c","type":"next","payload":{"data":{"count":3}}}"""),
      Frame.Text("""{"type":"complete","id":"c"}""")))
  }
