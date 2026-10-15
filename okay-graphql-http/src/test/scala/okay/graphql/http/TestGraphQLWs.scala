package okay.graphql.http

import okay.{Stage, through}
import okay.freer.*
import okay.std.*
import okay.freer.given
import okay.std.given
import okay.graphql.*
import okay.http.{Frame, Request}

class TestGraphQLWs extends okay.testkit.Munit.Diagnosed:
  private val schema = Schema(ObjectType[Unit]("Query")(List(Field("hello", Scalar.string)(_ => "world"))))

  /** frames into a session, the frames it tells: the pure part, no socket */
  private def talk(session: Stage[Frame, Frame, Unit])(fs: Frame*): Seq[Frame] =
    !.run(Writer.run(through(Writer.of(fs.toList))(session)))._1

  test("the route: its path's sessions, the subprotocol, nothing elsewhere") {
    val ws = GraphQLWs.route(schema, "/graphql")
    assert(ws.sessions.isDefinedAt(Request.get("/graphql")))
    assert(!ws.sessions.isDefinedAt(Request.get("/other")))
    assertEquals(ws.subprotocol(Request.get("/graphql?x=1")), Some("graphql-transport-ws"))
    assertEquals(ws.subprotocol(Request.get("/other")), None)
  }

  test("a session: the client's frames to the connection, its pushes told, a protocol close a Close frame") {
    val s = GraphQLWs.Session(schema, Limits(), _ => true)
    val told = talk(s.stage)(Frame.Text("""{"type":"connection_init"}"""),
      Frame.Text("\u0001" + """{"type":"connection_ack"}"""),
      Frame.Text("""{"id":"1","type":"subscribe","payload":{"query":"{ hello }"}}"""),
      Frame.Text("\u0002" + "4400:Invalid message"))
    note(told.toString)
    assertEquals(told.toList, List(Frame.Text("""{"type":"connection_ack"}"""), Frame.Close(4400, "Invalid message")))
    assertEquals(s.connection.running, Set.empty[String])
  }
