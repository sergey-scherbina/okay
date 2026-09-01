package okay.demo

import okay.*
import okay.given
import okay.codec.Json
import okay.mcp.{Client, Duplex, Link, Mcp, Rpc, Server}
import okay.ui.{Event, Form, Host, Ui}

/**
 * The circle this module exists to close: an MCP server asks the
 * HUMAN (elicitation), the human is a FORM rendered from the
 * requested JSON Schema by okay-ui, the filled value goes back typed
 * — and every part is the library's own: the form from the Schema
 * algebra, the loop from Stage.transduce, the host a scripted value,
 * the wire a pair of channels.
 */
class TestElicitForm extends munit.FunSuite {

  /** a host that "is" a user: renders to values, plays a script */
  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  /** elicitation answered by a form session over a host */
  def formElicit(host: Seq[Event] => Host): (String, Json) => Duplex.Answer =
    (message, schema) => {
      // the form loop: state is the partial value; ok submits
      def view(j: Json): Ui = Ui.Column(Vector(
        Ui.Text(message),
        Form.ofSchema(schema)(j),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))
      // the script plays the user; ok/cancel end the loop by Closed
      var submitted = false
      def update(j: Json, e: Event): Json = e match
        case Event.Pressed("$ok") => submitted = true; j
        case _ => Form.editSchema(schema, j, e)
      val h = host(Nil)
      val value = Ui.run(Json.JObj(Vector.empty): Json)(view)(update)(h).runWith
      if submitted then Duplex.Answer.Accept(value) else Duplex.Answer.Decline
    }

  val schema = Json.parse(
    """{"type":"object","properties":{
         "path":{"type":"string"},
         "recursive":{"type":"boolean"}}}""")

  def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  test("a server's question becomes a form; the filled form goes back typed") {
    val answers = Channel[Duplex.Answer]()
    val serverInfo = Mcp.Info("asker", "1")
    val asking: okay.Stage[Rpc, Rpc, Unit] = okay.Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        okay.Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(serverInfo)))
          .flatMap(_ => okay.Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("e1"),
            Mcp.ElicitationCreate, Duplex.elicitParams("index what?", schema))))
      case Rpc.Answer(Json.JStr("e1"), result) => pure(answers.send(Duplex.answerOf(result)))
      case _ => pure(())
    }, pure)

    // the user, as a script: fill the path, tick the box, press ok
    val user = Seq(
      Event.Edited("path", "/work/okay"),
      Event.Toggled("recursive", true),
      Event.Pressed("$ok"),
      Event.Closed)

    val (client, server) = wire()
    Async.spawn(Server.over(server)(asking)): Unit
    Client.connect(client, Mcp.Info("ui", "1"), Duplex.Peer(
      elicit = Some(formElicit(_ => Scripted(user))))).runWith

    assertEquals(answers.receive(), Some(Duplex.Answer.Accept(Json.JObj(Vector(
      "path" -> Json.JStr("/work/okay"),
      "recursive" -> Json.JBool(true))))))
  }

  test("the human declines: the server hears that, not a hang") {
    val answers = Channel[Duplex.Answer]()
    val serverInfo = Mcp.Info("asker", "1")
    val asking: okay.Stage[Rpc, Rpc, Unit] = okay.Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        okay.Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(serverInfo)))
          .flatMap(_ => okay.Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("e1"),
            Mcp.ElicitationCreate, Duplex.elicitParams("sure?", schema))))
      case Rpc.Answer(Json.JStr("e1"), result) => pure(answers.send(Duplex.answerOf(result)))
      case _ => pure(())
    }, pure)

    val user = Seq(Event.Pressed("$cancel"), Event.Closed)
    val (client, server) = wire()
    Async.spawn(Server.over(server)(asking)): Unit
    Client.connect(client, Mcp.Info("ui", "1"), Duplex.Peer(
      elicit = Some(formElicit(_ => Scripted(user))))).runWith
    assertEquals(answers.receive(), Some(Duplex.Answer.Decline))
  }
}
