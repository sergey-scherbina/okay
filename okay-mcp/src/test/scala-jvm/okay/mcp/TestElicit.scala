package okay.mcp

import okay.*
import okay.given
import okay.codec.Json

/** Elicitation: the server asks the human; the client's Peer answers. */
class TestElicit extends munit.FunSuite {

  val info = Mcp.Info("okay-mcp", "0.1")

  def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  val schema = Json.parse("""{"type":"object","properties":{"path":{"type":"string"}}}""")

  /** a server stage that asks, and reports what came back */
  def asking(answers: Channel[Duplex.Answer]): Stage[Rpc, Rpc, Unit] =
    Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
          .flatMap(_ => Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("e1"),
            Mcp.ElicitationCreate, Duplex.elicitParams("which file?", schema))))
      case Rpc.Answer(Json.JStr("e1"), result) =>
        pure(answers.offer(Duplex.answerOf(result)): Unit)
      case Rpc.Failed(Json.JStr("e1"), _, _) =>
        pure(answers.offer(Duplex.Answer.Cancel): Unit)
      case _ => pure(())
    }, pure)

  test("accept carries the content, typed by the requested schema") {
    val got = Channel[Duplex.Answer]()
    val (client, server) = wire()
    Async.spawn(Server.over(server)(asking(got))): Unit
    Client.connect(client, Mcp.Info("c", "1"), Duplex.Peer(
      elicit = Some((msg, sch) => {
        assertEquals(msg, "which file?")
        Duplex.Answer.Accept(Json.parse("""{"path":"/tmp"}"""))
      }))).runWith
    assertEquals(got.receiveBlocking(),
      Some(Duplex.Answer.Accept(Json.JObj(Vector("path" -> Json.JStr("/tmp"))))))
  }

  test("decline is an answer too, and a peer with no handler refuses") {
    val got = Channel[Duplex.Answer]()
    val (client, server) = wire()
    Async.spawn(Server.over(server)(asking(got))): Unit
    Client.connect(client, Mcp.Info("c", "1"), Duplex.Peer(
      elicit = Some((_, _) => Duplex.Answer.Decline))).runWith
    assertEquals(got.receiveBlocking(), Some(Duplex.Answer.Decline))

    val got2 = Channel[Duplex.Answer]()
    val (client2, server2) = wire()
    Async.spawn(Server.over(server2)(asking(got2))): Unit
    Client.connect(client2, Mcp.Info("c", "1")).runWith   // no handler
    assertEquals(got2.receiveBlocking(), Some(Duplex.Answer.Cancel))  // the refusal arrived
  }

  test("the capability is declared exactly when the handler exists") {
    assert(Mcp.capability(
      Mcp.initializeParams(Mcp.Info("c", "1"), elicitation = true), "elicitation"))
    assert(!Mcp.capability(Mcp.initializeParams(Mcp.Info("c", "1")), "elicitation"))
  }
}
