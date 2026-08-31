package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.{Json, Schema}

/**
 * Client and server, end to end, over a pair of Channels: a real
 * handshake, a real listing and a real call, with no process and no
 * socket — because the transport is a seam and a Link is four lines.
 */
class TestSession extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val spec = ToolSpec[Add]("add", "add two numbers")
  val table = Map[String, ToolCall => String]("add" -> { c =>
    ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString)
  })

  /** two Links, wired to each other: the wire, in memory */
  def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  /** a server on its own fiber, a session on ours */
  def connected(serve: Stage[Rpc, Rpc, Unit]): Session =
    val (client, server) = wire()
    Async.spawn(Server.over(server)(serve)): Unit
    Client.connect(client, Mcp.Info("okay-test-client", "1")).runWith

  val serverInfo = Mcp.Info("okay-mcp", "0.1")

  test("the handshake happens, and the server says who it is") {
    val s = connected(Server.serve(serverInfo, Seq(spec), table))
    assertEquals(s.server, Some(serverInfo))
  }

  test("tools/list and tools/call, across a real wire") {
    val s = connected(Server.serve(serverInfo, Seq(spec), table))
    assertEquals(s.tools.runWith.map(_.name), Seq("add"))
    val call = ToolCall("c1", "add", Json.JObj(Vector(
      "a" -> Json.JNum(20), "b" -> Json.JNum(22))))
    assertEquals(s.call(call).runWith, "42")
  }

  test("a tool the server does not have answers, and the session lives on") {
    val s = connected(Server.serve(serverInfo, Seq(spec), table))
    val bad = s.call(ToolCall("c1", "nope", Json.JObj(Vector.empty))).runWith
    assert(bad.startsWith("error:"), bad)
    // the session is not poisoned: the next call still works
    assertEquals(s.call(ToolCall("c2", "add", Json.JObj(Vector(
      "a" -> Json.JNum(1), "b" -> Json.JNum(1))))).runWith, "2")
  }

  test("tools/list is followed across pages") {
    // a server of our own, answering the cursor protocol: the stage
    // vocabulary is public, so a test can be a server too
    val many = (1 to 5).map(i => ToolSpec(s"t$i", s"tool $i", Json.JObj(Vector.empty)))
    val paged: Stage[Rpc, Rpc, Unit] =
      val st: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
        case Rpc.Request(id, Mcp.Initialize, _) =>
          Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(serverInfo)))
        case Rpc.Request(id, Mcp.ToolsList, params) =>
          val page = Rpc.str(params, "cursor").getOrElse("0").toInt
          val slice = many.slice(page * 2, page * 2 + 2)
          val next = if (page + 1) * 2 < many.length then Some((page + 1).toString) else None
          Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.toolsResult(slice, next)))
        case _ => pure(())
      }, pure)
      st

    val s = connected(paged)
    assertEquals(s.tools.runWith.map(_.name), many.map(_.name))
  }

  test("a server that dies mid-call answers error, and does not take us with it") {
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)

    // a server that answers the handshake and then simply stops
    val quits: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(serverInfo)))
      case _ => pure(())
    }, pure)

    val fiber = Async.spawn(Server.over(link(down, up))(quits))
    val s = Client.connect(link(up, down), Mcp.Info("c", "1")).runWith

    // the server's side of the wire ends, which is what a process
    // exiting looks like from here — with a call in flight
    Async.spawn(async { Thread.sleep(50); down.close() }): Unit
    val answer = s.call(ToolCall("c1", "add", Json.JObj(Vector.empty))).runWith
    assert(answer.startsWith("error:"), answer)
    fiber.cancel()
  }
}
