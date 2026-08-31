package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.{Json, Schema}

/**
 * The server is a PURE stage, and this suite is the proof: the whole
 * protocol — handshake, listing, calling, errors — with no process,
 * no socket, no clock and no thread.
 */
class TestServer extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val spec = ToolSpec[Add]("add", "add two numbers")
  val table = Map[String, ToolCall => String]("add" -> { c =>
    ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString)
  })
  val info = Mcp.Info("okay-test", "0.1")

  /** drive the stage with a list of messages, collect what it tells */
  def talk(msgs: Rpc*): Seq[Rpc] =
    !.run(Writer.run(through(Writer.of(msgs.toList))(Server.serve(info, Seq(spec), table))))._1

  val hello = Rpc.Request(Json.JNum(1), Mcp.Initialize,
    Mcp.initializeParams(Mcp.Info("client", "1")))

  test("initialize answers with the version and the tools capability") {
    val Seq(Rpc.Answer(id, result)) = talk(hello): @unchecked
    assertEquals(id, Json.JNum(1))
    assertEquals(Rpc.str(result, "protocolVersion"), Some(Mcp.Version))
    assertEquals(Rpc.field(result, "capabilities").flatMap(Rpc.field(_, "tools")).isDefined, true)
    assertEquals(Rpc.field(result, "serverInfo").flatMap(Mcp.infoOf), Some(info))
  }

  test("tools/list publishes the DERIVED schema, not a hand-written one") {
    val out = talk(hello, Rpc.Request(Json.JNum(2), Mcp.ToolsList, Json.JObj(Vector.empty)))
    val Rpc.Answer(_, result) = out(1): @unchecked
    val (tools, cursor) = Mcp.toolsOf(result)
    assertEquals(cursor, None)
    assertEquals(tools.map(_.name), Seq("add"))
    // the schema on the wire IS ToolSpec.jsonSchema of the case class
    assertEquals(tools.head.schema, ToolSpec.jsonSchema(summon[Schema[Add]]))
    assertEquals(tools.head.description, "add two numbers")
  }

  test("tools/call runs the tool and answers content blocks") {
    val call = Rpc.Request(Json.JNum(3), Mcp.ToolsCall, Mcp.callParams(
      ToolCall("x", "add", Json.JObj(Vector("a" -> Json.JNum(2), "b" -> Json.JNum(3))))))
    val Rpc.Answer(_, result) = talk(hello, call)(1): @unchecked
    assertEquals(Mcp.textOf(result), "5")
    assertEquals(Rpc.field(result, "isError"), Some(Json.JBool(false)))
  }

  test("an unknown tool is an ANSWER with isError, not a protocol failure") {
    val call = Rpc.Request(Json.JNum(4), Mcp.ToolsCall, Mcp.callParams(
      ToolCall("x", "nope", Json.JObj(Vector.empty))))
    val out = talk(hello, call)(1)
    val Rpc.Answer(_, result) = out: @unchecked   // an Answer, note, not a Failed
    assertEquals(Rpc.field(result, "isError"), Some(Json.JBool(true)))
    assert(Mcp.textOf(result).startsWith("error: no such tool"), Mcp.textOf(result))
  }

  test("a throwing tool is the same: the model can read it and retry") {
    val bad = Map[String, ToolCall => String]("boom" -> (_ => throw RuntimeException("kaboom")))
    val out = !.run(Writer.run(through(Writer.of(List(hello,
      Rpc.Request(Json.JNum(5), Mcp.ToolsCall,
        Mcp.callParams(ToolCall("x", "boom", Json.JObj(Vector.empty)))))))(
      Server.serve(info, Nil, bad))))._1
    val Rpc.Answer(_, result) = out(1): @unchecked
    assertEquals(Mcp.textOf(result), "error: kaboom")
  }

  test("an unknown METHOD is a protocol failure, which is different") {
    val out = talk(hello, Rpc.Request(Json.JNum(6), "resources/list", Json.JObj(Vector.empty)))
    assertEquals(out(1), Rpc.Failed(Json.JNum(6), Rpc.MethodNotFound, "resources/list"))
  }

  test("requests before initialize are refused, as the protocol says") {
    val out = talk(Rpc.Request(Json.JNum(7), Mcp.ToolsList, Json.JObj(Vector.empty)))
    val Rpc.Failed(_, code, _) = out.head: @unchecked
    assertEquals(code, Rpc.InvalidRequest)
    // ping and initialize are the two that do not need it
    assertEquals(talk(Rpc.Request(Json.JNum(8), Mcp.Ping, Json.JObj(Vector.empty))).length, 1)
  }

  test("notifications are never answered; damage is answered exactly once") {
    assertEquals(talk(hello, Rpc.Notify(Mcp.Initialized, Json.JObj(Vector.empty))).length, 1)
    val damaged = Rpc.decode("{oops")
    val out = talk(hello, damaged)
    assertEquals(out(1), damaged)   // the parse error goes back as the protocol owes it
  }
}
