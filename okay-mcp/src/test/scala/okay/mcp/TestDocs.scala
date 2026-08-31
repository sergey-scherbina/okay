package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec, Turn}
import okay.codec.{Json, Schema}

/**
 * Resources and prompts: the other two capabilities, each landing on
 * a type this library already had — a resource is a document, a
 * prompt is a conversation opening.
 */
class TestDocs extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-mcp", "0.1")
  val spec = ToolSpec[Add]("add", "add two numbers")

  val files = Map(
    "okay://a.txt" -> "alpha",
    "okay://b.txt" -> "beta")

  val serving = Server.Serving(info,
    tools = Seq(spec),
    call = Map("add" -> (_ => "3")),
    resources = files.keys.toSeq.sorted.map(u => Mcp.Resource(u, u.split('/').last)),
    read = files.get,
    prompts = Seq(Mcp.Prompt("review", "review a file",
      Seq(Mcp.Prompt.Arg("path", "which file", required = true)))),
    prompt = (n, args) =>
      Option.when(n == "review")(Seq(
        Turn.System("You review code."),
        Turn.User(s"Review ${args.getOrElse("path", "?")}"))))

  def talk(serving: Server.Serving)(msgs: Rpc*): Seq[Rpc] =
    !.run(Writer.run(through(Writer.of(msgs.toList))(Server.serve(serving))))._1

  val hello = Rpc.Request(Json.JNum(1), Mcp.Initialize, Json.JObj(Vector.empty))

  test("capabilities declare what is THERE, and nothing else") {
    val Rpc.Answer(_, full) = talk(serving)(hello).head: @unchecked
    assert(Mcp.capability(full, "tools"))
    assert(Mcp.capability(full, "resources"))
    assert(Mcp.capability(full, "prompts"))

    // a tools-only server does not advertise the other two
    val Rpc.Answer(_, thin) =
      talk(Server.Serving(info, tools = Seq(spec), call = Map.empty))(hello).head: @unchecked
    assert(Mcp.capability(thin, "tools"))
    assert(!Mcp.capability(thin, "resources"), Json.print(thin))
    assert(!Mcp.capability(thin, "prompts"), Json.print(thin))
  }

  test("a method of a capability the server does not have is MethodNotFound") {
    val toolsOnly = Server.Serving(info, tools = Seq(spec), call = Map("add" -> (_ => "3")))
    val out = talk(toolsOnly)(hello,
      Rpc.Request(Json.JNum(2), Mcp.ResourcesList, Json.JObj(Vector.empty)),
      Rpc.Request(Json.JNum(3), Mcp.PromptsList, Json.JObj(Vector.empty)))
    assertEquals(out(1), Rpc.Failed(Json.JNum(2), Rpc.MethodNotFound, Mcp.ResourcesList))
    assertEquals(out(2), Rpc.Failed(Json.JNum(3), Rpc.MethodNotFound, Mcp.PromptsList))
    // and the same server answers what it DID declare
    assert(talk(toolsOnly)(hello,
      Rpc.Request(Json.JNum(4), Mcp.ToolsList, Json.JObj(Vector.empty)))(1)
      .isInstanceOf[Rpc.Answer])
  }

  test("resources/list and resources/read") {
    val out = talk(serving)(hello,
      Rpc.Request(Json.JNum(2), Mcp.ResourcesList, Json.JObj(Vector.empty)),
      Rpc.Request(Json.JNum(3), Mcp.ResourcesRead, Rpc.obj("uri" -> Json.JStr("okay://a.txt"))))
    val Rpc.Answer(_, listed) = out(1): @unchecked
    val (rs, cursor) = McpDocs.resourcesOf(listed)
    assertEquals(rs.map(_.uri), Seq("okay://a.txt", "okay://b.txt"))
    assertEquals(cursor, None)
    val Rpc.Answer(_, contents) = out(2): @unchecked
    assertEquals(McpDocs.contentsOf(contents), Some("alpha"))
  }

  test("an unknown uri is an ERROR, where an unknown tool is an answer") {
    val out = talk(serving)(hello,
      Rpc.Request(Json.JNum(4), Mcp.ResourcesRead, Rpc.obj("uri" -> Json.JStr("okay://nope"))),
      Rpc.Request(Json.JNum(5), Mcp.ToolsCall,
        Mcp.callParams(ToolCall("x", "nope", Json.JObj(Vector.empty)))))
    assert(out(1).isInstanceOf[Rpc.Failed], out(1).toString)     // the program's mistake
    assert(out(2).isInstanceOf[Rpc.Answer], out(2).toString)     // the model's mistake
  }

  test("prompts/list and prompts/get round-trip turns, arguments substituted") {
    val out = talk(serving)(hello,
      Rpc.Request(Json.JNum(6), Mcp.PromptsList, Json.JObj(Vector.empty)),
      Rpc.Request(Json.JNum(7), Mcp.PromptsGet, Rpc.obj(
        "name" -> Json.JStr("review"),
        "arguments" -> Rpc.obj("path" -> Json.JStr("Rpc.scala")))))
    val Rpc.Answer(_, listed) = out(1): @unchecked
    val ps = McpDocs.promptsOf(listed)
    assertEquals(ps.map(_.name), Seq("review"))
    assertEquals(ps.head.arguments.map(a => (a.name, a.required)), Seq(("path", true)))

    val Rpc.Answer(_, got) = out(2): @unchecked
    assertEquals(McpDocs.turnsOf(got), Seq(
      // the system turn arrives as a user one: MCP prompts have no
      // system role, and that loss is the protocol's
      Turn.User("You review code."),
      Turn.User("Review Rpc.scala")))
  }

  test("an assistant turn keeps its role across the wire") {
    val turns = Seq(Turn.User("hi"), Turn.Assistant("hello"))
    assertEquals(McpDocs.turnsOf(McpDocs.promptResult("", turns)), turns)
  }

  test("an unknown prompt is an error, like an unknown uri") {
    val out = talk(serving)(hello,
      Rpc.Request(Json.JNum(8), Mcp.PromptsGet, Rpc.obj("name" -> Json.JStr("nope"))))
    assert(out(1).isInstanceOf[Rpc.Failed], out(1).toString)
  }
}
