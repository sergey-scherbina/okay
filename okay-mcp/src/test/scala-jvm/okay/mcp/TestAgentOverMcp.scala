package okay.mcp

import okay.*
import okay.given
import okay.agent.*
import okay.codec.{Json, Schema}

/**
 * The claim the module exists for: an agent program does not change
 * by one character when its tools come from an MCP server.
 *
 * The test runs the SAME program twice — once against a local tool
 * table, once against an MCP server over a wire — and asserts the two
 * answers are identical. Nothing in the program mentions MCP; the
 * only difference between the runs is which `Handler[Tool]` was in
 * scope, which is the whole thesis of handlers as policy.
 */
class TestAgentOverMcp extends munit.FunSuite {

  final case class SearchArgs(query: String, limit: Option[Int])
  given Schema[SearchArgs] = Schema.derived

  val spec = ToolSpec[SearchArgs]("search", "look something up")

  val table = Map[String, ToolCall => String]("search" -> { c =>
    ToolSpec.args[SearchArgs](c).fold(e => s"bad args: $e",
      a => s"${a.limit.getOrElse(10)} hits for '${a.query}'")
  })

  /** the program under test — note what it does NOT say */
  def program: String ! Agent = Agent.converse("find okay", Seq(spec))

  val call = ToolCall("c1", "search", Json.JObj(Vector(
    "query" -> Json.JStr("okay"), "limit" -> Json.JNum(3))))

  def script = Handlers.scripted(Seq(
    Reply("let me look", Seq(call)),
    Reply("done", Nil)))

  def run[A](prog: A ! Agent)(tool: Handler[Tool]): A =
    given Handler[Model] = script
    given Handler[Tool] = tool
    given Handler[Context] = Handlers.context(Compact.all)._2
    given rowCA: Handler[Context + Async] = Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] = Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = Handler.union[Model, Tool + (Context + Async)]
    prog.runWith

  /** an MCP server of the same tools, on a fiber, and a session to it */
  def mcpTools: Handler[Tool] =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), Mcp.Info("okay-mcp", "0.1"), Seq(spec), table)): Unit
    Client.connect(link(up, down), Mcp.Info("agent", "1")).runWith.handler

  test("the same agent program, local tools and MCP tools, same answer") {
    val local = run(program)(Handlers.tools(table))
    val overMcp = run(program)(mcpTools)
    assertEquals(overMcp, local)
    assertEquals(overMcp, "done")
  }

  test("what the model saw came back through the wire, verbatim") {
    val seen = scala.collection.mutable.Buffer[ToolCall]()
    val (state, ctx) = Handlers.context(Compact.all)
    given Handler[Model] = script
    given Handler[Tool] = Handlers.recording(mcpTools)(seen)
    given Handler[Context] = ctx
    given rowCA: Handler[Context + Async] = Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] = Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = Handler.union[Model, Tool + (Context + Async)]
    program.runWith: Unit

    assertEquals(seen.map(_.name).toList, List("search"))
    // the tool RESULT in the conversation is the MCP server's answer
    assertEquals(state.recall.collect { case Turn.Result(_, c) => c }.toList,
      List("3 hits for 'okay'"))
  }

  test("an MCP server's tools are ToolSpecs: the agent declares what it found") {
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), Mcp.Info("okay-mcp", "0.1"), Seq(spec), table)): Unit
    val session = Client.connect(link(up, down), Mcp.Info("agent", "1")).runWith

    // discovered, not declared: the specs a model is told about can
    // come from the server, schema and all
    val discovered = session.tools.runWith
    assertEquals(discovered, Seq(spec))
    assertEquals(run(Agent.converse("find okay", discovered))(session.handler), "done")
  }
}
