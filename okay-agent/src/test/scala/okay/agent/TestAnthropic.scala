package okay.agent

import okay.{!, %, +, Async, Handler, Writer, effect}
import okay.given
import okay.codec.{Json, Schema}
import okay.llm.Transport
import scala.collection.mutable

/**
 * The same effect, a genuinely different wire — which is the point of
 * having two providers rather than one. Anthropic's Messages API
 * puts `system` at the top level, carries content as typed blocks,
 * names a tool's schema `input_schema`, hands tool arguments over as
 * an object rather than a string of JSON, and takes results back as a
 * user message of tool_result blocks. If any of that had reached the
 * `Model` effect, the effect would have been OpenAI-shaped by
 * accident; none of it did.
 */
class TestAnthropic extends munit.FunSuite {

  def canned(bodies: Seq[String], sent: mutable.Buffer[String]): Transport =
    new Transport:
      private var rest = bodies.toList
      def post(url: String, headers: Map[String, String], body: String)
      : Unit ! (Writer % String + Async) =
        type F = Writer % String + Async
        sent += body
        val reply = rest match
          case r :: t => rest = t; r
          case Nil => """{"content":[]}"""
        effect[F, String](Writer(reply)).map(_ => ())

  case class SearchArgs(query: String)
  given Schema[SearchArgs] = Schema.derived
  val spec = ToolSpec[SearchArgs]("search", "look something up")

  def answer(text: String): String =
    s"""{"id":"msg_1","type":"message","role":"assistant","content":[{"type":"text","text":"$text"}],"stop_reason":"end_turn","usage":{"input_tokens":9,"output_tokens":3}}"""

  val wantsTool =
    """{"id":"msg_2","type":"message","role":"assistant","content":[{"type":"text","text":"looking"},{"type":"tool_use","id":"toolu_1","name":"search","input":{"query":"okay"}}],"stop_reason":"tool_use"}"""

  def run[A](prog: A ! Agent)(model: Handler[Model], tool: Handler[Tool],
                              ctx: Handler[Context]): A =
    given Handler[Model] = model
    given Handler[Tool] = tool
    given Handler[Context] = ctx
    given rowCA: Handler[Context + Async] = okay.Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] =
      okay.Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Model, Tool + (Context + Async)]
    prog.runWith

  test("system is lifted to the top level and content becomes blocks") {
    val sent = mutable.Buffer[String]()
    val model = Provider.anthropic(canned(Seq(answer("hi")), sent), "k", "claude-x")
    val (_, ctx) = Handlers.context(Compact.all)
    val prog = Agent.remember(Turn.System("be brief"))
      .flatMap(_ => Agent.converse("hello there", Seq(spec)))
    run(prog)(model, Handlers.tools(Map.empty), ctx)

    val fields: Map[String, Json] = Json.parse(sent.head) match
      case Json.JObj(fs) => fs.toMap
      case other => fail(s"not an object: $other")

    assertEquals(fields("system"), Json.JStr("be brief"))
    val messages = Json.print(fields("messages"))
    assert(!messages.contains("be brief"), "the system turn leaked into the messages")
    assert(messages.contains("\"type\":\"text\""), messages)
    // a tool declares its schema under input_schema, not parameters
    val tools = Json.print(fields("tools"))
    assert(tools.contains("input_schema"), tools)
    assert(!tools.contains("parameters"), tools)
    // max_tokens is required by this API and is always sent
    assert(fields.contains("max_tokens"))
  }

  test("a tool_use block round-trips and the result returns as blocks") {
    val sent = mutable.Buffer[String]()
    val model = Provider.anthropic(canned(Seq(wantsTool, answer("done")), sent), "k", "c")
    val calls = mutable.Buffer[ToolCall]()
    val tools = Handlers.recording(Handlers.tools(Map("search" -> { c =>
      ToolSpec.args[SearchArgs](c).fold(e => s"bad: $e", a => s"hits for ${a.query}")
    })))(calls)
    val (_, ctx) = Handlers.context(Compact.all)

    val out = run(Agent.converse("find okay", Seq(spec)))(model, tools, ctx)
    assertEquals(out, "done")
    // the arguments arrived as an OBJECT here, not a string of JSON
    assertEquals(calls.head.args, Json.JObj(Vector("query" -> Json.JStr("okay"))))
    assertEquals(calls.head.id, "toolu_1")
    // and the result went back as a user message of tool_result blocks
    assert(sent(1).contains("tool_result"), sent(1))
    assert(sent(1).contains("toolu_1"), sent(1))
  }

  test("parallel tool results merge into ONE user message") {
    val two = """{"id":"m","type":"message","role":"assistant","content":[{"type":"tool_use","id":"t1","name":"search","input":{"query":"a"}},{"type":"tool_use","id":"t2","name":"search","input":{"query":"b"}}],"stop_reason":"tool_use"}"""
    val sent = mutable.Buffer[String]()
    val model = Provider.anthropic(canned(Seq(two, answer("ok")), sent), "k", "c")
    val tools = Handlers.tools(Map("search" -> (_ => "a hit")))
    val (_, ctx) = Handlers.context(Compact.all)
    run(Agent.converse("both", Seq(spec)))(model, tools, ctx)

    val messages: Json = Json.parse(sent(1)) match
      case Json.JObj(fs) =>
        val m: Map[String, Json] = fs.toMap
        m("messages")
      case other => fail(s"not an object: $other")
    val userBlocks: Vector[Json] = messages match
      case Json.JArr(ms) => ms.collect {
        case Json.JObj(fs) =>
          val m: Map[String, Json] = fs.toMap
          (m.get("role"), m.get("content"))
      }.collect { case (Some(Json.JStr("user")), Some(c)) => c }
      case _ => Vector.empty
    // the last user message carries BOTH results, as the API requires
    val last = userBlocks.last match
      case Json.JArr(bs) => bs.length
      case _ => 0
    assertEquals(last, 2, "parallel tool results were sent as separate messages")
  }

  test("a truncated body yields the blocks that arrived") {
    val cut = """{"id":"m","content":[{"type":"text","text":"half an ans"""
    val sent = mutable.Buffer[String]()
    val model = Provider.anthropic(canned(Seq(cut), sent), "k", "m")
    val (_, ctx) = Handlers.context(Compact.all)
    assertEquals(run(Agent.converse("x"))(model, Handlers.tools(Map.empty), ctx),
      "half an ans")
  }
}
