package okay.agent

import okay.{!, %, +, Async, Handler, Writer, effect, pure}
import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transport}
import scala.collection.mutable

/**
 * The agent against the real protocol — only the socket is mocked.
 * What this proves is that nothing above the effect changed: the same
 * converse loop, the same compaction, the same tool schemas.
 */
class TestProvider extends munit.FunSuite {

  /** a transport that records what was sent and replies with a script */
  def canned(bodies: Seq[String], sent: mutable.Buffer[String]): Transport =
    new Transport:
      private var rest = bodies.toList
      def post(url: String, headers: Map[String, String], body: String)
      : Unit ! (Writer % String + Async) =
        type F = Writer % String + Async
        sent += body
        val reply = rest match
          case r :: t => rest = t; r
          case Nil => """{"choices":[]}"""
        effect[F, Unit](Writer(reply)).map(_ => ())

  case class SearchArgs(query: String)
  given Schema[SearchArgs] = Schema.derived
  val spec = ToolSpec[SearchArgs]("search", "look something up")

  def answer(text: String): String =
    s"""{"choices":[{"message":{"role":"assistant","content":"$text"},
       |"finish_reason":"stop"}]}""".stripMargin

  val wantsTool =
    """{"choices":[{"message":{"role":"assistant","content":null,
      |"tool_calls":[{"id":"call_1","type":"function","function":
      |{"name":"search","arguments":"{\"query\": \"okay\"}"}}]},
      |"finish_reason":"tool_calls"}]}""".stripMargin

  def run[A](prog: A ! Agent)(model: Handler[Model], tool: Handler[Tool],
                              ctx: Handler[Context]): A =
    given Handler[Model] = model
    given Handler[Tool] = tool
    given Handler[Context] = ctx
    given rowMA: Handler[Model + Async] = okay.Handler.union[Model, Async]
    given rowCMA: Handler[Context + (Model + Async)] =
      okay.Handler.union[Context, Model + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Tool, Context + (Model + Async)]
    prog.runWith

  test("the request carries the turns and the DERIVED tool schema") {
    val sent = mutable.Buffer[String]()
    val model = Provider.openAi(canned(Seq(answer("hi")), sent), "k", "gpt-x")
    val (_, ctx) = Handlers.context(Compact.all)
    val _ = run(Agent.converse("hello there", Seq(spec)))(
      model, Handlers.tools(Map.empty), ctx)

    assertEquals(sent.length, 1)
    val body = Json.parse(sent.head)
    val fields = body match
      case Json.JObj(fs) => fs.toMap
      case other => fail(s"not an object: $other")

    assertEquals(fields("model"), Json.JStr("gpt-x"))
    // the user's turn made it onto the wire
    assert(Json.print(fields("messages")).contains("hello there"))
    // and the tool declaration is the schema Schema.derived produced
    val tools = Json.print(fields("tools"))
    assert(tools.contains("\"name\":\"search\""), tools)
    assert(tools.contains("\"query\""), tools)
    assert(tools.contains("\"required\""), tools)
  }

  test("a tool call comes back decoded, runs, and the answer returns") {
    val sent = mutable.Buffer[String]()
    val model = Provider.openAi(canned(Seq(wantsTool, answer("42 hits")), sent), "k", "m")
    val calls = mutable.Buffer[ToolCall]()
    val tools = Handlers.recording(Handlers.tools(Map("search" -> { c =>
      // the SAME Schema that declared the tool decodes the arguments
      ToolSpec.args[SearchArgs](c).fold(e => s"bad: $e", a => s"hits for ${a.query}")
    })))(calls)
    val (_, ctx) = Handlers.context(Compact.all)

    val out = run(Agent.converse("find okay", Seq(spec)))(model, tools, ctx)
    assertEquals(out, "42 hits")
    assertEquals(calls.map(_.name).toList, List("search"))
    assertEquals(calls.head.args, Json.JObj(Vector("query" -> Json.JStr("okay"))))
    // the tool result went back to the provider as a `tool` message
    assert(sent(1).contains("\"role\":\"tool\""), sent(1))
    assert(sent(1).contains("call_1"), sent(1))
  }

  test("a truncated response body yields what it carried, not an exception") {
    val cut = """{"choices":[{"message":{"role":"assistant","content":"half an ans"""
    val sent = mutable.Buffer[String]()
    val model = Provider.openAi(canned(Seq(cut), sent), "k", "m")
    val (_, ctx) = Handlers.context(Compact.all)
    val out = run(Agent.converse("x"))(model, Handlers.tools(Map.empty), ctx)
    assertEquals(out, "half an ans")
  }

  test("an empty or damaged body is an empty answer, never a fault") {
    val sent = mutable.Buffer[String]()
    val model = Provider.openAi(canned(Seq("@@ not json @@"), sent), "k", "m")
    val (_, ctx) = Handlers.context(Compact.all)
    assertEquals(run(Agent.converse("x"))(model, Handlers.tools(Map.empty), ctx), "")
  }

  test("streaming: SSE deltas become text tokens, non-tokens dropped") {
    val events = List(
      """data: {"choices":[{"delta":{"role":"assistant"}}]}""", "",
      """data: {"choices":[{"delta":{"content":"Hel"}}]}""", "",
      """data: {"choices":[{"delta":{"content":"lo"}}]}""", "",
      "data: [DONE]", "")
    val transport = new Transport:
      def post(url: String, headers: Map[String, String], body: String)
      : Unit ! (Writer % String + Async) =
        type F = Writer % String + Async
        events.foldLeft(pure[F, Unit](()))((acc, l) =>
          acc.flatMap(_ => effect[F, Unit](Writer(l)).map(_ => ())))

    val tokens = collect(OpenAi.stream(transport, "k",
      OpenAi.request("m", Seq(OpenAi.message("user", "hi")), stream = true)))
    assertEquals(tokens.mkString, "Hello")
  }

  test("a payload cut mid-JSON is simply not a token") {
    assertEquals(OpenAi.token("""{"choices":[{"delta":{"conte"""), None)
    assertEquals(OpenAi.token("[DONE]"), None)
    assertEquals(OpenAi.token("""{"choices":[{"delta":{"content":"x"}}]}"""), Some("x"))
  }

  test("compaction and the provider meet: the budget shapes the request") {
    val sent = mutable.Buffer[String]()
    val model = Provider.openAi(
      canned(Seq.fill(4)(answer("ok")), sent), "k", "m")
    val (_, ctx) = Handlers.context(Compact.window(120)(Compact.chars))
    val prog = (1 to 3).foldLeft(okay.pure[Agent, String]("")) { (acc, i) =>
      acc.flatMap(_ => Agent.converse(s"turn $i " + "z" * 200))
    }
    run(prog)(model, Handlers.tools(Map.empty), ctx): Unit
    // the wire never carried more than the policy allows
    for body <- sent do
      assert(body.length < 2000, s"the compactor let a whole history through: ${body.length}")
  }

  /** drain a token stream (no real waiting in these tests) */
  def collect(s: Unit ! (Writer % String + Async)): List[String] =
    import okay.!.*
    def go(rest: Unit ! (Writer % String + Async), acc: List[String]): List[String] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => summon[Handler[Async]].handle(a); acc.reverse
          case Right(Writer.Say(w)) => (w :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Writer % String](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          // a tell answers nothing — the continuation gets unit, not the line
          case Right(Writer.Say(w)) => go(k(()), w :: acc)
    go(s, Nil)
}
