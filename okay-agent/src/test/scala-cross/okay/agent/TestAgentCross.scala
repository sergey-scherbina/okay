package okay.agent

import okay.{!, %, +, Async, Handler, Writer, effect, pure}
import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transport}
import scala.collection.mutable

/**
 * The cross-platform policy applied to the agent: ONE source, run by
 * the JVM suite and by the JS suite under Node.
 *
 * The finding that shaped it: a `Handler[Model]` must ANSWER with a
 * value, so it runs the request to completion inside itself — which
 * needs a thread that can park, and JS has none. The portable shape
 * is therefore peeling rather than handling. Tools and context are
 * program transformers (a relay and a state-threading walk), the
 * model becomes Async by relay, and what remains is driven by
 * `Async.runAsync` — the event loop on JS, a callback drive on the
 * JVM. The agent program itself is untouched: this is the same
 * `Agent.converse` the platform-bound suites use.
 */
class TestAgentCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  case class SearchArgs(query: String)
  given Schema[SearchArgs] = Schema.derived
  val spec = ToolSpec[SearchArgs]("search", "look something up")

  /** the seam, mocked — pure Scala, so it compiles on every platform */
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
        effect[F, String](Writer(reply)).map(_ => ())

  def answer(text: String): String =
    s"""{"choices":[{"message":{"role":"assistant","content":"$text"},"finish_reason":"stop"}]}"""

  val wantsTool =
    """{"choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"search","arguments":"{\"query\":\"okay\"}"}}]},"finish_reason":"tool_calls"}]}"""

  /**
   * Peel the row from the left, and the program is left in Async —
   * which every platform can run. No CanBlock anywhere.
   */
  def drive[A](prog: A ! Agent)(transport: Transport,
                                tools: Map[String, ToolCall => String],
                                policy: okay.Aggregator[Turn, ?, Seq[Turn]] = Compact.all)
  : scala.concurrent.Future[A] =
    val noTools: A ! (Context + (Model + Async)) =
      Handlers.relayTools[A, Context + (Model + Async)](tools)(prog)
    val noContext: A ! (Model + Async) =
      Memory.run[Vector[Turn], A, Model + Async](Compact.all)(noTools)
    val onlyAsync: A ! Async =
      Provider.openAiRelay[A, Async](transport, "k", "m")(noContext)
    Async.runAsync(onlyAsync)

  test("cross: the agent loop runs with nothing that can park") {
    val sent = mutable.Buffer[String]()
    drive(Agent.converse("hello"))(canned(Seq(answer("hi there")), sent), Map.empty)
      .map { out =>
        assertEquals(out, "hi there")
        assertEquals(sent.length, 1)
        assert(sent.head.contains("hello"), sent.head)
      }
  }

  test("cross: a tool call round-trips on the event loop") {
    val sent = mutable.Buffer[String]()
    val calls = mutable.Buffer[String]()
    drive(Agent.converse("find okay", Seq(spec)))(
      canned(Seq(wantsTool, answer("42 hits")), sent),
      Map("search" -> { c =>
        calls += c.name
        ToolSpec.args[SearchArgs](c).fold(e => s"bad: $e", a => s"hits for ${a.query}")
      })).map { out =>
        assertEquals(out, "42 hits")
        assertEquals(calls.toList, List("search"))
        // the tool result went back to the provider
        assert(sent(1).contains("\"role\":\"tool\""), sent(1))
      }
  }

  test("cross: the derived tool schema reaches the wire") {
    val sent = mutable.Buffer[String]()
    drive(Agent.converse("x", Seq(spec)))(canned(Seq(answer("ok")), sent), Map.empty)
      .map { _ =>
        val tools = Json.print(Json.parse(sent.head) match
          case Json.JObj(fs) =>
            val m: Map[String, Json] = fs.toMap
            m("tools")
          case other => fail(s"not an object: $other"))
        assert(tools.contains("\"name\":\"search\""), tools)
        assert(tools.contains("\"query\""), tools)
      }
  }

  test("cross: compaction is the same policy on both platforms") {
    val policy = Compact.window(60)(Compact.chars)
    val turns = (1 to 8).map(i => Turn.User(s"message $i")).toVector
    val whole = turns.foldLeft(policy.init)(policy.add)
    // the property that matters is platform-independent by construction
    for split <- 1 until turns.length do
      val (l, r) = turns.splitAt(split)
      assertEquals(
        policy.present(policy.merge(
          l.foldLeft(policy.init)(policy.add),
          r.foldLeft(policy.init)(policy.add))),
        policy.present(whole))
  }
}
