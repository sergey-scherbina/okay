package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.rag.*
import scala.collection.mutable

/**
 * The end of the plan: code reaches the model through `recall`, under
 * one budget, without a tool call.
 */
class TestGrounded extends munit.FunSuite {

  val files = Seq(
    Source("Greeter.scala",
      "/** Greets people by name. */\n" +
        "class Greeter(name: String) {\n" +
        "  def hello: String = \"Hello, \" + name\n" +
        "}\n"),
    Source("Http.scala",
      "/** Sends requests over the network. */\n" +
        "class HttpClient(timeout: Int) {\n" +
        "  def get(url: String): String = fetch(url)\n" +
        "}\n"))

  val segments = files.flatMap(f => Ingest.segment(f, 400)(_.length))
  val keyword = Retrieve.keyword(Keyword.index(segments))

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

  test("recall already contains the relevant code — no tool call") {
    val seen = mutable.Buffer[Seq[Turn]]()
    val model = Handlers.observing(Seq(Reply("it greets by name", Nil)), seen)
    val calls = mutable.Buffer[ToolCall]()
    val tools = Handlers.recording(Handlers.tools(Map.empty))(calls)
    val (_, ctx) = Grounded.context(Compact.all, keyword,
      budget = 2000, share = 0.5, k = 3)(Compact.chars)

    val answer = run(Agent.converse("what does Greeter do?"))(model, tools, ctx)
    assertEquals(answer, "it greets by name")
    assert(calls.isEmpty, "the agent should not have needed a tool")

    val context = seen.head
    assert(context.exists(t => Compact.text(t).contains("def hello")),
      s"the code never reached the model: ${context.map(Compact.text(_).take(40))}")
    assert(context.exists(t => Compact.text(t).contains("Greeter.scala")),
      "the passage lost its provenance on the way in")
  }

  test("retrieval follows the question: a different query, different code") {
    val seen = mutable.Buffer[Seq[Turn]]()
    val model = Handlers.observing(Seq(Reply("over the network", Nil)), seen)
    val (_, ctx) = Grounded.context(Compact.all, keyword,
      budget = 2000, share = 0.5, k = 2)(Compact.chars)

    run(Agent.converse("how are requests sent?"))(model, Handlers.tools(Map.empty), ctx)
    val context = seen.head
    assert(context.exists(t => Compact.text(t).contains("HttpClient")),
      "the http file should have been retrieved for a network question")
  }

  test("ONE budget: passages and conversation cannot starve each other") {
    val budget = 300
    val seen = mutable.Buffer[Seq[Turn]]()
    val model = Handlers.observing(
      Seq.fill(6)(Reply("x" * 200, Nil)), seen)
    val (_, ctx) = Grounded.context(Compact.all, keyword,
      budget = budget, share = 0.5, k = 4)(Compact.chars)

    val prog = (1 to 4).foldLeft(okay.pure[Agent, String]("")) { (acc, i) =>
      acc.flatMap(_ => Agent.converse(s"question $i about Greeter " + "y" * 100))
    }
    run(prog)(model, Handlers.tools(Map.empty), ctx)

    for ctxSeen <- seen do
      val used = ctxSeen.map(Compact.chars).sum
      assert(used <= budget, s"the shared budget overflowed: $used > $budget")
      // and neither side was crowded out entirely
      assert(ctxSeen.exists(t => Compact.text(t).contains("Greeter.scala")),
        "retrieval was starved")
      assert(ctxSeen.exists {
        case Turn.User(_) => true
        case _ => false
      }, "the conversation was starved")
  }

  test("the explicit search tool still works for an agent that steers") {
    val call = ToolCall("c1", "search",
      okay.codec.Json.JObj(Vector("q" -> okay.codec.Json.JStr("network"))))
    val model = Handlers.scripted(Seq(
      Reply("let me look", Seq(call)), Reply("found it", Nil)))
    val tools = Handlers.tools(Map("search" -> { _ =>
      okay.!.run(keyword.retrieve("network", 1)).headOption
        .fold("nothing")(_.segment.text)
    }))
    val (state, ctx) = Handlers.context(Compact.all)
    assertEquals(run(Agent.converse("find it"))(model, tools, ctx), "found it")
    assert(state.recall.exists {
      case Turn.Result(_, c) => c.contains("HttpClient")
      case _ => false
    })
  }
}
