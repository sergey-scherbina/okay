package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.codec.Json

/**
 * A tool result too big for the context: the model sees a projection
 * and a handle, the whole value stays readable, and widening costs a
 * tool call rather than a prompt inflated in advance.
 */
class TestLarge extends munit.FunSuite {

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

  val big = (1 to 500).map(i => s"line $i of a very long file").mkString("\n")

  def readFile = ToolCall("c1", "read", Json.JObj(Vector.empty))
  def expand(handle: String, from: Int, len: Int) =
    ToolCall("c2", Large.ExpandTool, Json.JObj(Vector(
      "handle" -> Json.JStr(handle),
      "from" -> Json.JNum(from),
      "length" -> Json.JNum(len))))

  test("a huge result reaches the context as a projection, not whole") {
    val store = Large.Store()
    val tools = Large.projecting(
      Handlers.tools(Map("read" -> (_ => big))), store, limit = 1000, window = 300)
    val model = Handlers.scripted(Seq(
      Reply("", Seq(readFile)), Reply("read it", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)

    run(Agent.converse("read the file"))(model, tools, ctx)

    val results = state.recall.collect { case Turn.Result(_, c) => c }
    assertEquals(results.length, 1)
    val seen = results.head
    assert(seen.length < 500, s"the whole file reached the context: ${seen.length}")
    assert(seen.contains("result-1"), seen.take(80))
    assert(seen.contains("line 1 of"), "the projection lost the head of the value")
    // and the whole thing is still there, unabridged
    assertEquals(store.get("result-1"), Some(big))
  }

  test("expand reads more of it, from where the model asks") {
    val store = Large.Store()
    val tools = Large.projecting(
      Handlers.tools(Map("read" -> (_ => big))), store, limit = 1000, window = 300)
    val model = Handlers.scripted(Seq(
      Reply("", Seq(readFile)),
      Reply("", Seq(expand("result-1", 300, 120))),
      Reply("done", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)

    val out = run(Agent.converse("read the file", Seq(Large.spec)))(model, tools, ctx)
    assertEquals(out, "done")

    val widened = state.recall.collect { case Turn.Result(_, c) => c }(1)
    assertEquals(widened.take(120), big.substring(300, 420))
    assert(widened.contains("more characters"), "it did not say what remained")
  }

  test("a small result passes through untouched") {
    val store = Large.Store()
    val tools = Large.projecting(
      Handlers.tools(Map("read" -> (_ => "short answer"))), store, limit = 1000)
    val model = Handlers.scripted(Seq(Reply("", Seq(readFile)), Reply("ok", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)
    run(Agent.converse("read"))(model, tools, ctx)
    assertEquals(state.recall.collect { case Turn.Result(_, c) => c }, Seq("short answer"))
    assertEquals(store.size, 0, "a small result should not have been stored")
  }

  test("an unknown handle is an answer, not a fault") {
    val store = Large.Store()
    val tools = Large.projecting(Handlers.tools(Map.empty), store)
    val model = Handlers.scripted(Seq(
      Reply("", Seq(expand("nope", 0, 10))), Reply("ok", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)
    run(Agent.converse("x", Seq(Large.spec)))(model, tools, ctx)
    assert(state.recall.exists {
      case Turn.Result(_, c) => c.contains("no such result")
      case _ => false
    })
  }
}
