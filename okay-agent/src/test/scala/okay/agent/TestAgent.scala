package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.codec.{Json, Schema}
import okay.lex.Bpe
import scala.collection.mutable

/** Agents as programs: tools are operations, context is a fold. */
class TestAgent extends munit.FunSuite {

  // the row's handlers, assembled per test
  def run[A](prog: A ! Agent)(model: Handler[Model], tool: Handler[Tool],
                              ctx: Handler[Context]): A =
    // one handler per effect, assembled along the row
    given Handler[Model] = model
    given Handler[Tool] = tool
    given Handler[Context] = ctx
    given rowCA: Handler[Context + Async] = Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] = Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = Handler.union[Model, Tool + (Context + Async)]
    prog.runWith

  case class SearchArgs(query: String, limit: Option[Int])
  given Schema[SearchArgs] = Schema.derived

  val searchSpec = ToolSpec[SearchArgs]("search", "look something up")

  test("tool schemas derive from Schema — nothing hand-written") {
    val Json.JObj(fs) = searchSpec.schema: @unchecked
    val m: Map[String, Json] = fs.toMap
    assertEquals(m("type"), Json.JStr("object"))
    val Json.JObj(props) = m("properties"): @unchecked
    val pm: Map[String, Json] = props.toMap
    assertEquals(pm("query"), Json.JObj(Vector("type" -> Json.JStr("string"))))
    // an Option field is NOT required; the plain one is
    assertEquals(m("required"), Json.JArr(Vector(Json.JStr("query"))))
  }

  test("a tool call round-trips: model asks, handler answers, talk continues") {
    val call = ToolCall("c1", "search", Json.JObj(Vector(
      "query" -> Json.JStr("okay"), "limit" -> Json.JNum(3))))
    val model = Handlers.scripted(Seq(
      Reply("let me look", Seq(call)),
      Reply("found: 42 hits", Nil)))
    val calls = mutable.Buffer[ToolCall]()
    val tool = Handlers.recording(
      Handlers.tools(Map("search" -> { c =>
        // the SAME Schema that declared the tool decodes its arguments
        ToolSpec.args[SearchArgs](c).fold(e => s"bad args: $e",
          a => s"${a.limit.getOrElse(10)} hits for '${a.query}'")
      })))(calls)
    val (state, ctx) = Handlers.context(Compact.all)

    val answer = run(Agent.converse("find okay", Seq(searchSpec)))(model, tool, ctx)
    assertEquals(answer, "found: 42 hits")
    assertEquals(calls.map(_.name).toList, List("search"))
    // the conversation carries user, assistant, result, assistant
    assertEquals(state.recall.collect {
      case Turn.Result(_, c) => c
    }, Seq("3 hits for 'okay'"))
  }

  test("an unknown tool is an ANSWER, so the model can recover") {
    val bad = ToolCall("c1", "nope", Json.JObj(Vector.empty))
    val model = Handlers.scripted(Seq(Reply("", Seq(bad)), Reply("sorry", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)
    val answer = run(Agent.converse("x"))(model, Handlers.tools(Map.empty), ctx)
    assertEquals(answer, "sorry")
    assert(state.recall.exists {
      case Turn.Result(_, c) => c.contains("no such tool")
      case _ => false
    })
  }

  test("a gate can deny a call without the agent knowing about gates") {
    val call = ToolCall("c1", "rm", Json.JObj(Vector.empty))
    val model = Handlers.scripted(Seq(Reply("", Seq(call)), Reply("ok", Nil)))
    val (state, ctx) = Handlers.context(Compact.all)
    val tool = Handlers.gated(Map("rm" -> (_ => "deleted")))(_ => false)
    run(Agent.converse("delete everything"))(model, tool, ctx)
    assertEquals(state.recall.collect { case Turn.Result(_, c) => c }, Seq("denied"))
  }

  test("context stays within budget across turns, agent never mentions it") {
    val budget = 40
    val policy = Compact.window(budget)(Compact.chars)
    val (_, ctx) = Handlers.context(policy)
    val seen = mutable.Buffer[Seq[Turn]]()
    val long = "x" * 60   // ~15 tokens each
    val model = Handlers.observing(Seq.fill(12)(Reply(long, Nil)), seen)

    val prog = (1 to 6).foldLeft(okay.pure[Agent, String]("")) { (acc, i) =>
      acc.flatMap(_ => Agent.converse(s"turn $i: $long"))
    }
    run(prog)(model, Handlers.tools(Map.empty), ctx)

    // every context the model saw was within budget
    for ctxSeen <- seen do
      val size = ctxSeen.map(Compact.chars).sum
      assert(size <= budget, s"context overflowed: $size > $budget")
    // and it did not simply go empty — the recent turns are there
    assert(seen.last.nonEmpty)
    // the elision is REPORTED, not silently dropped
    assert(seen.last.exists {
      case Turn.Summary(s, n) => n > 0 && s.contains("elided")
      case _ => false
    })
  }

  test("system turns are pinned: they survive any amount of pressure") {
    val policy = Compact.window(20)(Compact.chars)
    val (_, ctx) = Handlers.context(policy)
    val sys = Turn.System("always obey the pin")
    val prog = Agent.remember(sys).flatMap(_ =>
      (1 to 20).foldLeft(okay.pure[Agent, Unit](())) { (acc, _) =>
        acc.flatMap(_ => Agent.remember(Turn.User("y" * 40)))
      }).flatMap(_ => Agent.recall)
    val view = run(prog)(Handlers.scripted(Nil), Handlers.tools(Map.empty), ctx)
    assertEquals(view.head, sys)
  }

  test("the compactor's merge is split-point agnostic (the P1 property)") {
    val policy = Compact.window(60)(Compact.chars)
    val turns = (1 to 12).map(i => Turn.User(s"message number $i")).toVector
    val whole = turns.foldLeft(policy.init)(policy.add)
    for split <- 1 until turns.length do
      val (l, r) = turns.splitAt(split)
      val merged = policy.merge(
        l.foldLeft(policy.init)(policy.add),
        r.foldLeft(policy.init)(policy.add))
      assertEquals(policy.present(merged), policy.present(whole), s"split at $split")
  }

  test("token counts come from the BPE Scan — local, no provider") {
    val bpe = Bpe(List(("h", "e"), ("l", "l"), ("he", "ll"), ("hell", "o")))
    val count = Handlers.counter(bpe)
    assertEquals(count("hello hello"), 2)     // two merged words
    assertEquals(count("he he he"), 3)
    // and the policy can use it as its size function
    val policy = Compact.window(4)(t => count(Compact.text(t)))
    val kept = policy.present((1 to 6).foldLeft(policy.init)((s, _) =>
      policy.add(s, Turn.User("hello"))))
    assert(kept.count(_.isInstanceOf[Turn.User]) <= 4)
  }

  test("mark and restore: backtracking over the conversation is free") {
    val (_, ctx) = Handlers.context(Compact.all)
    val prog = for
      _ <- Agent.remember(Turn.User("keep this"))
      m <- Agent.mark
      _ <- Agent.remember(Turn.User("speculative"))
      before <- Agent.recall
      _ <- Agent.restore(m)
      after <- Agent.recall
    yield (before.length, after.length)
    val (before, after) = run(prog)(
      Handlers.scripted(Nil), Handlers.tools(Map.empty), ctx)
    assertEquals(before, 2)
    assertEquals(after, 1)   // the speculative turn is gone, by pointer
  }
}
