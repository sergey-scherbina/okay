package okay.agent

import okay.*
import okay.given
import okay.codec.Json
import Stepper.*

/** specs/llm-agentic.md, "The stepper" — one test per box */
class TestStepper extends munit.FunSuite {

  def runRest[A](prog: A ! Rest)(model: Handler[Model], ctx: Handler[Context]): A =
    given Handler[Model] = model
    given Handler[Context] = ctx
    given rowMA: Handler[Model + Async] = Handler.union[Model, Async]
    given rowAll: Handler[Rest] = Handler.union[Context, Model + Async]
    prog.runWith

  def freshCtx: Handler[Context] = Handlers.context(Compact.all)._2

  val search = ToolCall("1", "search", Json.JObj(Vector("q" -> Json.JStr("okay"))))
  val fetch = ToolCall("2", "fetch", Json.JObj(Vector("u" -> Json.JStr("x"))))

  /** the agent: ask the model, run its calls, ask again, answer */
  def agent: String ! Agent =
    Agent.complete().flatMap { r1 =>
      Agent.runTools(r1.calls).flatMap { _ =>
        Agent.complete().map(_.text)
      }
    }

  test("the run pauses at every tool call; fed the real results, it equals the unstepped run") {
    val table = Map[String, ToolCall => String](
      "search" -> (_ => "found it"), "fetch" -> (_ => "the page"))
    def model = Handlers.scripted(Seq(
      Reply("", Seq(search, fetch)), Reply("the answer", Nil)))

    // unstepped, for the baseline
    val direct =
      given Handler[Model] = model
      given Handler[Tool] = Handlers.tools(table)
      given Handler[Context] = freshCtx
      given r1: Handler[Model + Async] = Handler.union[Model, Async]
      given r2: Handler[Context + (Model + Async)] = Handler.union[Context, Model + Async]
      given r3: Handler[Agent] = Handler.union[Tool, Context + (Model + Async)]
      agent.runWith

    // stepped: collect what paused, answer from the same table
    val seen = scala.collection.mutable.Buffer[String]()
    val steppedRun = drive(stepped(agent)) { c =>
      seen += c.name
      pure(table(c.name)(c))
    }
    assertEquals(runRest(steppedRun)(model, freshCtx), direct)
    assertEquals(seen.toList, List("search", "fetch"))
  }

  test("the operator edits a result mid-flight; the program is none the wiser") {
    def model = Handlers.scripted(Seq(
      Reply("", Seq(search)), Reply("done", Nil)))
    val edited = drive(stepped(
      Agent.complete().flatMap(r => Agent.call(r.calls.head)))) { _ =>
      pure("EDITED: nothing like what the tool would say")
    }
    val result = runRest(edited)(model, freshCtx)
    assertEquals(result, "EDITED: nothing like what the tool would say")
  }

  test("multi-shot: one pause, two futures — fork the run at a tool call") {
    def model = Handlers.scripted(Seq(Reply("", Seq(search))))
    val prog: String ! Agent =
      Agent.complete().flatMap(r => Agent.call(r.calls.head)).map(_.toUpperCase)

    val forked = stepped(prog).flatMap {
      case Delim.Paused.Ask(_, resume, _) =>
        // the SAME continuation, resumed twice with different pasts.
        // `Delim.run` is what the bespoke enum used to hide: the
        // resumption is a program in the machine's own row.
        Delim.run(resume("first world")).flatMap { a =>
          Delim.run(resume("second world")).map { b => (a.finished, b.finished) }
        }
      case done => pure((done.finished, done.finished))
    }
    val (a, b) = runRest(forked)(model, freshCtx)
    assertEquals(a, Some("FIRST WORLD"))
    assertEquals(b, Some("SECOND WORLD"))
  }

  test("the transparent driver: stepping with nobody watching equals not stepping") {
    val table = Map[String, ToolCall => String]("search" -> (_ => "found"))
    def model = Handlers.scripted(Seq(Reply("", Seq(search)), Reply("fin", Nil)))
    val direct =
      given Handler[Model] = model
      given Handler[Tool] = Handlers.tools(table)
      given Handler[Context] = freshCtx
      given r1: Handler[Model + Async] = Handler.union[Model, Async]
      given r2: Handler[Context + (Model + Async)] = Handler.union[Context, Model + Async]
      given r3: Handler[Agent] = Handler.union[Tool, Context + (Model + Async)]
      agent.runWith
    assertEquals(runRest(transparent(stepped(agent))(table))(model, freshCtx), direct)
  }

  test("a stepping run is NOT replayable, and the row says why") {
    // the backlog asked for this rewrite partly to gain `Delim.replay`
    // for free. It does not: replaying a stepping session would ask
    // the MODEL again, and `Replayable` refuses the row that says so.
    val e = compileErrors("""
      okay.Delim.replay[ToolCall, String, String, Stepper.Rest](
        summon[okay.Delim.Asking[ToolCall, String, String, okay.Delim + Stepper.Rest]] ?=>
          okay.pure(""))(Nil)""")
    assert(e.nonEmpty, "a stepping run typechecked as replayable")
  }
}
