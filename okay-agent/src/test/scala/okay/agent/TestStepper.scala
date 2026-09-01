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
    var toolSaw = ""
    val edited = drive(stepped(
      Agent.complete().flatMap(r => Agent.call(r.calls.head)))) { c =>
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
      case Stepping.Paused(_, resume) =>
        // the SAME continuation, resumed twice with different pasts
        resume("first world").flatMap { a =>
          resume("second world").map { b => (a, b) }
        }
      case done => pure((done, done))
    }
    val (a, b) = runRest(forked)(model, freshCtx)
    assertEquals(a, Stepping.Done("FIRST WORLD"))
    assertEquals(b, Stepping.Done("SECOND WORLD"))
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
}
