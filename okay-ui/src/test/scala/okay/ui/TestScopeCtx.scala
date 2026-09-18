package okay.ui

import okay.*

/**
 * The capability door on Scope (ctx-prompts): exit reaches the
 * NEAREST scope by nesting, a bound prompt still crosses, and the
 * explicit forms are untouched beside it.
 */
class TestScopeCtx extends munit.FunSuite {

  def runDialog[A](prog: A ! Dialog, answers: Event*): A =
    var r = Dialog.start(prog)
    for e <- answers do r = Dialog.step(r, e)
    r match
      case Dialog.Running.Done(a) => a
      case other => fail(s"not done: $other")

  test("bounded { exit(v) } — no prompt in hand; the nearest scope answers") {
    val prog: String ! Dialog = Scope.bounded[String]:
      Scope.lift(Dialog.show(Ui.Text("q"))).flatMap { _ =>
        Scope.exit[String, String]("bailed")
      }
    assertEquals(runDialog(prog, Event.Pressed("x")), "bailed")
  }

  test("two nested marks: exit reaches the INNER; the outer completes normally") {
    val prog: String ! Dialog = Scope.bounded[String]:
      Scope.mark[String]:
        Scope.exit[String, String]("inner-exit")
      .map(inner => s"outer-saw:$inner")
    assertEquals(runDialog(prog), "outer-saw:inner-exit")
  }

  /**
   * WHAT THE MOVE BOUGHT (delim-doors-are-prompted, 2026-09-18): the
   * mistake it prevents is not exotic. A caller who has no scope at
   * all could summon `Delim.prompt[String]` — one line — hand it as
   * the given, and `exit` would COMPILE and then die at runtime with
   * `NoPrompt`. The evidence cannot be made outside `Delim`, so the
   * same program is now refused by the compiler.
   */
  test("an exit with a forged prompt does not compile — and a real scope still does") {
    val forged = compileErrors("""
      import okay.*
      import okay.ui.*
      given okay.Prompt[String] = okay.Delim.prompt[String]
      val p: String ! Scope.Row = Scope.exit[String, String]("nowhere")
    """)
    assert(forged.nonEmpty, "a forged prompt still compiles")
    assertEquals(compileErrors("""
      import okay.*
      import okay.ui.*
      val p: String ! Dialog = Scope.bounded[String](Scope.exit[String, String]("ok"))
    """), "")
  }

  test("a BOUND outer prompt crosses the inner scope — multi-prompt kept, opt-in") {
    // the bound name is the EVIDENCE now (delim-doors-are-prompted):
    // a `Prompt` is one line to make and proves nothing, so binding
    // one here would have proved nothing either
    val prog: String ! Dialog = Scope.bounded[String]: (outer: okay.Delim.Prompted[String]) ?=>
      Scope.mark[String]:
        Scope.exit[String, String]("straight-out")(using outer)
      .map(inner => s"NEVER:$inner")
    assertEquals(runDialog(prog), "straight-out")
  }
}
