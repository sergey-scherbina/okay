package okay.ui

import okay.*
import okay.given

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

  test("a BOUND outer prompt crosses the inner scope — multi-prompt kept, opt-in") {
    val prog: String ! Dialog = Scope.bounded[String]: (outer: okay.Prompt[String]) ?=>
      Scope.mark[String]:
        Scope.exit[String, String]("straight-out")(using outer)
      .map(inner => s"NEVER:$inner")
    assertEquals(runDialog(prog), "straight-out")
  }
}
