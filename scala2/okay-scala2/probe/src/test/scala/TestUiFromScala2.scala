package scala2probe

import okay.scala2._
import okay.ui.{Event, Frame, Ui}

object UiModel {
  // a counter: two buttons, a label
  def view(n: Int): Ui = Ui.Column(Vector(
    Ui.Text("count: " + n),
    Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc")))))

  def update(n: Int, e: Event): Int = e match {
    case Event.Pressed("inc") => n + 1
    case Event.Pressed("dec") => n - 1
    case _ => n
  }
}

/** okay-ui from Scala 2.13 (specs/scala2-facade.md, stage 10) */
class TestUiFromScala2 extends munit.FunSuite {
  import UiModel._

  test("the loop folds the events, draws each changed view, and answers the final state") {
    val host = ScriptedHost(Event.Pressed("inc"), Event.Pressed("inc"), Event.Pressed("dec"), Event.Pressed("nope"))
    assertEquals(UiApp.run(0)(view)(update)(host.host).runWith, 1)
    assertEquals(host.frames, Vector(view(0), view(1), view(2), view(1)))
  }

  test("a frame is plain text, so what the user sees is a string") {
    val text = Frame.render(view(3)).mkString("\n")
    assert(text.contains("count: 3"), text)
    assert(text.contains("+"), text)
  }

  test("events from the world merge in beside the user's, and can end the loop") {
    val host = ScriptedHost.open()
    // Source[Event], not Source(...) alone: from Scala 2, a Scala 3 enum
    // case's constructor is typed as the CASE (Event.Pressed), not
    // widened to the enum as Scala 3 does, and Source is invariant
    val ticks = Source[Event](Event.Pressed("inc"), Event.Pressed("inc"), Event.Pressed("inc"), Event.Closed)
    assertEquals(UiApp.runWith(0)(view)(update)(host.host, ticks).runWith, 3)
    assertEquals(host.frames.head, view(0))
  }
}
