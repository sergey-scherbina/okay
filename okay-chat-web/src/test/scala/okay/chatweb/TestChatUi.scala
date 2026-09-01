package okay.chatweb

import okay.ui.{Event, Frame, Ui}
import ChatUi.*

/** the React frontend's brain, tested where brains are tested: on
 * the JVM, with scripted events — no browser anywhere */
class TestChatUi extends munit.FunSuite {

  def text(ui: Ui): String = Frame.render(ui).mkString("\n")

  test("a send opens the turn: history captured, draft cleared, busy") {
    val s0 = State()
    val (s1, _) = update(s0, Event.Edited("draft", "hello"))
    val (s2, go) = update(s1, Event.Pressed("send"))
    assertEquals(go, Go.Send(Vector(Msg("user", "hello"))))
    assertEquals(s2.draft, "")
    assert(s2.busy)
    assertEquals(s2.messages.map(_.role), Vector("user", "assistant"))
    // a second send while busy is refused by the fold itself
    val (s3, go3) = update(s2.copy(draft = "again"), Event.Pressed("send"))
    assertEquals(go3, Go.Stay)
    assertEquals(s3.messages.length, 2)
  }

  test("tokens append to the open bubble; done closes; the view shows it all") {
    var s = update(update(State(), Event.Edited("draft", "hi"))._1, Event.Pressed("send"))._1
    for t <- Vector("Hello", " ", "there") do
      s = update(s, Event.Edited("$token", t))._1
    s = update(s, Event.Pressed("$done"))._1
    assertEquals(s.messages.last.text, "Hello there")
    assert(!s.busy)
    assert(text(view(s)).contains("bot: Hello there"))
  }

  test("a cut closes the turn and the view renders the scissors line") {
    var s = update(update(State(), Event.Edited("draft", "x"))._1, Event.Pressed("send"))._1
    s = update(s, Event.Edited("$token", "partial"))._1
    s = update(s, Event.Edited("$cut", """{"rule":"token-budget"}"""))._1
    assert(!s.busy)
    assertEquals(s.messages.last.cut.isDefined, true)
    assert(text(view(s)).contains("generation cut"))
  }
}
