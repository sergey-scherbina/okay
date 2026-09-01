package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}

/**
 * Scenarios are programs: a wizard is show-await-validate-branch with
 * retry as recursion — run standalone over a host, and run UNCHANGED
 * as a screen inside the Elm loop, because the continuation is just
 * state.
 */
class TestDialog extends munit.FunSuite {

  final case class Name(first: String)
  final case class Age(years: Int)
  given Schema[Name] = Schema.derived
  given Schema[Age] = Schema.derived

  /** the wizard: two typed forms, the second retried until it parses */
  def wizard: Option[(Name, Age)] ! Dialog =
    Form.ask[Name]("who are you?").flatMap {
      case None => pure(None)
      case Some(n) => Form.ask[Age]("how old?").map(_.map(a => (n, a)))
    }

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  val fill: Seq[Event] = Seq(
    Event.Edited("first", "ada"), Event.Pressed("$ok"),
    Event.Edited("years", "not a number"), Event.Pressed("$ok"),  // refused, shown
    Event.Edited("years", "36"), Event.Pressed("$ok"))

  test("a wizard is a program; an invalid step retries itself") {
    val host = Scripted(fill)
    assertEquals(Dialog.run(host)(wizard).runWith, Some(Some((Name("ada"), Age(36)))))
    // the refusal was SHOWN: some frame carries the error line
    assert(host.frames.exists(f => Frame.render(f).exists(_.contains("! "))),
      host.frames.map(Frame.render(_)).mkString("\n"))
  }

  test("cancel answers None; a host that ends early answers None outright") {
    assertEquals(Dialog.run(Scripted(Seq(Event.Pressed("$cancel"))))(wizard).runWith,
      Some(None))
    assertEquals(Dialog.run(Scripted(Nil))(wizard).runWith, None)
    assertEquals(Dialog.run(Scripted(Seq(Event.Closed)))(wizard).runWith, None)
  }

  test("the SAME scenario runs inside Ui.run: the continuation is the state") {
    val done = (a: Option[(Name, Age)]) => Ui.Text(s"done: $a")
    val host = Scripted(fill ++ Seq(Event.Closed))
    val finished = Ui.run(Dialog.start(wizard))(Dialog.view(done))(Dialog.update)(host).runWith
    finished match
      case Dialog.Running.Done(a) => assertEquals(a, Some((Name("ada"), Age(36))))
      case other => fail(s"the scenario did not finish: $other")
    // and the loop showed the done screen
    assertEquals(Frame.render(host.frames.last), Vector("done: Some((Name(ada),Age(36)))"))
  }
}
