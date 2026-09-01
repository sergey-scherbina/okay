package okay.ui

import okay.*
import okay.given
import PWizard.*

/** specs/ui-toolkit.md, "The typed wizard" — one test per box */
class TestPWizard extends munit.FunSuite {

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  // the two steps, each NAMING its state requirement in its type:
  // askName needs nothing, askAge needs the name already collected
  def askName[R]: Step[Unit, Unit, String, R] =
    step((_: Unit) => Ui.Input("", "name", "who are you?")) {
      case (_, Event.Edited("name", v)) if v.nonEmpty => Some(v)
      case _ => None
    }

  def askAge[R]: Step[Unit, String, (String, Int), R] =
    step((name: String) => Ui.Column(Vector(
      Ui.Text(s"hello, $name"),                    // the view SEES the typed state-so-far
      Ui.Input("", "age", "how old?")))) {
      case (name, Event.Edited("age", v)) => v.toIntOption.map(name -> _)
      case _ => None
    }

  def wizard[R]: Cont[Unit, ((String, Int)) => Machine[R], Unit => Machine[R]] =
    askName.flatMap(_ => askAge)

  test("the typed wizard collects through a growing state; validation retries") {
    var m = run(())(wizard)
    def feed(e: Event): Unit = m match
      case Machine.Showing(_, resume) => m = resume(e)
      case _ => ()
    feed(Event.Edited("name", ""))          // refused by the fold: still asking
    assert(m.isInstanceOf[Machine.Showing[?]])
    feed(Event.Edited("name", "ada"))
    m match
      case Machine.Showing(ui, _) =>
        assert(Frame.render(ui).exists(_.contains("hello, ada")),
          "the second step's view reads the typed state")
      case other => fail(s"expected the age step, got $other")
    feed(Event.Edited("age", "not a number"))  // refused
    feed(Event.Edited("age", "36"))
    assertEquals(m, Machine.Done((("ada", 36), ())))
  }

  test("the bridge: the same wizard runs as an ordinary Dialog program") {
    val host = Scripted(Seq(
      Event.Edited("name", "ada"),
      Event.Edited("age", "36")))
    val got = Async.run[Option[((String, Int), Unit)], Pure](
      Dialog.run(host)(toDialog(run(())(wizard)))).runWith
    assertEquals(got, Some((("ada", 36), ())))
  }

  test("misordering is a TYPE error: age before name does not compile") {
    val errors = compileErrors(
      "import okay.ui.PWizard.*\n" +
      "def bad[R] = TestPWizardSteps.askAge[R].flatMap(_ => TestPWizardSteps.askName[R])\n" +
      "PWizard.run(())(bad)")
    assert(errors.nonEmpty && errors.contains("Found"), errors)
  }
}

/** the steps, visible to the compileErrors probe */
object TestPWizardSteps {
  import PWizard.*
  def askName[R]: Step[Unit, Unit, String, R] =
    step((_: Unit) => Ui.Text("who?")) { case _ => Some("x") }
  def askAge[R]: Step[Unit, String, (String, Int), R] =
    step((_: String) => Ui.Text("age?")) { case (n, _) => Some((n, 1)) }
}
