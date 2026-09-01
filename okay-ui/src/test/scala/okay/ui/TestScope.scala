package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import Scope.*

/** specs/ui-toolkit.md, "Dialog scopes" — one test per box */
class TestScope extends munit.FunSuite {

  final case class Name(first: String)
  final case class Age(years: Int)
  given Schema[Name] = Schema.derived
  given Schema[Age] = Schema.derived

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  /** a sub-flow of TWO steps with no Option threading between them:
   * a step that answers None cancels the whole scope in one line */
  def subFlow: Option[(Name, Age)] ! Dialog =
    scoped[Option[(Name, Age)]] { p =>
      for
        n <- lift(Form.ask[Name]("who?")).flatMap {
          case Some(n) => pure(n)
          case None => cancel(p)(None)     // exits the SCOPE, not the step
        }
        a <- lift(Form.ask[Age]("age?")).flatMap {
          case Some(a) => pure(a)
          case None => cancel(p)(None)
        }
      yield Some((n, a))
    }

  test("a scope cancels as a unit: no Option threading between its steps") {
    // the happy path
    val ok = Scripted(Seq(
      Event.Edited("first", "ada"), Event.Pressed("$ok"),
      Event.Edited("years", "36"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[Option[(Name, Age)]], Pure](
      Dialog.run(ok)(subFlow)).runWith, Some(Some((Name("ada"), Age(36)))))
    // cancel at the SECOND step aborts the whole scope with one line
    val bail = Scripted(Seq(
      Event.Edited("first", "ada"), Event.Pressed("$ok"),
      Event.Pressed("$cancel")))
    assertEquals(Async.run[Option[Option[(Name, Age)]], Pure](
      Dialog.run(bail)(subFlow)).runWith, Some(None))
  }

  test("multi-prompt: an inner scope aborts ACROSS its boundary to the outer one") {
    // outer collects a report; inner is a sub-questionnaire. "abort all"
    // from INSIDE the inner scope exits the outer directly — capturing
    // across the intervening delimiter, which is the multi-prompt point.
    def flow: String ! Dialog =
      scoped[String] { outer =>
        for
          _ <- push[String] { inner =>
            lift(Dialog.show(Ui.Button("inner", "step"))).flatMap {
              case Event.Pressed("$abort-all") => cancel(outer)("aborted-everything")
              case Event.Pressed("$abort-inner") => cancel(inner)("inner-aborted")
              case _ => pure("inner-done")
            }
          }
          _ <- lift(Dialog.show(Ui.Button("outer", "step")))
        yield "completed"
      }

    // inner abort: the outer continues to its own step
    val innerBail = Scripted(Seq(Event.Pressed("$abort-inner"), Event.Pressed("x")))
    assertEquals(Async.run[Option[String], Pure](
      Dialog.run(innerBail)(flow)).runWith, Some("completed"))
    // abort-all: the OUTER answers immediately; its second step never shows
    val all = Scripted(Seq(Event.Pressed("$abort-all")))
    assertEquals(Async.run[Option[String], Pure](
      Dialog.run(all)(flow)).runWith, Some("aborted-everything"))
    assertEquals(all.frames.length, 1, "the outer step must not have rendered")
  }

  test("Dialog itself is untouched: a plain scenario still runs beside the scoped one") {
    val host = Scripted(Seq(Event.Edited("first", "ada"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[Option[Name]], Pure](
      Dialog.run(host)(Form.ask[Name]("who?"))).runWith, Some(Some(Name("ada"))))
  }
}
