package okay.ui

import okay.*
import okay.given
import okay.Direct.*
import okay.codec.Schema

/** ui-direct: the direct wizard, askWith's three outcomes, the
 * ambient-Host doors — one test per promise */
class TestUiDirect extends munit.FunSuite {

  final case class Name(first: String)
  final case class Age(years: Int)
  given Schema[Name] = Schema.derived
  given Schema[Age] = Schema.derived

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  type D = [A] =>> A ! Dialog

  test("the direct wizard: a flatMap chain reads as straight-line code") {
    // the same two-step wizard TestDialog writes with flatMaps —
    // here as plain vals under the PREFIX mark: a program of type
    // A ! F collapses under its own type's symbol (`.?` is ambiguous
    // on rows — Effects has its own row-`?`; postfix `.!` shadowed
    // the object `!`; the prefix carries a different name and reads
    // as "perform")
    val wizard: (Option[Name], Option[Age]) ! Dialog = direct[D] {
      val name = !Form.ask[Name]("who are you?")
      val age = !Form.ask[Age]("how old?")
      (name, age)
    }
    val host = Scripted(Seq(
      Event.Edited("first", "ada"), Event.Pressed("$ok"),
      Event.Edited("years", "36"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[(Option[Name], Option[Age])], Pure](
      Dialog.run(host)(wizard)).runWith,
      Some((Some(Name("ada")), Some(Age(36)))))
  }

  test("askWith: the forgiving policy IS ask; patience gives up; a valid submit never asks") {
    // forgiving: invalid then corrected — same as ask
    val h1 = Scripted(Seq(
      Event.Edited("years", "nope"), Event.Pressed("$ok"),
      Event.Edited("years", "36"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[Option[Age]], Pure](
      Dialog.run(h1)(Form.askWith[Age]("age?")(Form.forgiving))).runWith,
      Some(Some(Age(36))))
    // patience(1): the first invalid submit gives up — None
    val h2 = Scripted(Seq(
      Event.Edited("years", "nope"), Event.Pressed("$ok"),
      Event.Edited("years", "36"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[Option[Age]], Pure](
      Dialog.run(h2)(Form.askWith[Age]("age?")(Form.patience(1)))).runWith,
      Some(None))
    // a repairing policy resumes with a forced value at the signal point
    val h3 = Scripted(Seq(Event.Edited("years", "nope"), Event.Pressed("$ok")))
    val force: (Any, Vector[String]) => okay.Condition.Decision =
      case (_: Form.InvalidSubmit, _) => okay.Condition.Decision.Resume(Age(99))
      case _ => okay.Condition.Decision.Fail
    assertEquals(Async.run[Option[Option[Age]], Pure](
      Dialog.run(h3)(Form.askWith[Age]("age?")(force))).runWith,
      Some(Some(Age(99))))
    // a VALID submit never consults the policy
    val h4 = Scripted(Seq(Event.Edited("years", "36"), Event.Pressed("$ok")))
    val paranoid: (Any, Vector[String]) => okay.Condition.Decision =
      (_, _) => throw new AssertionError("must not be consulted")
    assertEquals(Async.run[Option[Option[Age]], Pure](
      Dialog.run(h4)(Form.askWith[Age]("age?")(paranoid))).runWith,
      Some(Some(Age(36))))
  }

  test("the ambient-Host doors: hosted equals run(host)") {
    given Host = Scripted(Seq(Event.Edited("first", "ada"), Event.Pressed("$ok")))
    assertEquals(Async.run[Option[Option[Name]], Pure](
      Dialog.hosted(Form.ask[Name]("who?"))).runWith, Some(Some(Name("ada"))))
  }
}
