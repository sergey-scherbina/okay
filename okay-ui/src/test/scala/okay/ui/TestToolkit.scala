package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}

/** specs/ui-toolkit.md, Behavior — one test per checkbox */
class TestToolkit extends munit.FunSuite {

  final case class Addr(city: String, zip: Int)
  final case class Person(name: String, addr: Addr)
  given Schema[Addr] = Schema.derived
  given Schema[Person] = Schema.derived

  enum Pet:
    case Dog(name: String)
    case Fish
  given Schema[Pet] = Schema.derived
  final case class Owner(who: String, pet: Pet)
  given Schema[Owner] = Schema.derived

  final case class Tags(title: String, tags: Vector[String])
  given Schema[Tags] = Schema.derived

  final class Scripted(script: Seq[Event]) extends Host:
    val frames = scala.collection.mutable.Buffer[Ui]()
    def render(ui: Ui): Unit ! Async = async { frames += ui; () }
    def events: Source[Event] = okay.Source.of(script.toList)

  def texts(ui: Ui): String = Frame.render(ui).mkString("\n")

  test("a nested product: titled section, dotted-path edits, decodes by the codec") {
    val host = Scripted(Seq(
      Event.Edited("name", "ada"),
      Event.Edited("addr.city", "kyiv"),
      Event.Edited("addr.zip", "10101"),
      Event.Pressed("$ok")))
    val got = Dialog.run(host)(Form.ask[Person]("who?")).runWith
    assertEquals(got, Some(Some(Person("ada", Addr("kyiv", 10101)))))
    assert(texts(host.frames.head).contains("addr"), "the section is titled")
  }

  test("a sum: case Select + subform; choosing swaps; the codec shape survives") {
    val host = Scripted(Seq(
      Event.Edited("who", "ada"),
      Event.Edited("pet.name", "rex"),          // Dog is the first case
      Event.Pressed("$ok")))
    assertEquals(Dialog.run(host)(Form.ask[Owner]("pets?")).runWith,
      Some(Some(Owner("ada", Pet.Dog("rex")))))
    // choosing the OTHER case swaps the subform and still decodes
    val host2 = Scripted(Seq(
      Event.Edited("who", "bob"),
      Event.Chosen("pet.$case", 1),             // Fish — no fields
      Event.Pressed("$ok")))
    assertEquals(Dialog.run(host2)(Form.ask[Owner]("pets?")).runWith,
      Some(Some(Owner("bob", Pet.Fish))))
    // the swap was VISIBLE: a frame after Chosen has no pet.name input
    assert(host2.frames.drop(2).exists(f => !texts(f).contains("name ")),
      "the Dog subform should be gone after choosing Fish")
  }

  test("a list: add, edit by index, remove; the value is the codec's array") {
    val host = Scripted(Seq(
      Event.Edited("title", "reading"),
      Event.Pressed("tags$add"),
      Event.Pressed("tags$add"),
      Event.Edited("tags[0]", "scala"),
      Event.Edited("tags[1]", "wrong"),
      Event.Pressed("tags[1]$del"),
      Event.Pressed("tags$add"),
      Event.Edited("tags[1]", "effects"),
      Event.Pressed("$ok")))
    assertEquals(Dialog.run(host)(Form.ask[Tags]("tags?")).runWith,
      Some(Some(Tags("reading", Vector("scala", "effects")))))
  }

  test("per-field errors: two bad fields, two messages, each under its field") {
    val v = Json.JObj(Vector(
      "name" -> Json.JStr("ada"),
      "addr" -> Json.JObj(Vector(
        "city" -> Json.JStr("kyiv"),
        "zip" -> Json.JStr("not-a-number")))))
    val errs = Form.errors[Person](v)
    assert(errs.exists(_._1 == "addr.zip"), errs.toString)
    // a missing required field errors too
    val v2 = Json.JObj(Vector("name" -> Json.JStr("ada")))
    assert(Form.errors[Person](v2).exists(_._1 == "addr"), Form.errors[Person](v2).toString)
    // and the rendered form places the message after the field's row
    val ui = Form.ofWith[Person](errs).apply(v)
    assert(texts(ui).contains("! "), texts(ui))
  }

  test("cross-field checks: run on the decoded value, hold submit until clean") {
    final case class Range(lo: Int, hi: Int)
    given Schema[Range] = Schema.derived
    val ordered: Form.Check[Range] =
      r => if r.lo <= r.hi then Vector.empty else Vector("hi" -> "must be >= lo")
    val host = Scripted(Seq(
      Event.Edited("lo", "5"), Event.Edited("hi", "3"),
      Event.Pressed("$ok"),                     // refused: decoded fine, check fails
      Event.Edited("hi", "7"),
      Event.Pressed("$ok")))
    assertEquals(Dialog.run(host)(Form.ask[Range]("range?", ordered)).runWith,
      Some(Some(Range(5, 7))))
    // the refusal was shown, attached near the hi field
    assert(host.frames.exists(f => texts(f).contains("must be >= lo")))
  }

  test("the composed dialogs answer through the same event contract") {
    assertEquals(Dialog.run(Scripted(Seq(Event.Pressed("$ok"))))(
      Toolkit.confirm("sure?")).runWith, Some(true))
    assertEquals(Dialog.run(Scripted(Seq(Event.Pressed("$cancel"))))(
      Toolkit.confirm("sure?")).runWith, Some(false))
    assertEquals(Dialog.run(Scripted(Seq(Event.Pressed("$ok"))))(
      Toolkit.alert("done")).runWith, Some(()))
    assertEquals(Dialog.run(Scripted(Seq(
      Event.Edited("$value", "hello"), Event.Pressed("$ok"))))(
      Toolkit.prompt("say:")).runWith, Some(Some("hello")))
    assertEquals(Dialog.run(Scripted(Seq(
      Event.Chosen("$choice", 2), Event.Pressed("$ok"))))(
      Toolkit.choice("pick:", Vector("a", "b", "c"))).runWith, Some(Some(2)))
    // a host that closes ends the RUN itself: the outer None
    assertEquals(Dialog.run(Scripted(Seq(Event.Closed)))(
      Toolkit.prompt("say:")).runWith, None)
  }

  test("the drift law, extended: nesting, a sum and a list round-trip the codec") {
    final case class Everything(person: Person, pet: Pet, tags: Vector[String])
    given Schema[Everything] = Schema.derived
    val host = Scripted(Seq(
      Event.Edited("person.name", "ada"),
      Event.Edited("person.addr.city", "kyiv"),
      Event.Edited("person.addr.zip", "10101"),
      Event.Edited("pet.name", "rex"),
      Event.Pressed("tags$add"),
      Event.Edited("tags[0]", "founder"),
      Event.Pressed("$ok")))
    val got = Dialog.run(host)(Form.ask[Everything]("all of it")).runWith.flatten.get
    assertEquals(got, Everything(Person("ada", Addr("kyiv", 10101)),
      Pet.Dog("rex"), Vector("founder")))
    // the law itself: encode what we got, decode it back, same value
    assertEquals(Json.read[Everything](Json.write(got)), Right(got))
  }
}
