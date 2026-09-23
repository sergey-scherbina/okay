package scala2probe

import okay.codec.Schema
import okay.scala2._
import okay.ui.{Event, Frame, Ui}

object FormModel {
  final case class Signup(name: String, age: Int, newsletter: Boolean)
  object Signup {
    implicit val schema: Schema[Signup] =
      Schemas.product3("Signup", "name", "age", "newsletter")(Signup.apply)(s => (s.name, s.age, s.newsletter))
  }
}

/** okay-ui's Form from Scala 2.13 (specs/scala2-facade.md, stage 11) */
class TestFormFromScala2 extends munit.FunSuite {
  import FormModel._

  test("a blank form is not a value yet, and says which fields are missing") {
    val form = FormState.blank[Signup]
    assert(form.decoded.isLeft, form.decoded.toString)
    assert(form.errors.map(_._1).contains("name"), form.errors.toString)
  }

  test("edits fold into the value; once complete it decodes") {
    val filled = FormState.blank[Signup]
      .edit(Event.Edited("name", "Ada"))
      .edit(Event.Edited("age", "36"))
      .edit(Event.Toggled("newsletter", true))
    assertEquals(filled.errors, Vector.empty)
    assertEquals(filled.decoded, Right(Signup("Ada", 36, newsletter = true)))
  }

  test("a form filled from a value decodes back to it, and draws it") {
    val form = FormState.of(Signup("Bob", 7, newsletter = false))
    assertEquals(form.decoded, Right(Signup("Bob", 7, newsletter = false)))
    val text = Frame.render(form.view).mkString("\n")
    assert(text.contains("Bob"), text)
  }

  test("the form is the state of a UiApp loop") {
    val host = ScriptedHost(Event.Edited("name", "Cy"), Event.Edited("age", "5"))
    val done = Eff.runAsync(UiApp.run(FormState.blank[Signup])(_.view)(_.edit(_))(host.host))
    assertEquals(done.decoded, Right(Signup("Cy", 5, newsletter = false)))
    assert(host.frames.size >= 2, host.frames.size.toString)
  }

  test("labels name the fields for a human") {
    val form = FormState.blank[Signup].withLabels(Map("name" -> "Your name"))
    val text = Frame.render(form.view).mkString("\n")
    assert(text.contains("Your name"), text)
    assert(Ui.focusable(form.view).nonEmpty)
  }
}
