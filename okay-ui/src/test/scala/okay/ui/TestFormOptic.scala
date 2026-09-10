package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, JsonOptic, Schema}

/**
 * specs/optics.md stage 1, the fourth item, decided by what the code
 * says. `Form.edit` was to be REWRITTEN on the optic path. It is not,
 * and this file is the reason and the guard.
 *
 * `Form.edit` does two things the optic path does not and must not:
 * it CREATES missing parents on the way down (`get(value, n)
 * .getOrElse(empty(fs()))`), and it interprets the Edit at the leaf
 * (a text into a JNum by the field's schema, Add growing a list, Del
 * shrinking it, Choose swapping a sum's case). The first is the
 * unlawful lens `JsonOptic` deliberately does not have — a lens that
 * creates a missing field breaks GetPut — and the second is not
 * navigation at all. A rewrite would have had to smuggle both into an
 * optic, and the optics would have stopped being lawful to make a
 * router shorter.
 *
 * What IS true, and is asserted here: where both are defined — the
 * parents present — `Form.edit` touches EXACTLY the focus
 * `JsonOptic.path` names, and nothing else. That is the statement the
 * rewrite was after, and it holds without one.
 *
 * REFINED 2026-09-10 (specs/optics.md stage 6). The FIRST of those two
 * reasons has dissolved: creating a missing parent is unlawful only
 * while absence is a case the setter handles behind the caller's back.
 * `Iso.non` puts absence in the focus, `JsonOptic.creating` composes
 * it down a path, and the last test here shows that path producing
 * EXACTLY what `Form.edit` produces, defaults and all. The second
 * reason stands and is why Form still keeps its router: interpreting
 * a text by the field's schema, growing a list, swapping a sum's
 * case are not navigation, and no optic should pretend they are.
 */
class TestFormOptic extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Address, tags: Vector[String])
  given Schema[Address] = Schema.derived
  given Schema[Person] = Schema.derived

  val ada = Person("ada", 36, Address("Warszawa", 12345), Vector("a", "b"))
  def enc[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))

  test("Form.edit touches exactly the focus the optic path names, and nothing else") {
    val schema = summon[Schema[Person]]
    val before = enc(ada)
    for (key, event) <- Vector(
      ("name", Event.Edited("name", "bob")),
      ("age", Event.Edited("age", "7")),
      ("address.city", Event.Edited("address.city", "Kraków")),
      ("address.zip", Event.Edited("address.zip", "99")),
      ("tags[1]", Event.Edited("tags[1]", "B")))
    do
      val after = Form.edit[Person](before, event)
      val optic = JsonOptic.path(schema, key).getOrElse(fail(s"the optic path does not know $key"))
      val focusAfter = optic.preview(after).getOrElse(fail(s"nothing at $key after the edit"))
      // the edit is the optic's set of whatever Form put there: which
      // says the edit reached that focus and left the rest alone
      assertEquals(optic.set(focusAfter)(before), after, s"Form.edit moved something outside $key")
      // and the focus really did change
      assertNotEquals(optic.preview(before), Some(focusAfter), s"the edit at $key changed nothing")
  }

  test("the two disagree exactly where the optic is honest: a missing parent") {
    // a partial value — the shape a form starts from — whose parent
    // object is not there yet: Form CREATES it, the optic refuses.
    // That refusal is the unlawful lens JsonOptic does not have.
    val schema = summon[Schema[Person]]
    val partial = Json.JObj(Vector("name" -> Json.JStr("ada")))
    val edited = Form.edit[Person](partial, Event.Edited("address.city", "Kraków"))
    assert(edited != partial, "Form creates the missing parent")
    assertEquals(JsonOptic.path(schema, "address.city").flatMap(_.preview(partial)), None,
      "the optic previews nothing where the parent is missing — which is why Form keeps its router")
    // once the parent is there, they agree again, exactly
    val seeded = Form.edit[Person](partial, Event.Edited("address.city", "old"))
    val after = Form.edit[Person](seeded, Event.Edited("address.city", "Kraków"))
    val optic = JsonOptic.path(schema, "address.city").get
    assertEquals(optic.set(Json.JStr("Kraków"))(seeded), after)
  }

  test("the creating path IS what Form does, once absence is part of the focus") {
    // Form's own default for a missing product is `JObj(Vector.empty)`
    // (Form.empty), and that is the `non` default here. With the same
    // defaults, the lawful composition and the hand-written router
    // agree to the byte — which is what the stage-1 rewrite wanted and
    // could not have while the creation lived inside `set`.
    val partial = Json.JObj(Vector("name" -> Json.JStr("ada")))
    val viaForm = Form.edit[Person](partial, Event.Edited("address.city", "Kraków"))
    val viaOptic = JsonOptic.creating(List(
      ("address", Json.JObj(Vector.empty)),
      ("city", Json.JStr("")))).set(Json.JStr("Kraków"))(partial)
    assertEquals(viaOptic, viaForm)

    // two levels of absence, the same way
    val bare = Json.JObj(Vector.empty)
    assertEquals(
      JsonOptic.creating(List(("address", Json.JObj(Vector.empty)), ("zip", Json.JNum(0))))
        .set(Json.JNum(99))(bare),
      Form.edit[Person](bare, Event.Edited("address.zip", "99")))
  }
}
