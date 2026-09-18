package okay.ui

import okay.codec.{Json, Schema}

/**
 * form-errors-on-validate (BACKLOG, answered 2026-09-18): the entry
 * asked whether `Form.errors` should become `Validate.errors` with the
 * form's wording. Comparing the two on one schema (FormErrorsProbe)
 * found the question was not about wording at all — it was about two
 * places where the form's answer was WRONG, and the accumulating
 * decoder's was right. This suite is those two.
 */
class TestFormErrors extends munit.FunSuite {

  enum Disposition derives Schema:
    case Suspicious, Dismissed

  final case class Address(city: String, zip: String) derives Schema
  final case class Close(disposition: Disposition, note: String = "",
                         address: Option[Address] = None,
                         tags: Vector[String] = Vector.empty) derives Schema

  private val chosen = Json.JObj(Vector(
    "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty)))))

  test("a field the SCHEMA defaults is not 'required' — the form refused what the decoder accepts") {
    // `note` and `tags` have defaults, so this value decodes; the form
    // said "required" for both and held the submit, forever, for a
    // field the user cannot see is empty
    assert(Form.decode[Close](chosen).isRight, Form.decode[Close](chosen).toString)
    assertEquals(Form.errors[Close](chosen), Vector.empty)
  }

  test("an error inside an Option lands on the FIELD, not on the option — or it renders nowhere") {
    val half = Json.JObj(Vector(
      "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty))),
      "address" -> Json.JObj(Vector("city" -> Json.JStr("Kyiv")))))
    val es = Form.errors[Close](half)
    assertEquals(es.length, 1, es.toString)
    val (path, _) = es.head
    // the keys the form actually renders under
    val keys = Ui.focusable(Form.of[Close](half)).collect {
      case Ui.Input(_, k, _, _, _) => k
      case Ui.Check(_, k, _) => k
      case Ui.Select(_, _, k) => k
    }
    assert(keys.contains(path), s"the error at '$path' renders under none of: ${keys.mkString(", ")}")
    assertEquals(path, "address.zip")
  }

  test("what was already right stays right: a missing required field, and a damaged list element") {
    assertEquals(Form.errors[Close](Json.JObj(Vector.empty)).map(_._1), Vector("disposition"))
    val damaged = Json.JObj(Vector(
      "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty))),
      "tags" -> Json.JArr(Vector(Json.JStr("ok"), Json.JBool(true)))))
    assertEquals(Form.errors[Close](damaged).map(_._1), Vector("tags[1]"))
  }
}
