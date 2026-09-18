package okay.ui

import okay.codec.{Json, Schema, Validate}

/**
 * form-errors-on-validate: what the two walks actually SAY, side by
 * side, on one form's schema — the evidence the backlog entry asks for
 * before `Form.errors` is made `Validate.errors` with a wording.
 *
 * Two things decide it, and neither is taste: whether the PATHS are
 * the keys a form can render under, and whether the MESSAGES are ones
 * an analyst should read.
 *
 *   sbt "okayUiJVM/Test/runMain okay.ui.FormErrorsProbe"
 */
object FormErrorsProbe:

  enum Disposition derives Schema:
    case Suspicious, Dismissed

  final case class Address(city: String, zip: String) derives Schema
  final case class Close(disposition: Disposition, note: String = "",
                         address: Option[Address] = None, tags: Vector[String] = Vector.empty) derives Schema

  private def show(name: String, es: Vector[(String, String)]): Unit =
    println(s"  $name:")
    if es.isEmpty then println("    (none)")
    else es.foreach((p, m) => println(f"    ${if p.isEmpty then "<form>" else p}%-22s $m"))

  def main(args: Array[String]): Unit =
    val cases = Vector(
      "the empty form (nothing filled in)" -> Json.JObj(Vector.empty),
      "a chosen case, nothing else" -> Json.JObj(Vector(
        "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty))))),
      "an option half filled (the city typed, the zip not)" -> Json.JObj(Vector(
        "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty))),
        "note" -> Json.JStr("x"),
        "address" -> Json.JObj(Vector("city" -> Json.JStr("Kyiv"))))),
      "a list with a damaged element" -> Json.JObj(Vector(
        "disposition" -> Json.JObj(Vector("Suspicious" -> Json.JObj(Vector.empty))),
        "note" -> Json.JStr("x"),
        "tags" -> Json.JArr(Vector(Json.JStr("ok"), Json.JBool(true))))))
    for (name, value) <- cases do
      println(s"-- $name")
      show("Form.errors    ", Form.errors[Close](value))
      show("Validate.errors", Validate.errors(summon[Schema[Close]])(value))
    // and the keys the FORM actually renders under: an error whose
    // path is not one of these renders nowhere
    val keys = Ui.focusable(Form.of[Close](Json.JObj(Vector.empty))).flatMap {
      case Ui.Input(_, k, _, _, _) => Some(k)
      case Ui.Check(_, k, _) => Some(k)
      case Ui.Select(_, _, k) => Some(k)
      case Ui.Button(_, k, _) => Some(k)
      case _ => None
    }
    println(s"-- the form's own keys: ${keys.mkString(", ")}")
