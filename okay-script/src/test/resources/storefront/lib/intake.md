---
exports:
  - Intake
  - Delivery
  - Payment
  - checks
  - blank
route: false
---

# the intake, as a TYPE

What a customer tells the atelier, once: the same `Schema` renders
the form, reads the post back and validates it, so a field cannot
exist on one side and not the other. The hand-written sheet on the
front page is the other road — fast, no round trip; this one is
typed.

```scala declare
import okay.codec.{Json, Schema}
import okay.ui.{Event, Form, Ui}

enum Delivery derives Schema:
  case Post, InPerson

enum Payment derives Schema:
  case Transfer, OnDelivery

/** `itemValue` is OPTIONAL and numeric, and both are the TYPE's job:
 * a required `String` cannot be left empty (an empty text is not
 * posted at all, so the decode refuses before any check runs -- found
 * by driving the form in a browser, where a visitor leaves it blank),
 * and "a number" said as `Int` needs no rule of mine. */
final case class Intake(need: String, delivery: Delivery, payment: Payment,
                        itemValue: Option[Int], name: String, contact: String,
                        consent: Boolean) derives Schema

/** what a form cannot say about itself: the cross-field rules. A
 * consent that is merely PRESENT is not consent — it must be given,
 * and that is a check rather than a type, because the form must be
 * able to render the unchecked state. */
val checks: Vector[Form.Check[Intake]] = Vector(
  i => if i.need.trim.nonEmpty then Vector.empty else Vector("need" -> "powiedz, co trzeba zrobić"),
  i => if i.contact.trim.nonEmpty then Vector.empty else Vector("contact" -> "zostaw kontakt"),
  i => if i.consent then Vector.empty else Vector("consent" -> "bez zgody nie mogę odpowiedzieć"),
)

/** the value a fresh form starts from: every choice on its first
 * option and every check false -- a submit that changed nothing must
 * still decode (the same rule the analyst page learnt) */
def blank: Json =
  val start = Json.JObj(Vector.empty)
  Ui.focusable(Form.of[Intake](start)).foldLeft(start: Json) { (j, u) =>
    u match
      case Ui.Select(_, _, k) => Form.edit[Intake](j, Event.Chosen(k, 0))
      case Ui.Check(_, k, _) => Form.edit[Intake](j, Event.Toggled(k, false))
      case _ => j
  }
```
