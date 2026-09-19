package okay.ui

import okay.codec.{Json, Schema}

/**
 * form-blank (specs/ui-product.md stage 3): the value a form STARTS
 * from, in one place.
 *
 * The defect this suite was written for, mechanism first. A form's
 * `Select` always SHOWS an option — a sum's case knob shows the first
 * case — but the value behind it holds nothing until something is
 * chosen. On the scriptless road `Html.events` sends an event only for
 * a field whose posted value DIFFERS from the shown one, so a user who
 * fills the text fields and submits sends nothing at all about the
 * Select, and the submission is refused for a field they can see is
 * filled in. The live road does not have it (`live.js` and `Ui.submit`
 * collect every field's current value), which is why okay-watch found
 * it and okay-script did not.
 */
class TestFormBlank extends munit.FunSuite {

  enum Kind derives Schema:
    case Wire, Card

  final case class Payment(who: String, kind: Kind, urgent: Boolean) derives Schema

  private val FormKey = "pay"

  /** the app as a plain-road page: the form, and the state it folds */
  private def view(v: Json): Ui =
    val fields = Form.of[Payment](v) match
      case c: Ui.Column => c.children
      case other => Vector(other)
    Ui.Form(fields, "Save", FormKey)

  /** what a browser posts: every named field it holds, plus the press.
   * A Select posts the option TEXT it is showing, which is exactly
   * what the shown tree says it shows — so `Html.events` sees no
   * change and says nothing about it. */
  private def post(v: Json, typed: Map[String, String]): Json =
    val shown = view(v)
    val fields = Ui.focusable(shown).collect {
      case Ui.Input(value, k, _, _, _) => k -> typed.getOrElse(k, value)
      case Ui.Select(os, i, k) => k -> typed.getOrElse(k, os.lift(i).getOrElse(""))
    }.toMap + (Html.PressField -> FormKey)
    Html.events(shown, fields).foldLeft(v) { (acc, e) =>
      e match
        case Event.Submitted(_, _) => Form.submitted[Payment](acc, e)
        case other => Form.edit[Payment](acc, other)
    }

  test("THE DEFECT: starting from the empty object, a submit that touched no Select is refused") {
    val empty = Json.JObj(Vector.empty)
    // the form SHOWS the first case and an unticked box
    assertEquals(Ui.focusable(view(empty)).collect { case Ui.Select(os, i, _) => os(i) }, Vector("Wire"))
    val sent = post(empty, Map("who" -> "Ada"))
    // and the value behind them holds neither
    val errs = Form.errors[Payment](sent).map(_._2)
    assert(errs.nonEmpty, s"expected the old refusal, got a clean form: $sent")
    assert(Form.decode[Payment](sent).isLeft, s"decoded what it should not: $sent")
  }

  test("Form.blank is what the tree already shows: every Select on its first option, every Check false") {
    val blank = Form.blank[Payment]
    val sent = post(blank, Map("who" -> "Ada"))
    assertEquals(Form.errors[Payment](sent), Vector.empty)
    assertEquals(Form.decode[Payment](sent), Right(Payment("Ada", Kind.Wire, false)))
  }

  test("the blank renders the SAME tree as the empty object — it is a value, not a second opinion") {
    // the point of deriving it through `Form.of` + `Form.edit`: what a
    // form starts from cannot disagree with what a form shows
    assertEquals(Form.of[Payment](Form.blank[Payment]), Form.of[Payment](Json.JObj(Vector.empty)))
  }

  test("a chosen case still wins, and the other case's own fields come through") {
    val chosen = Form.edit[Payment](Form.blank[Payment], Event.Chosen("kind.$case", 1))
    val sent = post(chosen, Map("who" -> "Ada"))
    assertEquals(Form.decode[Payment](sent), Right(Payment("Ada", Kind.Card, false)))
  }

  test("nested products, lists and Options: the blank reaches every Select under them and leaves Options absent") {
    final case class Where(city: String, kind: Kind) derives Schema
    final case class Deep(where: Where, note: Option[String], tags: Vector[String]) derives Schema
    val blank = Form.blank[Deep]
    assertEquals(Form.errors[Deep](Form.edit[Deep](blank, Event.Edited("where.city", "Kyiv"))), Vector.empty)
    assertEquals(Form.decode[Deep](Form.edit[Deep](blank, Event.Edited("where.city", "Kyiv"))),
      Right(Deep(Where("Kyiv", Kind.Wire), None, Vector.empty)))
  }
}
