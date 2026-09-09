package okay.script.api

import okay.codec.{Json, Schema}
import okay.ui.{Event, Form, Ui}

/** The plain road of a typed form: no JavaScript, a `<form
 * method="post">` rendered from the Schema and a POST read back
 * through the SAME Schema -- okay-ui's `Form`, "the fifth algebra",
 * with HTML names on the fields. See specs/okay-script.md "Typed
 * forms".
 */
object Forms:

  /** what a failed `read` hands back: the value as posted (to render
   * the form again, filled in) and the errors under their fields
   * (`""` for the whole form) */
  final case class Draft(value: Json, errors: Vector[(String, String)])

  object Draft:
    val empty: Draft = Draft(Json.JObj(Vector.empty), Vector.empty)

  private val indexed = """\[(\d+)\]""".r

  /** the value a form starts from: every check the Schema shows is
   * `false` rather than absent -- an untouched checkbox is a `false`,
   * not a "required" (what the plain road gets from HTML for free,
   * the live road needs said) */
  def defaults[A](using Schema[A]): Json =
    Ui.focusable(Form.of[A](Json.JObj(Vector.empty))).collect { case Ui.Check(_, k, _) => Event.Toggled(k, false) }
      .foldLeft(Json.JObj(Vector.empty): Json)(Form.edit[A])

  /** the Schema's form as HTML that posts to `action`: the element
   * structure `Live.html` gives, plus `name=` on every field (its
   * dotted key) and a submit button; `draft.errors` render under
   * their fields as `Form` renders them */
  def html[A](action: String, draft: Draft = Draft.empty, submit: String = "Submit")(using Schema[A]): String =
    val tree = Form.ofWith[A](draft.errors)(draft.value)
    val whole = draft.errors.collect { case ("", m) => m }
    s"""<form method="post" action="${Live.escape(action)}">""" +
      Live.html(tree, named = true) +
      whole.map(m => s"""<p class="okay-error">! ${Live.escape(m)}</p>""").mkString +
      s"""<button type="submit">${Live.escape(submit)}</button></form>"""

  /** the posted fields (`Web.current.form`), folded through okay-ui's
   * own edit site: for every focusable of the rendered tree an
   * `Edited`/`Toggled`/`Chosen`, applied with `Form.edit`, re-rendered
   * and repeated until stable (a sum's case knob changes the fields
   * below it); then the per-field errors, the decode, and the
   * cross-field `checks`. A `__press=<key>` (a list's `+`/`-`) is an
   * edit, not a submit: it answers the Draft with that edit applied.
   * A checkbox not posted is `false`, as HTML has it. */
  def read[A](fields: Map[String, String], checks: Form.Check[A]*)(using Schema[A]): Either[Draft, A] =
    // the lists the post names: `items[2].name` wants `items` to be
    // at least 3 long before its inputs exist to be filled
    val wanted: Map[String, Int] =
      fields.keys.toVector.flatMap(k => indexed.findAllMatchIn(k).map(m => k.substring(0, m.start) -> m.group(1).toInt))
        .groupMapReduce(_._1)(_._2)(math.max)
    var value: Json = Json.JObj(Vector.empty)
    var stable = false
    var rounds = 0
    while !stable && rounds < 32 do
      val keys = Ui.focusable(Form.of[A](value)).flatMap(Ui.keyOf)
      val grow = wanted.collect { case (p, n) if !keys.exists(_.startsWith(s"$p[$n]")) => Event.Pressed(s"$p$$add") }
      if grow.nonEmpty then value = grow.foldLeft(value)(Form.edit[A])
      else
        val events = Ui.focusable(Form.of[A](value)).flatMap {
          // an empty text is NOT posted into the value: an optional
          // field stays absent (None), a required one reads "required"
          case Ui.Input(_, k, _, _, _) => fields.get(k).filter(_.nonEmpty).map(Event.Edited(k, _))
          case Ui.Check(_, k, _) => Some(Event.Toggled(k, fields.contains(k)))
          case Ui.Select(os, _, k) => fields.get(k).map(v => Event.Chosen(k, math.max(os.indexOf(v), 0)))
          case _ => None
        }
        val next = events.foldLeft(value)(Form.edit[A])
        stable = next == value
        value = next
      rounds += 1
    fields.get("__press") match
      case Some(k) => Left(Draft(Form.edit[A](value, Event.Pressed(k)), Vector.empty))
      case None =>
        val errs = Form.errors[A](value)
        if errs.nonEmpty then Left(Draft(value, errs))
        else Form.decode[A](value) match
          case Left(m) => Left(Draft(value, Vector("" -> m)))
          case Right(a) =>
            val failures = checks.flatMap(_(a)).toVector
            if failures.isEmpty then Right(a) else Left(Draft(value, failures))
