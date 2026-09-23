package okay.scala2

import okay.codec.Schema
import okay.ui.{Event, Ui}

/**
 * A form over `A`, for Scala 2.13 (specs/scala2-facade.md, stage 11).
 *
 * okay-ui's `Form` renders a form from the same `Schema` that decodes
 * it, and folds the user's edits into the value. Every one of its
 * functions takes or answers that value as an `okay.codec.Json`, which
 * scalac 2.13 cannot read. So this class holds the value itself and
 * speaks only in `A`, `Ui` and `Event`: `view` to draw it, `edit` to
 * fold an event, `errors` for what does not validate yet, `decoded` for
 * the finished value. It is an ordinary immutable state, so it drops
 * into `UiApp.run` as the state of the loop.
 */
final class FormState[A] private (private val value: FormValue, private val schema: Schema[A],
                                  private val labels: Map[String, String]) {

  /** the form, with the current value and the error beside each field
   * that does not validate */
  def view: Ui = okay.ui.Form.ofWith[A](okay.ui.Form.errors[A](value.json)(using schema), labels)(using schema)(value.json)

  /** fold one event: an edit, a toggle, a choice, or a whole submitted
   * form from a client that folds the fields itself */
  def edit(e: Event): FormState[A] = {
    val next = e match {
      case Event.Submitted(_, _) => okay.ui.Form.submitted[A](using schema)(value.json, e)
      case other => okay.ui.Form.edit[A](using schema)(value.json, other)
    }
    new FormState(new FormValue(next), schema, labels)
  }

  /** each field that does not validate yet, as (field path, message) */
  def errors: Vector[(String, String)] = okay.ui.Form.errors[A](value.json)(using schema)

  /** the value, if it is complete and valid */
  def decoded: Either[String, A] = okay.ui.Form.decode[A](using schema)(value.json)

  /** the value as JSON text, complete or not */
  def json: String = okay.codec.Json.print(value.json)

  /** the same form, with human labels for some fields (by field path) */
  def withLabels(labels: Map[String, String]): FormState[A] = new FormState(value, schema, labels)
}

/** the JSON value, out of `FormState`'s constructor: scalac 2.13 cannot
 * read `okay.codec.Json`, and it reads a constructor's types eagerly */
private[scala2] final class FormValue(val json: okay.codec.Json) extends AnyVal

object FormState {

  /** an empty form: every Select on its first option, every Check off */
  def blank[A](using s: Schema[A]): FormState[A] =
    new FormState(new FormValue(okay.ui.Form.blank[A]), s, Map.empty)

  /** a form filled with `a` */
  def of[A](a: A)(using s: Schema[A]): FormState[A] =
    new FormState(new FormValue(okay.codec.Json.parse(okay.codec.Json.write(a))), s, Map.empty)
}
