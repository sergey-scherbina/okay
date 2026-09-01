package okay.ui

/**
 * The React-shaped rendering, PURE: a Ui tree becomes the element
 * tree a `createElement` host expects — type, props, children — as a
 * VALUE, so the mapping is asserted on the JVM and the js glue is the
 * five lines it should be. Works for anything with React's shape
 * (Preact included), which is the point of targeting the shape rather
 * than the library.
 */
final case class Elem(tag: String,
                      props: Vector[(String, String)],
                      children: Vector[Elem] = Vector.empty,
                      text: Option[String] = None)

object React {

  import Ui.*

  /** the tree, in createElement's terms; keys ride as data-key, which
   * is also how the glue knows which Event a DOM event means */
  def elem(ui: Ui): Elem = ui match
    case Text(s, style) =>
      val cls = (if style.bold then Vector("okay-bold") else Vector.empty) ++
        (if style.dim then Vector("okay-dim") else Vector.empty)
      Elem("span", if cls.isEmpty then Vector.empty else Vector("className" -> cls.mkString(" ")),
        text = Some(s))
    case Row(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-row")), children.map(elem))
    case Column(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-col")), children.map(elem))
    case Button(label, key) =>
      Elem("button", keyed(key, Vector.empty), text = Some(label))
    case Input(value, key, label) =>
      val input = Elem("input", keyed(key, Vector("value" -> value)))
      if label.isEmpty then input
      else Elem("label", Vector.empty, Vector(Elem("span", Vector.empty, text = Some(label)), input))
    case Check(on, key, label) =>
      val box = Elem("input", keyed(key, Vector("type" -> "checkbox", "checked" -> on.toString)))
      if label.isEmpty then box
      else Elem("label", Vector.empty, Vector(box, Elem("span", Vector.empty, text = Some(label))))
    case Select(options, selected, key) =>
      Elem("select", keyed(key, Vector("value" -> options.lift(selected).getOrElse(""))),
        options.map(o => Elem("option", Vector("value" -> o), text = Some(o))))

  private def keyed(key: String, props: Vector[(String, String)]): Vector[(String, String)] =
    if key.isEmpty then props else ("data-key" -> key) +: props

  /** the DOM event a rendered node reports, back as OUR event — the
   * other half of the glue, pure as well */
  def event(ui: Ui, key: String, kind: String, value: String): Option[Event] =
    Ui.focusable(ui).collectFirst {
      case Button(_, k) if k == key && kind == "click" => Event.Pressed(k)
      case Input(_, k, _) if k == key && kind == "input" => Event.Edited(k, value)
      case Check(on, k, _) if k == key && kind == "change" => Event.Toggled(k, !on)
      case Select(o, _, k) if k == key && kind == "change" =>
        Event.Chosen(k, math.max(o.indexOf(value), 0))
    }
}
