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
        (if style.dim then Vector("okay-dim") else Vector.empty) ++
        (if style.tone != Tone.Plain then Vector("okay-tone-" + style.tone.toString.toLowerCase) else Vector.empty) ++
        (if style.size != Size.Normal then Vector("okay-size-" + style.size.toString.toLowerCase) else Vector.empty)
      Elem("span", if cls.isEmpty then Vector.empty else Vector("className" -> cls.mkString(" ")),
        text = Some(s))
    case Row(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-row")), children.map(elem))
    case Column(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-col")), children.map(elem))
    case Box(children, dir, weights, gap, pad, key) =>
      // flexbox forgets the layout problem: weights are flex-grow on
      // the children, gap and pad are the box's own style
      val cls = if dir == Dir.Horizontal then "okay-box okay-h" else "okay-box okay-v"
      val style = (if gap > 0 then Vector(s"gap:${gap}ch") else Vector.empty) ++
        (if pad > 0 then Vector(s"padding:${pad}ch") else Vector.empty)
      // the weights ALSO ride on the box as data-w, so a patch consumer
      // replacing or inserting one child can give it its flex without
      // holding the tree (TestDom found a Replace losing it)
      val weighted = weights.length == children.length
      val props = Vector("className" -> cls) ++
        (if style.nonEmpty then Vector("style" -> style.mkString(";")) else Vector.empty) ++
        (if weighted then Vector("data-w" -> weights.mkString(" ")) else Vector.empty)
      val kids = children.zipWithIndex.map { (c, i) =>
        val e = elem(c)
        if weighted then styled(e, s"flex:${weights(i)}") else e
      }
      Elem("div", keyed(key, props), kids)
    case Scroll(child, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-scroll", "style" -> "overflow:auto")), Vector(elem(child)))
    case Image(src, alt) => Elem("img", Vector("src" -> src, "alt" -> alt))
    case Button(label, key, role) =>
      val props = if role == Role.Plain then Vector.empty
        else Vector("className" -> ("okay-" + role.toString.toLowerCase))
      Elem("button", keyed(key, props), text = Some(label))
    case Input(value, key, label, kind, _) =>
      val input = kind match
        case InputKind.Text => Elem("input", keyed(key, Vector("value" -> value)))
        case InputKind.Secret => Elem("input", keyed(key, Vector("type" -> "password", "value" -> value)))
        case InputKind.Number => Elem("input", keyed(key, Vector("type" -> "number", "value" -> value)))
        case InputKind.Multiline => Elem("textarea", keyed(key, Vector("value" -> value)))
      if label.isEmpty then input
      else Elem("label", Vector.empty, Vector(Elem("span", Vector.empty, text = Some(label)), input))
    case Check(on, key, label) =>
      val box = Elem("input", keyed(key, Vector("type" -> "checkbox", "checked" -> on.toString)))
      if label.isEmpty then box
      else Elem("label", Vector.empty, Vector(box, Elem("span", Vector.empty, text = Some(label))))
    case Select(options, selected, key) =>
      Elem("select", keyed(key, Vector("value" -> options.lift(selected).getOrElse(""))),
        options.map(o => Elem("option", Vector("value" -> o), text = Some(o))))
    // the React host claims no semantic node in stage 0: it draws the
    // lowering, which is the node's meaning
    case semantic => elem(Ui.lower(semantic, Set.empty))

  /** a style declaration appended to an element's own */
  private def styled(e: Elem, decl: String): Elem =
    e.props.indexWhere(_._1 == "style") match
      case -1 => e.copy(props = e.props :+ ("style" -> decl))
      case i => e.copy(props = e.props.updated(i, "style" -> (e.props(i)._2 + ";" + decl)))

  private def keyed(key: String, props: Vector[(String, String)]): Vector[(String, String)] =
    if key.isEmpty then props else ("data-key" -> key) +: props

  /** the DOM event a rendered node reports, back as OUR event — the
   * other half of the glue, pure as well */
  def event(ui: Ui, key: String, kind: String, value: String): Option[Event] =
    Ui.focusable(ui).collectFirst {
      case Button(_, k, _) if k == key && kind == "click" => Event.Pressed(k)
      case Input(_, k, _, _, _) if k == key && kind == "input" => Event.Edited(k, value)
      case Check(on, k, _) if k == key && kind == "change" => Event.Toggled(k, !on)
      case Select(o, _, k) if k == key && kind == "change" =>
        Event.Chosen(k, math.max(o.indexOf(value), 0))
    }
}
