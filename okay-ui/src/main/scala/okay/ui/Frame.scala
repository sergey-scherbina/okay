package okay.ui

/**
 * The terminal's PURE half: a frame is a Vector[String], focus is an
 * index into the tab order, and a raw key against a tree is an
 * interpretation — all values, all testable with no tty anywhere.
 * The impure half (stty, stdin, painting) lives in the platform
 * source and is as thin as the seam demands.
 */
object Frame {

  import Ui.*

  private val Esc = "\u001b"

  /** render a tree as lines; the focused widget is marked */
  def render(ui: Ui, focus: Option[Ui] = None): Vector[String] = ui match
    case Text(s, style) =>
      val lines = s.split("\n", -1).toVector
      if style.bold then lines.map(l => s"$Esc[1m$l$Esc[0m")
      else if style.dim then lines.map(l => s"$Esc[2m$l$Esc[0m")
      else lines
    case Column(children, _) => children.flatMap(c => render(c, focus))
    case Row(children, _) =>
      val blocks = children.map(c => render(c, focus))
      val height = blocks.map(_.length).maxOption.getOrElse(0)
      val padded = blocks.map { b =>
        val w = b.map(width).maxOption.getOrElse(0)
        b.padTo(height, "").map(l => l + " " * (w - width(l)))
      }
      (0 until height).toVector.map(i => padded.map(_(i)).mkString(" "))
    case b @ Button(label, _) =>
      Vector(if focus.contains(b) then s"[>$label<]" else s"[ $label ]")
    case i @ Input(value, _, label) =>
      val name = if label.isEmpty then "" else s"$label: "
      Vector(if focus.contains(i) then s"$name[$value*]" else s"$name[$value]")
    case c @ Check(on, _, label) =>
      val box = if on then "[x]" else "[ ]"
      val f = if focus.contains(c) then ">" else " "
      Vector(s"$f$box $label")
    case s @ Select(options, selected, _) =>
      val cur = options.lift(selected).getOrElse("")
      Vector(if focus.contains(s) then s"<$cur>" else s" $cur ")

  /** printable width — the ANSI escapes a styled Text carries are zero wide */
  def width(s: String): Int = s.replaceAll("\u001b" + "\\[[0-9;]*m", "").length

  /**
   * One raw key against the tree, at a focus: the next focus and what
   * the key MEANT — Tab moves, Enter presses or toggles, characters
   * edit, angle brackets choose. Interpretation is the host's job
   * precisely so the tree can stay a value.
   */
  def interpret(ui: Ui, focus: Int, ch: Char): (Int, Option[Event]) =
    val order = Ui.focusable(ui)
    def focused = order.lift(focus)
    ch match
      case '\t' => ((focus + 1) % math.max(order.length, 1), None)
      case '\n' | '\r' => focused match
        case Some(Button(_, k)) => (focus, Some(Event.Pressed(k)))
        case Some(Check(on, k, _)) => (focus, Some(Event.Toggled(k, !on)))
        case _ => (focus, None)
      case '\u007f' | '\b' => focused match   // backspace erases
        case Some(Input(v, k, _)) if v.nonEmpty => (focus, Some(Event.Edited(k, v.init)))
        case _ => (focus, None)
      case '<' => focused match     // previous option
        case Some(Select(_, i, k)) if i > 0 => (focus, Some(Event.Chosen(k, i - 1)))
        case _ => (focus, None)
      case '>' => focused match     // next option
        case Some(Select(o, i, k)) if i + 1 < o.length => (focus, Some(Event.Chosen(k, i + 1)))
        case _ => (focus, None)
      case c if !c.isControl => focused match
        case Some(Input(v, k, _)) => (focus, Some(Event.Edited(k, v + c)))
        case Some(Check(on, k, _)) if c == ' ' => (focus, Some(Event.Toggled(k, !on)))
        case _ => (focus, None)
      case _ => (focus, None)
}
