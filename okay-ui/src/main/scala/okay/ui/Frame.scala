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
      // tokens map to the terminal's idiom: emphasis is bold, muted is
      // dim, danger is red; size has no terminal meaning
      if style.bold || style.tone == Tone.Emphasis then lines.map(l => s"$Esc[1m$l$Esc[0m")
      else if style.dim || style.tone == Tone.Muted then lines.map(l => s"$Esc[2m$l$Esc[0m")
      else if style.tone == Tone.Danger then lines.map(l => s"$Esc[31m$l$Esc[0m")
      else lines
    case Column(children, _) => children.flatMap(c => render(c, focus))
    case Row(children, _) => beside(children.map(c => render(c, focus)), Vector.empty, " ")
    case Box(children, Dir.Vertical, _, gap, pad, _) =>
      val blocks = children.map(c => render(c, focus))
      val joined = blocks.zipWithIndex.flatMap { (b, i) =>
        (if i > 0 then Vector.fill(gap)("") else Vector.empty) ++ b }
      joined.map(l => " " * pad + l)
    case Box(children, Dir.Horizontal, weights, gap, pad, _) =>
      beside(children.map(c => render(c, focus)), weights, " " * gap).map(l => " " * pad + l)
    case Scroll(child, _) => render(child, focus)
    case Image(_, alt) => Vector(s"[image: $alt]")
    case b @ Button(label, _, role) =>
      Vector(if focus.contains(b) then s"[>$label<]"
             else if role == Role.Active then s"[=$label=]" else s"[ $label ]")
    case i @ Input(value, _, label, kind, _) =>
      val name = if label.isEmpty then "" else s"$label: "
      val shown = if kind == InputKind.Secret then "*" * value.length else value
      Vector(if focus.contains(i) then s"$name[$shown*]" else s"$name[$shown]")
    case c @ Check(on, _, label) =>
      val box = if on then "[x]" else "[ ]"
      val f = if focus.contains(c) then ">" else " "
      Vector(s"$f$box $label")
    case s @ Select(options, selected, _) =>
      val cur = options.lift(selected).getOrElse("")
      Vector(if focus.contains(s) then s"<$cur>" else s" $cur ")

    case Form(fields, submit, k) =>
      render(Box(fields :+ Button(submit, k, Role.Primary), Dir.Vertical), focus)
    case semantic => render(Ui.lower(semantic, Set.empty), focus)

  /** blocks side by side; with weights, the row's natural width is
   * divided by weight and each block padded to its share — "the
   * terminal divides width by weight" */
  private def beside(blocks: Vector[Vector[String]], weights: Vector[Int], sep: String): Vector[String] =
    val height = blocks.map(_.length).maxOption.getOrElse(0)
    val natural = blocks.map(b => b.map(width).maxOption.getOrElse(0))
    val widths =
      if weights.length == blocks.length && weights.forall(_ > 0) then
        val total = natural.sum
        val sum = weights.sum
        natural.zip(weights).map((n, w) => math.max(n, total * w / sum))
      else natural
    val padded = blocks.zip(widths).map { (b, w) =>
      b.padTo(height, "").map(l => l + " " * (w - width(l)))
    }
    (0 until height).toVector.map(i => padded.map(_(i)).mkString(sep))

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
        case Some(Button(_, k, _)) => (focus, Some(Event.Pressed(k)))
        case Some(Check(on, k, _)) => (focus, Some(Event.Toggled(k, !on)))
        case _ => (focus, None)
      case '\u007f' | '\b' => focused match   // backspace erases
        case Some(Input(v, k, _, _, _)) if v.nonEmpty => (focus, Some(Event.Edited(k, v.init)))
        case _ => (focus, None)
      case '<' => focused match     // previous option
        case Some(Select(_, i, k)) if i > 0 => (focus, Some(Event.Chosen(k, i - 1)))
        case _ => (focus, None)
      case '>' => focused match     // next option
        case Some(Select(o, i, k)) if i + 1 < o.length => (focus, Some(Event.Chosen(k, i + 1)))
        case _ => (focus, None)
      case c if !c.isControl => focused match
        case Some(Input(v, k, _, _, _)) => (focus, Some(Event.Edited(k, v + c)))
        case Some(Check(on, k, _)) if c == ' ' => (focus, Some(Event.Toggled(k, !on)))
        case _ => (focus, None)
      case _ => (focus, None)
}
