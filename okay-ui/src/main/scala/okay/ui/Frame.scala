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

  /**
   * Render a tree as lines; the focused widget is marked.
   *
   * `width` is the BUDGET this subtree may use, in characters, and 0
   * means "no budget" — which is v1's layout exactly, so every caller
   * that does not pass one is unchanged (ui-terminal-width). With a
   * budget, a horizontal box divides IT by weight instead of dividing
   * the row's natural width, which is what `Resized` was always for
   * and what the spec's sentence "the terminal divides width by
   * weight" meant; and a text WRAPS to the budget rather than running
   * past the screen — the same rule the browser's stylesheet states,
   * for the same reason: a value that is cut cannot be checked against
   * anything.
   */
  def render(ui: Ui, focus: Option[Ui] = None, width: Int = 0): Vector[String] = ui match
    case Text(s, style) =>
      val lines = s.split("\n", -1).toVector.flatMap(wrap(_, width))
      // tokens map to the terminal's idiom: emphasis is bold, muted is
      // dim, danger is red; size has no terminal meaning
      if style.bold || style.tone == Tone.Emphasis then lines.map(l => s"$Esc[1m$l$Esc[0m")
      else if style.dim || style.tone == Tone.Muted then lines.map(l => s"$Esc[2m$l$Esc[0m")
      else if style.tone == Tone.Danger then lines.map(l => s"$Esc[31m$l$Esc[0m")
      else lines
    case Column(children, _) => children.flatMap(c => render(c, focus, width))
    case Row(children, _) =>
      val shares = split(width, children.length, Vector.empty, children.length - 1)
      beside(children.zip(shares).map((c, w) => render(c, focus, w)), Vector.empty, " ",
        children.map(alignOf), shares)
    case Box(children, Dir.Vertical, _, gap, pad, _) =>
      val blocks = children.map(c => render(c, focus, math.max(width - 2 * pad, 0)))
      val joined = blocks.zipWithIndex.flatMap { (b, i) =>
        (if i > 0 then Vector.fill(gap)("") else Vector.empty) ++ b }
      joined.map(l => " " * pad + l)
    case Box(children, Dir.Horizontal, weights, gap, pad, _) =>
      val budget = math.max(width - 2 * pad, 0)
      val shares = split(budget, children.length, weights, gap * (children.length - 1))
      beside(children.zip(shares).map((c, w) => render(c, focus, w)), weights, " " * gap,
        children.map(alignOf), shares)
        .map(l => " " * pad + l)
    case Scroll(child, _) => render(child, focus, width)
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
      render(Box(fields :+ Button(submit, k, Role.Primary), Dir.Vertical), focus, width)
    // the budget reaches THROUGH a lowering, which is where a table is
    // drawn — the compiler caught both of these dropping it (E221,
    // "recursive call used a default argument"), and a table is
    // exactly the node whose columns the budget is for
    case semantic => render(Ui.lower(semantic, Set.empty), focus, width)

  /**
   * A LINE WRAPPED TO A BUDGET — never cut. A budget of 0 is "no
   * budget" and the line is itself. Breaking prefers the last SPACE
   * inside the budget, because prose reads better broken at a word;
   * where there is none (an IBAN, a hash, a URL — exactly the values
   * a reader must be able to copy and compare) it breaks at the
   * budget, which is the terminal's spelling of the browser's
   * `overflow-wrap: anywhere`.
   */
  private def wrap(line: String, width: Int): Vector[String] =
    if width <= 0 || line.length <= width then Vector(line)
    else
      val cut = line.lastIndexOf(' ', width)
      val at = if cut > 0 then cut else width
      val rest = if cut > 0 then line.substring(at + 1) else line.substring(at)
      line.substring(0, at) +: wrap(rest, width)

  /**
   * A budget divided among children: by WEIGHT when there is one per
   * child, and evenly otherwise. The separators between them are
   * taken off the top, so the shares plus the gaps are the budget. A
   * budget of 0 hands every child 0, which is "no budget" all the way
   * down — v1's layout.
   */
  private def split(width: Int, n: Int, weights: Vector[Int], gaps: Int): Vector[Int] =
    if width <= 0 || n <= 0 then Vector.fill(math.max(n, 0))(0)
    else
      val room = math.max(width - math.max(gaps, 0), 0)
      val ws = if weights.length == n && weights.forall(_ > 0) then weights else Vector.fill(n)(1)
      val total = ws.sum
      // the remainder goes to the last child rather than being lost,
      // so the shares always add up to the room
      val base = ws.map(w => room * w / total)
      base.updated(n - 1, base.last + (room - base.sum))

  /** what a cell says about where it sits — the terminal's half of
   * `Align` (ui-text-intent). Only a `Text` says it: a container's
   * alignment would be a layout property, and layout is `Box`'s */
  private def alignOf(ui: Ui): Align = ui match
    case Text(_, style) => style.align
    case _ => Align.Start

  /** blocks side by side; with weights, the row's natural width is
   * divided by weight and each block padded to its share — "the
   * terminal divides width by weight". A block whose text asked for
   * `Align.End` is padded on the LEFT instead, which is what makes a
   * column of numbers comparable down the page */
  private def beside(blocks: Vector[Vector[String]], weights: Vector[Int], sep: String,
                     aligns: Vector[Align], shares: Vector[Int]): Vector[String] =
    val height = blocks.map(_.length).maxOption.getOrElse(0)
    val natural = blocks.map(b => b.map(width).maxOption.getOrElse(0))
    val widths =
      // a SCREEN budget was handed down: the columns are its shares,
      // and a block narrower than its share is padded into it
      if shares.length == blocks.length && shares.forall(_ > 0) then shares
      else if weights.length == blocks.length && weights.forall(_ > 0) then
        val total = natural.sum
        val sum = weights.sum
        natural.zip(weights).map((n, w) => math.max(n, total * w / sum))
      else natural
    val padded = blocks.zip(widths).zipWithIndex.map { case ((b, w), i) =>
      val end = aligns.lift(i).contains(Align.End)
      b.padTo(height, "").map { l =>
        val pad = " " * (w - width(l))
        if end then pad + l else l + pad
      }
    }
    (0 until height).toVector.map(i => padded.map(_(i)).mkString(sep))

  /**
   * WHICH LINE THE FOCUSED WIDGET IS ON, so a host that clips a tall
   * frame to its screen can keep the focus visible (ui-terminal-scroll).
   *
   * Found by the ONE difference between the marked frame and the
   * unmarked one: marking is the only thing `focus` changes, so the
   * first line that differs is the line the focus is on. That reuses
   * the renderer rather than threading a line counter through every
   * case of it, and it cannot drift from what is actually drawn.
   */
  def focusLine(ui: Ui, focus: Option[Ui], width: Int = 0): Option[Int] =
    focus.flatMap { _ =>
      val marked = render(ui, focus, width)
      val plain = render(ui, None, width)
      if marked.length != plain.length then Some(0)
      else marked.indices.find(i => marked(i) != plain(i))
    }

  /**
   * A frame clipped to a screen: `rows` lines from `top`, padded to
   * the height so a shorter frame does not leave the previous paint
   * behind it. `rows <= 0` is "no screen" and the frame is itself.
   */
  def clip(lines: Vector[String], top: Int, rows: Int): Vector[String] =
    if rows <= 0 then lines
    else
      val from = math.max(0, math.min(top, math.max(lines.length - rows, 0)))
      val taken = lines.slice(from, from + rows)
      taken ++ Vector.fill(math.max(rows - taken.length, 0))("")

  /** the top a view must have to keep `line` on a screen of `rows`,
   * moving as little as possible — the rule a reader expects when
   * Tab walks off the bottom */
  def follow(top: Int, line: Int, rows: Int): Int =
    if rows <= 0 then top
    else if line < top then line
    else if line >= top + rows then line - rows + 1
    else top

  /** printable width — the ANSI escapes a styled Text carries are zero wide */
  def width(s: String): Int = s.replaceAll("\u001b" + "\\[[0-9;]*m", "").length

  /**
   * A KEY, after the escape sequences are decoded (ui-terminal-keys).
   * A terminal sends `ESC [ A` for an arrow and `ESC [ Z` for
   * Shift-Tab, one BYTE at a time, so naming what arrived has to
   * happen before a tree can be asked about it — and, like everything
   * else in this file, it is a value so it can be tested without a
   * tty.
   */
  enum Key:
    case Ch(c: Char)
    case Up, Down, Left, Right, BackTab, Home, End
    // the frame is taller than the screen: these move the VIEW, not
    // the focus (ui-terminal-scroll), and the host reads them itself
    case PageUp, PageDown
    /** a sequence this decoder does not name: dropped, never guessed */
    case Unknown

  /**
   * The decoder's state between bytes. A terminal hands an escape
   * sequence over several reads, and a host reads one byte at a time,
   * so the state is what has been seen so far of a sequence that has
   * not ended.
   */
  enum KeyState:
    case Plain
    case Escaped            // ESC seen
    case Bracket            // ESC [ (or ESC O) seen
    case Digits(ds: String) // ESC [ 1 … waiting for the final ~

  /**
   * ONE BYTE IN, the keys it completed out — usually none or one, and
   * TWO when a lone ESC turns out not to have started a sequence (the
   * ESC itself, then the byte that followed it). Total: an unknown
   * final byte answers `Unknown` rather than a guess, and the state
   * returns to `Plain` so one strange sequence cannot swallow the
   * keys after it.
   */
  def feed(st: KeyState, b: Int): (KeyState, Vector[Key]) =
    val c = b.toChar
    st match
      case KeyState.Plain =>
        if b == 27 then (KeyState.Escaped, Vector.empty) else (KeyState.Plain, Vector(Key.Ch(c)))
      case KeyState.Escaped =>
        if c == '[' || c == 'O' then (KeyState.Bracket, Vector.empty)
        // ESC that began nothing: the key itself, then this byte
        else if b == 27 then (KeyState.Escaped, Vector(Key.Ch('\u001b')))
        else (KeyState.Plain, Vector(Key.Ch('\u001b'), Key.Ch(c)))
      case KeyState.Bracket =>
        if c.isDigit then (KeyState.Digits(c.toString), Vector.empty)
        else (KeyState.Plain, Vector(named(c)))
      case KeyState.Digits(ds) =>
        if c.isDigit then (KeyState.Digits(ds + c), Vector.empty)
        else if c == '~' then (KeyState.Plain, Vector(tilde(ds)))
        else (KeyState.Plain, Vector(named(c)))

  private def named(c: Char): Key = c match
    case 'A' => Key.Up
    case 'B' => Key.Down
    case 'C' => Key.Right
    case 'D' => Key.Left
    case 'Z' => Key.BackTab
    case 'H' => Key.Home
    case 'F' => Key.End
    case _ => Key.Unknown

  /** the numbered forms: `ESC [ 1 ~` is Home on some terminals, `4 ~`
   * End, `7 ~`/`8 ~` on others */
  private def tilde(ds: String): Key = ds match
    case "1" | "7" => Key.Home
    case "4" | "8" => Key.End
    case "5" => Key.PageUp
    case "6" => Key.PageDown
    case _ => Key.Unknown

  /**
   * One raw key against the tree, at a focus: the next focus and what
   * the key MEANT — Tab moves, Enter presses or toggles, characters
   * edit, angle brackets choose. Interpretation is the host's job
   * precisely so the tree can stay a value.
   */
  def interpret(ui: Ui, focus: Int, ch: Char): (Int, Option[Event]) =
    interpret(ui, focus, Key.Ch(ch))

  /**
   * The same, at a decoded key. WHAT THE ARROWS DO, and why this way:
   * Up/Down and Tab/Shift-Tab move the FOCUS, Home/End jump to the
   * ends of the tab order, and Left/Right choose within a `Select`
   * exactly as `<`/`>` already did. Left/Right do nothing in an
   * `Input` ON PURPOSE — that is where a caret goes when this host
   * gains one, and taking the keys now would have to be taken back.
   */
  def interpret(ui: Ui, focus: Int, key: Key): (Int, Option[Event]) =
    val order = Ui.focusable(ui)
    def focused = order.lift(focus)
    val n = math.max(order.length, 1)
    def move(d: Int) = (((focus + d) % n) + n) % n
    key match
      case Key.Ch(ch) => interpretChar(ui, focus, ch)
      case Key.Down => (move(1), None)
      case Key.Up | Key.BackTab => (move(-1), None)
      case Key.Home => (0, None)
      case Key.End => (math.max(order.length - 1, 0), None)
      case Key.Right => focused match
        case Some(Ui.Select(o, i, k)) if i + 1 < o.length => (focus, Some(Event.Chosen(k, i + 1)))
        case _ => (focus, None)
      case Key.Left => focused match
        case Some(Ui.Select(_, i, k)) if i > 0 => (focus, Some(Event.Chosen(k, i - 1)))
        case _ => (focus, None)
      // the view is the host's, not the tree's: these say nothing and
      // move no focus, and the host reads them itself
      case Key.PageUp | Key.PageDown | Key.Unknown => (focus, None)

  private def interpretChar(ui: Ui, focus: Int, ch: Char): (Int, Option[Event]) =
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
