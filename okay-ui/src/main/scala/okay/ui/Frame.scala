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
   * WHERE A WIDGET WAS DRAWN: the cell it starts at and the cell it
   * fills, in the frame `render` answers (ui-terminal-layout-map).
   *
   * `key` is the widget's own, or empty where it has none — the INDEX
   * is what identifies it, and the placements come out in the order
   * `Ui.focusable` walks, because this walk and that one descend
   * through the same nodes in the same order.
   */
  final case class Placed(key: String, row: Int, col: Int, w: Int, h: Int):
    def holds(r: Int, c: Int): Boolean = r >= row && r < row + h && c >= col && c < col + w

  /**
   * WHAT THE SCREEN GIVES A FRAME: the width to lay out in, the height
   * to fit into, the caret in the focused input, and where each keyed
   * `Scroll` is scrolled to (ui-scroll-viewport).
   *
   * Zero is "unbounded" for both budgets, which is v1's layout, so a
   * caller that knows nothing about screens passes nothing.
   */
  final case class View(width: Int = 0, height: Int = 0, caret: Int = -1,
                        scroll: Map[String, Int] = Map.empty)

  /**
   * THE FRAME AND WHERE EVERY FOCUSABLE WIDGET SITS IN IT.
   *
   * `render` is this function's first half — one walk, not two, which
   * is the whole reason it is written this way round: two walks of one
   * layout convention are held equal by a law until the day they are
   * not (`ui-path-two-walks` measured the alternative and refused it
   * for a different reason, and the lesson is the same).
   *
   * What it buys: hit-testing reads a MAP rather than searching the
   * frame for the text each widget drew.
   */
  def laid(ui: Ui, focus: Option[Ui], view: View): (Vector[String], Vector[Placed]) =
    at(ui, focus, view, 0, 0)

  def laid(ui: Ui, focus: Option[Ui] = None, width: Int = 0, caret: Int = -1)
          : (Vector[String], Vector[Placed]) =
    at(ui, focus, View(width = width, caret = caret), 0, 0)

  /** render a tree as lines; the focused widget is marked. The lines
   * of `laid`, which is where the layout actually happens */
  def render(ui: Ui, focus: Option[Ui] = None, width: Int = 0, caret: Int = -1): Vector[String] =
    at(ui, focus, View(width = width, caret = caret), 0, 0)._1

  /** the same, at a full view — what a host with a screen renders */
  def render(ui: Ui, focus: Option[Ui], view: View): Vector[String] =
    at(ui, focus, view, 0, 0)._1

  /**
   * The walk, at an origin. Every case answers the lines it always
   * answered and the placements those lines imply — a container from
   * where it put its children, a leaf for itself.
   *
   * A LEAF FILLS ITS BUDGET, not its text: a cell wider than the
   * button in it is still the button's cell, so a click on the padding
   * lands on the button a reader can see there.
   */
  private def at(ui: Ui, focus: Option[Ui], v: View, row: Int, col: Int)
               : (Vector[String], Vector[Placed]) = ui match
    case Text(s, style) =>
      val lines = s.split("\n", -1).toVector.flatMap(wrap(_, v.width))
      // tokens map to the terminal's idiom: emphasis is bold, muted is
      // dim, danger is red; size has no terminal meaning
      val out =
        if style.bold || style.tone == Tone.Emphasis then lines.map(l => s"$Esc[1m$l$Esc[0m")
        else if style.dim || style.tone == Tone.Muted then lines.map(l => s"$Esc[2m$l$Esc[0m")
        else if style.tone == Tone.Danger then lines.map(l => s"$Esc[31m$l$Esc[0m")
        else lines
      (out, Vector.empty)

    case Column(children, _) => stacked(children, focus, v, row, col, gap = 0, pad = 0)
    case Box(children, Dir.Vertical, _, gap, pad, _) => stacked(children, focus, v, row, col, gap, pad)

    case Row(children, _) => side(children, focus, v, row, col, Vector.empty, " ", 0)
    case Box(children, Dir.Horizontal, weights, gap, pad, _) =>
      side(children, focus, v, row, col, weights, " " * gap, pad)

    /**
     * A SCROLL CLIPS ITS OWN CHILD, when it was given a height
     * (ui-scroll-viewport). The child is drawn WHOLE and then cut to
     * the viewport at this key's offset — so what is off-screen was
     * laid out, and scrolling shows it without re-laying anything.
     *
     * The placements move with the view and the ones that scrolled out
     * of it are dropped: a click can only land on what a reader can
     * see, which is the same rule the capability list states for the
     * wire.
     */
    case Scroll(child, key) =>
      val (cl, cp) = at(child, focus, v.copy(height = 0), row, col)
      if v.height <= 0 then (cl, cp)
      else
        val top = math.max(0, math.min(v.scroll.getOrElse(key, 0), math.max(cl.length - v.height, 0)))
        val shown = cp.map(p => p.copy(row = p.row - top))
          .filter(p => p.row + p.h > row && p.row < row + v.height)
        (clip(cl, top, v.height), shown)

    case Image(_, alt) => (Vector(s"[image: $alt]"), Vector.empty)

    case b @ Button(label, _, role) =>
      leaf(ui, Vector(if focus.contains(b) then s"[>$label<]"
                      else if role == Role.Active then s"[=$label=]" else s"[ $label ]"),
        v.width, row, col)
    case i @ Input(value, _, label, kind, _) =>
      val name = if label.isEmpty then "" else s"$label: "
      val shown = if kind == InputKind.Secret then "*" * value.length else value
      // THE CARET IS REVERSE VIDEO, which is what a terminal's own
      // cursor is (ui-terminal-caret). It costs NO COLUMNS - `width`
      // strips the escapes - so a caret cannot push a value out of its
      // column, and past the end of the value it sits on a space,
      // which is where the next character goes. A host with no caret
      // passes -1 and gets v1's trailing mark.
      leaf(ui, Vector(
        if !focus.contains(i) then s"$name[$shown]"
        else if v.caret < 0 then s"$name[$shown*]"
        else s"$name[${carets(shown, v.caret)}]"), v.width, row, col)
    case c @ Check(on, _, label) =>
      val box = if on then "[x]" else "[ ]"
      val f = if focus.contains(c) then ">" else " "
      leaf(ui, Vector(s"$f$box $label"), v.width, row, col)
    case s @ Select(options, selected, _) =>
      val cur = options.lift(selected).getOrElse("")
      leaf(ui, Vector(if focus.contains(s) then s"<$cur>" else s" $cur "), v.width, row, col)

    case Form(fields, submit, k) =>
      at(Box(fields :+ Button(submit, k, Role.Primary), Dir.Vertical), focus, v, row, col)
    // the budget reaches THROUGH a lowering, which is where a table is
    // drawn - the compiler caught both of these dropping it (E221,
    // "recursive call used a default argument"), and a table is
    // exactly the node whose columns the budget is for
    case semantic => at(Ui.lower(semantic, Set.empty), focus, v, row, col)

  /** a leaf, wrapped to its cell, and the cell it fills */
  private def leaf(ui: Ui, lines: Vector[String], budget: Int, row: Int, col: Int)
                  : (Vector[String], Vector[Placed]) =
    val out = lines.flatMap(wrap(_, budget))
    val w = math.max(out.map(width).maxOption.getOrElse(0), budget)
    (out, Vector(Placed(Ui.keyOf(ui).getOrElse(""), row, col, w, out.length)))

  /**
   * THE SCROLL REGION A WIDGET IS INSIDE, innermost first — what a
   * host moves when the reader pages while the focus is in a list
   * rather than on the page (ui-scroll-viewport).
   */
  def scrollAt(ui: Ui, focus: Int): Option[String] =
    Ui.focusable(ui).lift(focus).flatMap { f =>
      def go(u: Ui, inside: Option[String]): Option[String] =
        if u == f then inside
        else u match
          case Scroll(child, key) => go(child, if key.isEmpty then inside else Some(key))
          case Column(cs, _) => cs.view.flatMap(c => go(c, inside)).headOption
          case Row(cs, _) => cs.view.flatMap(c => go(c, inside)).headOption
          case b: Box => b.children.view.flatMap(c => go(c, inside)).headOption
          case Form(fs, submit, k) =>
            go(Box(fs :+ Button(submit, k, Role.Primary), Dir.Vertical), inside)
          case _: Text | _: Image | _: Button | _: Input | _: Check | _: Select => None
          case semantic => go(Ui.lower(semantic, Set.empty), inside)
      go(ui, None)
    }

  /** is this child a viewport — the node that shares out the leftover
   * vertical space rather than taking its own height */
  private def scrolls(ui: Ui): Boolean = ui match
    case _: Scroll => true
    case _ => false

  /**
   * Children below one another: gap blank rows between, pad columns in.
   *
   * WHO GETS THE LEFTOVER VERTICAL SPACE, when this box was given a
   * height (ui-scroll-viewport): every child that is not a `Scroll`
   * takes its NATURAL height, and the `Scroll` children share what is
   * left, evenly, the remainder to the last. A nested `Scroll` then
   * divides its own share the same way.
   *
   * It is the rule a browser and every terminal application use, and
   * it needs nothing new in the tree — which is why it was chosen over
   * giving `Scroll` a weight: a height in the tree would be a pixel by
   * another name.
   */
  private def stacked(children: Vector[Ui], focus: Option[Ui], v: View,
                      row: Int, col: Int, gap: Int, pad: Int)
                     : (Vector[String], Vector[Placed]) =
    val budget = math.max(v.width - 2 * pad, 0)
    val inner = v.copy(width = budget)
    val heights: Vector[Int] =
      if v.height <= 0 || !children.exists(scrolls) then Vector.fill(children.length)(0)
      else
        val naturals = children.map(c => at(c, focus, inner.copy(height = 0), 0, 0)._1.length)
        val viewports = children.indices.filter(i => scrolls(children(i)))
        val fixed = children.indices.filterNot(viewports.contains).map(naturals).sum +
          gap * math.max(children.length - 1, 0)
        val left = math.max(v.height - fixed, 0)
        val each = left / viewports.length
        children.indices.toVector.map { i =>
          if !viewports.contains(i) then 0
          else if i == viewports.last then left - each * (viewports.length - 1)
          else each
        }
    val (lines, places, _) = children.zipWithIndex
      .foldLeft((Vector.empty[String], Vector.empty[Placed], row)) {
        case ((ls, ps, r), (c, i)) =>
          val top = if i > 0 then r + gap else r
          val (cl, cp) = at(c, focus, inner.copy(height = heights(i)), top, col + pad)
          (ls ++ (if i > 0 then Vector.fill(gap)("") else Vector.empty) ++ cl, ps ++ cp, top + cl.length)
      }
    (lines.map(l => " " * pad + l), places)

  /** children beside one another: each in its share, separated */
  private def side(children: Vector[Ui], focus: Option[Ui], v: View,
                   row: Int, col: Int, weights: Vector[Int], sep: String, pad: Int)
                  : (Vector[String], Vector[Placed]) =
    val budget = math.max(v.width - 2 * pad, 0)
    // MEASURED before divided: a column narrower than its longest
    // word breaks the word, and a word is the smallest thing wrapping
    // must not split (ui-column-minimum)
    val natural = children.map(c => at(c, focus, v.copy(width = 0), 0, 0)._1)
    val shares = split(budget, children.length, weights, sep.length * (children.length - 1),
      natural.map(longestWord), natural.map(b => b.map(Frame.width).maxOption.getOrElse(0)))
    val blocks = children.zip(shares).map((c, w) => at(c, focus, v.copy(width = w), 0, 0)._1)
    val widths = columns(blocks, weights, shares)
    // where each child begins: its own share plus the separators
    // before it, counted in the SAME widths `beside` pads to
    val offsets = widths.scanLeft(0)((x, w) => x + w + sep.length).init
    val places = children.zip(shares).zip(offsets).flatMap { case ((c, w), dx) =>
      at(c, focus, v.copy(width = w), row, col + pad + dx)._2
    }
    (beside(blocks, weights, sep, children.map(alignOf), shares).map(l => " " * pad + l), places)

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
    if width <= 0 || Frame.width(line) <= width then Vector(line)
    else
      // COUNTED IN PRINTABLE COLUMNS, not characters. A focused input
      // carries its caret as reverse video — nine characters of escape
      // that occupy no column — so measuring the raw string wrapped it
      // five columns early and broke a field that fitted
      // (ui-terminal-layout-map, found by a test that expected a
      // wrapped input and got a mangled one).
      var i = 0
      var cols = 0
      var lastSpace = -1
      var cut = -1
      while i < line.length && cut < 0 do
        if line.startsWith(Esc, i) then
          val m = line.indexOf('m', i)
          i = if m < 0 then line.length else m + 1
        else
          if cols == width then cut = i
          else
            if line.charAt(i) == ' ' then lastSpace = i
            cols += 1
            i += 1
      if cut < 0 then Vector(line)
      else
        val at = if lastSpace > 0 then lastSpace else cut
        val rest = if lastSpace > 0 then line.substring(at + 1) else line.substring(at)
        line.substring(0, at) +: wrap(rest, width)

  /**
   * A budget divided among children: by WEIGHT when there is one per
   * child, and evenly otherwise. The separators between them are taken
   * off the top, so the shares plus the gaps are the budget. A budget
   * of 0 hands every child 0, which is "no budget" all the way down —
   * v1's layout.
   *
   * AND NO COLUMN IS NARROWER THAN ITS LONGEST WORD, where the budget
   * allows it (ui-column-minimum). Weights alone gave okay-watch's
   * analyst page headers that broke mid-word at 80 columns —
   * `alert/s`, `weigh/t`, `hel/d/by` — which is the defect that page
   * had already met in a browser and fixed there with
   * `overflow-wrap: normal` on `th`. A terminal has no stylesheet to
   * say it with, so the layout says it: a word is the smallest thing
   * that must not be broken, and a column that can hold its longest
   * one does not break it.
   *
   * The room is taken from columns that have SLACK — more share than
   * their content needs — and the widest of them first, because that
   * is where a character costs a reader least. When the minimums do
   * not fit at all, the shares stand: a screen too narrow to hold the
   * words is a screen where breaking is the honest answer, not an
   * error.
   */
  private def split(width: Int, n: Int, weights: Vector[Int], gaps: Int,
                    mins: Vector[Int], naturals: Vector[Int]): Vector[Int] =
    if width <= 0 || n <= 0 then Vector.fill(math.max(n, 0))(0)
    else
      val room = math.max(width - math.max(gaps, 0), 0)
      val ws = if weights.length == n && weights.forall(_ > 0) then weights else Vector.fill(n)(1)
      val total = ws.sum
      // the remainder goes to the last child rather than being lost,
      // so the shares always add up to the room
      val base = ws.map(w => room * w / total)
      val shares = base.updated(n - 1, base.last + (room - base.sum))
      if mins.length != n || mins.sum > room then shares else raise(shares, mins, naturals)

  /** every share up to its minimum, paid for out of the slack in the
   * others — widest first */
  private def raise(shares: Vector[Int], mins: Vector[Int], naturals: Vector[Int]): Vector[Int] =
    val out = scala.collection.mutable.ArrayBuffer.from(shares)
    def slack(i: Int): Int =
      val need = if naturals.length == out.length then math.max(naturals(i), mins(i)) else mins(i)
      math.max(out(i) - math.max(need, 1), 0)
    out.indices.foreach { i =>
      var owed = mins(i) - out(i)
      while owed > 0 do
        val from = out.indices.filter(j => j != i && slack(j) > 0).maxByOption(out(_))
        from match
          case Some(j) =>
            val take = math.min(owed, slack(j))
            out(j) -= take
            out(i) += take
            owed -= take
          case None => owed = 0     // nothing to take: the share stands
    }
    out.toVector

  /** the longest unbreakable run in what a child draws: a word, since
   * a word is the smallest thing wrapping must not split */
  private def longestWord(block: Vector[String]): Int =
    block.flatMap(l => strip(l).split(" ")).map(_.length).maxOption.getOrElse(0)

  /** what a cell says about where it sits — the terminal's half of
   * `Align` (ui-text-intent). Only a `Text` says it: a container's
   * alignment would be a layout property, and layout is `Box`'s */
  private def alignOf(ui: Ui): Align = ui match
    case Text(_, style) => style.align
    case _ => Align.Start

  /**
   * THE COLUMN WIDTHS A ROW LAYS OUT IN — one definition, because the
   * lines and the PLACEMENTS must agree about them or a click lands a
   * column off (ui-terminal-layout-map).
   */
  private def columns(blocks: Vector[Vector[String]], weights: Vector[Int],
                      shares: Vector[Int]): Vector[Int] =
    val natural = blocks.map(b => b.map(width).maxOption.getOrElse(0))
    // a SCREEN budget was handed down: the columns are its shares, and
    // a block narrower than its share is padded into it
    if shares.length == blocks.length && shares.forall(_ > 0) then shares
    else if weights.length == blocks.length && weights.forall(_ > 0) then
      val total = natural.sum
      val sum = weights.sum
      natural.zip(weights).map((n, w) => math.max(n, total * w / sum))
    else natural

  /** blocks side by side; with weights, the row's natural width is
   * divided by weight and each block padded to its share — "the
   * terminal divides width by weight". A block whose text asked for
   * `Align.End` is padded on the LEFT instead, which is what makes a
   * column of numbers comparable down the page */
  private def beside(blocks: Vector[Vector[String]], weights: Vector[Int], sep: String,
                     aligns: Vector[Align], shares: Vector[Int]): Vector[String] =
    val height = blocks.map(_.length).maxOption.getOrElse(0)
    val widths = columns(blocks, weights, shares)
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
    focus.flatMap { f =>
      val i = Ui.focusable(ui).indexOf(f)
      if i < 0 then None else laid(ui, focus, width)._2.lift(i).map(_.row)
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
   * WHICH FOCUSABLE WIDGET IS AT A CELL — a lookup in the map `laid`
   * answers (ui-terminal-layout-map).
   *
   * It used to SEARCH the frame for the text each focusable draws,
   * scanned in focus order, and carried two limits for it: a widget
   * whose text wrapped was found by its first line only, and two
   * widgets that render identically were told apart by order alone.
   * Both are gone — a placement knows its height, and two identical
   * widgets are two placements.
   */
  def hit(ui: Ui, row: Int, col: Int, width: Int = 0): Option[Int] =
    val places = laid(ui, None, width)._2
    val i = places.indexWhere(_.holds(row, col))
    if i < 0 then None else Some(i)

  /** a line without its ANSI escapes — what a reader's columns count */
  private def strip(s: String): String = s.replaceAll(Esc + "\\[[0-9;]*m", "")

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
    // a mouse press at a cell of the frame (ui-terminal-mouse), 0-based
    // — a key like any other, so the host still reads ONE door
    case Click(row: Int, col: Int)
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
    /** ESC [ < … — an SGR mouse report, ended by M (press) or m
     * (release); the digits are `button;column;row` */
    case Mouse(ds: String)

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
        if c == '<' then (KeyState.Mouse(""), Vector.empty)
        else if c.isDigit then (KeyState.Digits(c.toString), Vector.empty)
        else (KeyState.Plain, Vector(named(c)))
      case KeyState.Mouse(ds) =>
        // a report ends at M or m and NOWHERE else: bailing out on the
        // first odd byte would leak the rest of it into the stream as
        // keystrokes, which is a damaged report typing for the user
        if c == 'M' then (KeyState.Plain, Vector(click(ds)))
        // a RELEASE is not a click: a press names the cell, and
        // reporting both would deliver every click twice
        else if c == 'm' then (KeyState.Plain, Vector.empty)
        else (KeyState.Mouse(ds + c), Vector.empty)
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

  /** `button;column;row`, 1-based on the wire and 0-based here; only
   * the LEFT button (0) is a click, and anything else — a wheel, a
   * drag, a damaged report — is dropped rather than guessed at */
  private def click(ds: String): Key = ds.split(";") match
    case Array(b, x, y) =>
      (b.toIntOption, x.toIntOption, y.toIntOption) match
        case (Some(0), Some(col), Some(row)) if col > 0 && row > 0 => Key.Click(row - 1, col - 1)
        case _ => Key.Unknown
    case _ => Key.Unknown

  /** the numbered forms: `ESC [ 1 ~` is Home on some terminals, `4 ~`
   * End, `7 ~`/`8 ~` on others */
  private def tilde(ds: String): Key = ds match
    case "1" | "7" => Key.Home
    case "4" | "8" => Key.End
    case "5" => Key.PageUp
    case "6" => Key.PageDown
    case _ => Key.Unknown

  /** a value with the character under the caret in reverse video; at
   * the end of the value the caret is a reversed space */
  private def carets(shown: String, caret: Int): String =
    val at = math.max(0, math.min(caret, shown.length))
    val here = if at < shown.length then shown.charAt(at).toString else " "
    shown.take(at) + s"$Esc[7m$here$Esc[27m" + shown.drop(at + 1)

  /**
   * AN EDIT AT A CARET: the key, the value it is editing and where the
   * caret is - the caret after it, and the value when the key changed
   * one (ui-terminal-caret).
   *
   * v1 appended and backspaced at the END of the value, and that is
   * THIS function with the caret there, so there is one implementation
   * and not two: a host without a caret is a host whose caret never
   * moved.
   */
  def edit(value: String, caret: Int, key: Key): (Int, Option[String]) =
    val at = math.max(0, math.min(caret, value.length))
    key match
      case Key.Left => (math.max(at - 1, 0), None)
      case Key.Right => (math.min(at + 1, value.length), None)
      case Key.Home => (0, None)
      case Key.End => (value.length, None)
      // DEL and BS, by their codes rather than as escapes in the
      // source: the same two `interpret` has always erased on
      case Key.Ch(c) if c == 127.toChar || c == 8.toChar =>
        if at == 0 then (0, None)
        else (at - 1, Some(value.take(at - 1) + value.drop(at)))
      case Key.Ch(c) if !c.isControl =>
        (at + 1, Some(value.take(at) + c + value.drop(at)))
      case _ => (at, None)

  /**
   * The host's door: a key against the tree AT a caret - the next
   * focus, the next caret, and what the key meant. A caret of -1 is
   * "this host has none", and then this is `interpret` exactly.
   *
   * WHICH KEYS BELONG TO THE CARET, and it is a decision rather than
   * an accident: while an `Input` has the focus, Left/Right/Home/End
   * move the CARET - what every editor does, and what the keys lane
   * deliberately left room for when it gave Left/Right to `Select` and
   * Home/End to the tab order. Everywhere else they still do that.
   */
  def interpretAt(ui: Ui, focus: Int, caret: Int, key: Key): (Int, Int, Option[Event]) =
    Ui.focusable(ui).lift(focus) match
      case Some(Input(v, k, _, _, _)) if caret >= 0 && caretKey(key) =>
        val (at, edited) = edit(v, caret, key)
        (focus, at, edited.map(next => Event.Edited(k, next)))
      case _ =>
        val (nf, ev) = interpret(ui, focus, key)
        (nf, if caret < 0 then caret else caretEnd(ui, nf), ev)

  /**
   * WHICH KEYS THE CARET TAKES when an `Input` has the focus. Stated
   * as a SET rather than read off whether `edit` changed anything:
   * `End` at the end of a value changes nothing, and if that fell
   * through to the tree it would jump the focus — so pressing End
   * twice would do two unrelated things, which is not a keyboard
   * anyone can learn.
   */
  private def caretKey(key: Key): Boolean = key match
    case Key.Left | Key.Right | Key.Home | Key.End => true
    case Key.Ch(c) => !c.isControl || c == 127.toChar || c == 8.toChar
    case _ => false

  /**
   * The caret against a tree that has just arrived: kept where it is
   * when the focus is still an `Input` (an edit rebuilds the tree on
   * every keystroke, and a caret that jumped to the end on each one
   * would make typing in the middle impossible), clamped to the value
   * it is now in, and the END for a widget that has just taken the
   * focus.
   */
  def clampCaret(ui: Ui, focus: Int, caret: Int): Int =
    Ui.focusable(ui).lift(focus) match
      case Some(Input(v, _, _, _, _)) =>
        if caret < 0 then v.length else math.min(caret, v.length)
      case _ => caret

  /** the caret a newly focused widget starts with: the end of its
   * value, so typing appends - v1's behaviour, kept as the default */
  def caretEnd(ui: Ui, focus: Int): Int =
    Ui.focusable(ui).lift(focus) match
      case Some(Input(v, _, _, _, _)) => v.length
      case _ => 0

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
      // a click FOCUSES what is under it, and then means what pressing
      // that widget means — which is Enter's answer, so it is Enter's
      // code that answers it and there is no second table of meanings
      case Key.Click(row, col) => hit(ui, row, col) match
        case Some(i) => (i, interpret(ui, i, Key.Ch('\n'))._2)
        case None => (focus, None)

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
