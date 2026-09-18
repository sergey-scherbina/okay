package okay.ui

/**
 * ui-terminal-keys: a terminal sends `ESC [ A` for an arrow and
 * `ESC [ Z` for Shift-Tab, one BYTE per read, so naming what arrived
 * has to happen before the tree can be asked about it. Both halves are
 * values — a decoder and an interpretation — so both are tested with
 * no tty anywhere, which is the whole point of `Frame`.
 */
class TestTerminalKeys extends munit.FunSuite {

  import Ui.*
  import Frame.{Key, KeyState}

  /** every byte of a sequence, in order, as a host would read them */
  private def decode(bytes: Int*): Vector[Key] =
    bytes.foldLeft((KeyState.Plain: KeyState, Vector.empty[Key])) { case ((st, out), b) =>
      val (next, keys) = Frame.feed(st, b)
      (next, out ++ keys)
    }._2

  private val Esc = 27

  test("the sequences a terminal really sends, one byte at a time") {
    assertEquals(decode(Esc, '['.toInt, 'A'.toInt), Vector(Key.Up))
    assertEquals(decode(Esc, '['.toInt, 'B'.toInt), Vector(Key.Down))
    assertEquals(decode(Esc, '['.toInt, 'C'.toInt), Vector(Key.Right))
    assertEquals(decode(Esc, '['.toInt, 'D'.toInt), Vector(Key.Left))
    assertEquals(decode(Esc, '['.toInt, 'Z'.toInt), Vector(Key.BackTab))
    assertEquals(decode(Esc, '['.toInt, 'H'.toInt), Vector(Key.Home))
    assertEquals(decode(Esc, '['.toInt, 'F'.toInt), Vector(Key.End))
    // the numbered forms other terminals send
    assertEquals(decode(Esc, '['.toInt, '1'.toInt, '~'.toInt), Vector(Key.Home))
    assertEquals(decode(Esc, '['.toInt, '4'.toInt, '~'.toInt), Vector(Key.End))
    // an application-mode arrow (ESC O A) is the same key
    assertEquals(decode(Esc, 'O'.toInt, 'A'.toInt), Vector(Key.Up))
  }

  test("a plain character is itself, and a sequence yields nothing until it ends") {
    assertEquals(decode('a'.toInt), Vector(Key.Ch('a')))
    assertEquals(Frame.feed(KeyState.Plain, Esc)._2, Vector.empty[Key])
    assertEquals(Frame.feed(KeyState.Escaped, '['.toInt)._2, Vector.empty[Key])
  }

  test("a lone ESC is not swallowed: the key, then the byte that followed it") {
    assertEquals(decode(Esc, 'a'.toInt), Vector(Key.Ch(''), Key.Ch('a')))
    // and two ESCs in a row: the first is a key, the second still open
    assertEquals(decode(Esc, Esc, 'a'.toInt),
      Vector(Key.Ch(''), Key.Ch(''), Key.Ch('a')))
  }

  test("an unnamed sequence is Unknown, and the keys after it still arrive") {
    assertEquals(decode(Esc, '['.toInt, 'Q'.toInt), Vector(Key.Unknown))
    assertEquals(decode(Esc, '['.toInt, '9'.toInt, '~'.toInt), Vector(Key.Unknown))
    // one strange sequence does not swallow what follows
    assertEquals(decode(Esc, '['.toInt, 'Q'.toInt, 'x'.toInt), Vector(Key.Unknown, Key.Ch('x')))
  }

  private val tree = Column(Vector(
    Button("one", "b1"), Input("v", "in", "In"),
    Select(Vector("x", "y", "z"), 1, "sel"), Button("two", "b2")))

  test("Up, Down and Shift-Tab move the focus, and they wrap") {
    assertEquals(Frame.interpret(tree, 0, Key.Down)._1, 1)
    assertEquals(Frame.interpret(tree, 3, Key.Down)._1, 0, "Down wraps at the end")
    assertEquals(Frame.interpret(tree, 1, Key.Up)._1, 0)
    assertEquals(Frame.interpret(tree, 0, Key.Up)._1, 3, "Up wraps at the start")
    assertEquals(Frame.interpret(tree, 2, Key.BackTab)._1, 1)
    // and they say nothing, exactly as Tab always has
    assertEquals(Frame.interpret(tree, 0, Key.Down)._2, None)
  }

  test("Home and End go to the ends of the tab order") {
    assertEquals(Frame.interpret(tree, 2, Key.Home)._1, 0)
    assertEquals(Frame.interpret(tree, 1, Key.End)._1, 3)
    // a tree with nothing focusable answers 0 rather than a negative index
    assertEquals(Frame.interpret(Text("just words"), 0, Key.End)._1, 0)
  }

  test("Left and Right choose within a Select — and are LEFT ALONE in an Input") {
    assertEquals(Frame.interpret(tree, 2, Key.Right)._2, Some(Event.Chosen("sel", 2)))
    assertEquals(Frame.interpret(tree, 2, Key.Left)._2, Some(Event.Chosen("sel", 0)))
    // at the ends, nothing
    val atEnd = Column(Vector(Select(Vector("x"), 0, "one")))
    assertEquals(Frame.interpret(atEnd, 0, Key.Right)._2, None)
    assertEquals(Frame.interpret(atEnd, 0, Key.Left)._2, None)
    // an Input keeps them for the caret this host does not have yet
    assertEquals(Frame.interpret(tree, 1, Key.Right), (1, None))
    assertEquals(Frame.interpret(tree, 1, Key.Left), (1, None))
  }

  /**
   * ui-terminal-width: `Resized` existed as an event no host consumed,
   * and a row divided its NATURAL width by weight — never the
   * screen's. With a budget it divides the screen, and a value too
   * long for its column WRAPS rather than running past the edge,
   * which is the terminal's spelling of the rule the browser's
   * stylesheet already states.
   */
  test("a width budget divides the SCREEN by weight, and the shares add up to it") {
    val row = Box(Vector(Text("id"), Text("note")), Dir.Horizontal, weights = Vector(1, 3))
    val lines = Frame.render(row, None, 40)
    assertEquals(lines.length, 1)
    assertEquals(Frame.width(lines.head), 40, lines.head)
    // 1:3 of 40 is 10 and 30
    assert(lines.head.startsWith("id" + " " * 8), s"[${lines.head}]")
    // no budget is v1's layout, unchanged — this Box has gap 0, so
    // its columns touch, exactly as they did before there was a budget
    assertEquals(Frame.render(row, None), Vector("idnote"))
    // and a Row, which separates by a space, is unchanged too
    assertEquals(Frame.render(Row(Vector(Text("id"), Text("note"))), None), Vector("id note"))
  }

  test("a value too long for its column wraps — prose at a space, an identifier anywhere") {
    val prose = Frame.render(Text("the quick brown fox jumps"), None, 10)
    assertEquals(prose, Vector("the quick", "brown fox", "jumps"))
    // an IBAN has no space to break at, so it breaks at the budget —
    // and every character survives, which is the point
    val iban = "DE89370400440532013000"
    val wrapped = Frame.render(Text(iban), None, 10)
    assertEquals(wrapped.mkString, iban)
    assert(wrapped.forall(_.length <= 10), wrapped.toString)
  }

  test("the budget reaches through the containers a page is made of") {
    val page = Column(Vector(
      Box(Vector(Text("a b c d e f g h")), Dir.Vertical, pad = 2),
      Scroll(Text("x y z w v u t s"), "sc")))
    val lines = Frame.render(page, None, 10)
    // pad takes 2 from each side, so the text inside wraps at 6
    assert(lines.exists(l => l.startsWith("  ") && Frame.width(l) <= 10), lines.toString)
    assert(lines.forall(l => Frame.width(l) <= 10), lines.toString)
  }

  test("the budget reaches through a lowering, which is where a table's columns are") {
    // the compiler found this one: the semantic catch-all and `Form`
    // both recursed WITHOUT the budget, so a table — the node the
    // budget exists for — laid itself out naturally and ran past the
    // screen
    val t = Table(Vector("id", "note"),
      Vector(Vector(Text("c-1"), Text("a sentence that is long"))), "t", Vector(1, 4))
    val lines = Frame.render(t, None, 24)
    assert(lines.forall(l => Frame.width(l) <= 24), lines.mkString("|"))
    assert(lines.exists(_.contains("sentence")), lines.mkString("|"))
    val form = Ui.Form(Vector(Text("a b c d e f g h i j k")), "Save", "f")
    assert(Frame.render(form, None, 12).forall(l => Frame.width(l) <= 12))
  }

  /**
   * ui-terminal-scroll: a frame taller than the screen. The view is
   * the HOST's — the tree says nothing about it — so the pure half is
   * three functions a host composes: which line the focus is on, the
   * frame clipped to a screen, and the top that keeps a line visible.
   */
  test("PgUp and PgDn are decoded, and they move no focus and say nothing") {
    assertEquals(decode(Esc, '['.toInt, '5'.toInt, '~'.toInt), Vector(Key.PageUp))
    assertEquals(decode(Esc, '['.toInt, '6'.toInt, '~'.toInt), Vector(Key.PageDown))
    assertEquals(Frame.interpret(tree, 1, Key.PageUp), (1, None))
    assertEquals(Frame.interpret(tree, 1, Key.PageDown), (1, None))
  }

  test("focusLine finds the focused widget by the ONE thing focus changes") {
    val order = Ui.focusable(tree)
    assertEquals(Frame.focusLine(tree, order.lift(0)), Some(0))
    // the Input is the second line of this column, the Select the third
    assertEquals(Frame.focusLine(tree, order.lift(1)), Some(1))
    assertEquals(Frame.focusLine(tree, order.lift(2)), Some(2))
    assertEquals(Frame.focusLine(tree, None), None)
  }

  test("clip shows a screen of the frame, and pads a short one") {
    val lines = (0 until 10).toVector.map(i => s"line $i")
    assertEquals(Frame.clip(lines, 0, 3), Vector("line 0", "line 1", "line 2"))
    assertEquals(Frame.clip(lines, 4, 3), Vector("line 4", "line 5", "line 6"))
    // a top past the end is pulled back, never a gap of nothing
    assertEquals(Frame.clip(lines, 99, 3), Vector("line 7", "line 8", "line 9"))
    // a frame SHORTER than the screen is padded, so the last paint
    // does not show through under it
    assertEquals(Frame.clip(Vector("a"), 0, 3), Vector("a", "", ""))
    // no screen is the frame itself
    assertEquals(Frame.clip(lines, 5, 0), lines)
  }

  test("follow moves the view as little as it can, and only when it must") {
    assertEquals(Frame.follow(0, 2, 5), 0, "already visible: do not move")
    assertEquals(Frame.follow(0, 7, 5), 3, "off the bottom: just enough")
    assertEquals(Frame.follow(6, 2, 5), 2, "off the top: to the line")
    assertEquals(Frame.follow(3, 9, 0), 3, "no screen: nothing to follow")
  }

  test("every key the v1 char road knew still means what it meant") {
    assertEquals(Frame.interpret(tree, 0, '\t'), Frame.interpret(tree, 0, Key.Ch('\t')))
    assertEquals(Frame.interpret(tree, 0, '\n')._2, Some(Event.Pressed("b1")))
    assertEquals(Frame.interpret(tree, 1, 'x')._2, Some(Event.Edited("in", "vx")))
    assertEquals(Frame.interpret(tree, 2, '>')._2, Some(Event.Chosen("sel", 2)))
  }
}
