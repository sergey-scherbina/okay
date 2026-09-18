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

  test("every key the v1 char road knew still means what it meant") {
    assertEquals(Frame.interpret(tree, 0, '\t'), Frame.interpret(tree, 0, Key.Ch('\t')))
    assertEquals(Frame.interpret(tree, 0, '\n')._2, Some(Event.Pressed("b1")))
    assertEquals(Frame.interpret(tree, 1, 'x')._2, Some(Event.Edited("in", "vx")))
    assertEquals(Frame.interpret(tree, 2, '>')._2, Some(Event.Chosen("sel", 2)))
  }
}
