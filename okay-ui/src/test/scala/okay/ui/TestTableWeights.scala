package okay.ui

/**
 * ui-table-weights: a table says how wide its columns are.
 *
 * The lowering used to hand every column ONE share, which is not a
 * default a caller can override but the only thing the node could
 * express — and no stylesheet can correct it afterwards, because
 * `React` writes a `Box`'s weights INLINE on each child. Empty still
 * means equal, so nothing that does not ask for widths changes.
 */
class TestTableWeights extends munit.FunSuite:

  import Ui.*

  private def cells(row: Ui): Vector[Int] = row match
    case Box(_, _, w, _, _, _) => w
    case other => fail(s"a lowered row is a Box, got $other")

  private def rowsOf(lowered: Ui): Vector[Vector[Int]] = lowered match
    case Box(children, Dir.Vertical, _, _, _, _) => children.map(cells)
    case other => fail(s"a lowered table is a vertical Box, got $other")

  private val header = Vector("case", "subject", "evidence")
  private def body = Vector(
    Vector(Text("C-1"), Text("0xabc"), Text("4 transfers in 10 minutes")),
    Vector(Text("C-2"), Text("0xdef"), Text("a name on the list")))

  test("no weights is what it always was: every column one share") {
    val low = Ui.lower(Table(header, body, "t"), Set.empty)
    // the head row and both body rows
    assertEquals(rowsOf(low), Vector.fill(3)(Vector(1, 1, 1)))
  }

  test("weights reach the head row AND every body row") {
    val low = Ui.lower(Table(header, body, "t", Vector(3, 1, 9)), Set.empty)
    assertEquals(rowsOf(low), Vector.fill(3)(Vector(3, 1, 9)))
  }

  test("React writes them inline, which is the half a stylesheet cannot do") {
    val html = Html.render(Table(header, body, "t", Vector(3, 1, 9)))
    assert(html.contains("""style="flex:3""""), html)
    assert(html.contains("""style="flex:9""""), html)
  }

  /** a tree is data that may arrive over a wire from anywhere, so a
   * mis-sized table draws evenly rather than throwing */
  test("a weights vector of the wrong length is ignored, not obeyed and not fatal") {
    for wrong <- Vector(Vector(1), Vector(1, 2), Vector(1, 2, 3, 4)) do
      assertEquals(clue(rowsOf(Ui.lower(Table(header, body, "t", wrong), Set.empty))),
        Vector.fill(3)(Vector(1, 1, 1)))
  }

  test("a claiming client is handed the weights rather than the lowering") {
    val t = Table(header, body, "t", Vector(3, 1, 9))
    assertEquals(Ui.lower(t, Set(Vocab.table)), t)
  }

  test("the laws hold: keys are unchanged, and the tree still maps and rebuilds") {
    val t = Table(header, body, "t", Vector(3, 1, 9))
    // weights are layout, never capability
    assertEquals(Ui.keys(t), Ui.keys(Ui.lower(t, Set.empty)))
    assertEquals(Ui.keys(t), Ui.keys(Table(header, body, "t")))
    // map carries them through, both when it changes nothing and when
    // it rewrites every cell (which is the path that rebuilds the node
    // from its children)
    assertEquals(Ui.map(t, identity), t)
    val shouted = Ui.map(t, { case Text(s, st) => Text(s.toUpperCase, st); case u => u })
    assertEquals(shouted, Table(header, body.map(_.map {
      case Text(s, st) => Text(s.toUpperCase, st); case u => u
    }), "t", Vector(3, 1, 9)))
  }

  test("two tables differing only in weights are DIFFERENT — a widening is a change to draw") {
    val even = Table(header, body, "t")
    val wide = Table(header, body, "t", Vector(3, 1, 9))
    assertNotEquals(even, wide)
    assertNotEquals(Ui.lower(even, Set.empty), Ui.lower(wide, Set.empty))
    assert(Ui.diff(even, wide).nonEmpty, "a widening produced no patch")
  }

  test("the wire: weights round-trip, and a table without them encodes as it always did") {
    val wide = Table(header, body, "t", Vector(3, 1, 9))
    assertEquals(Protocol.treeOf(Protocol.line(Protocol.Msg.Tree(wide))), Some(wide))
    val even = Table(header, body, "t")
    assertEquals(Protocol.treeOf(Protocol.line(Protocol.Msg.Tree(even))), Some(even))
  }
