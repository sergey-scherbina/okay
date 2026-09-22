package okay.ui

import okay.*
import okay.given

/**
 * specs/zipper.md — `Plate[Ui]` against `Ui.path`: the cursor's path
 * affine and the hand-written one agree on every node where the
 * structural walk and the patch convention name the same children,
 * and where they do not, this file says exactly how.
 */
class TestUiZipper extends munit.FunSuite {

  import Ui.*

  val rnd = scala.util.Random(20260909)
  def tree(salt: Int, tab: Int = 0, open: Boolean = true): Ui =
    Box(Vector(
      Text(s"title $salt"),
      Form(Vector(Input("v", "name", "Name"), Check(salt % 2 == 0, "ok", "Ok")), "Save", "f"),
      Items((0 until 2).toVector.map(i => Row(Vector(Text(s"i$i"), Button("x", s"b$i")), key = s"it$i")), "list"),
      Table(Vector("a", "b"), Vector(Vector(Text("1"), Button("y", "t1")), Vector(Text("2"), Button("z", "t2"))), "tbl"),
      Tabs(Vector("one", "two"), tab, Vector(Text("A"), Form(Vector(Input("", "hidden", "H")), "Go", "hf")), "tabs"),
      Disclosure("more", open, Button("deep", "deep"), "d"),
      Scroll(Text("s"), "sc")),
      Dir.Vertical, key = "root")

  /** every root-first path into `u` by the zipper's own plate */
  def paths(u: Ui, prefix: List[Int] = Nil): Vector[List[Int]] =
    prefix +: Ui.plate.children(u).zipWithIndex.flatMap((k, i) => paths(k, prefix :+ i))

  /** the nodes on which `childAt` (the patch path) and `kidsOf` (the
   * structural walk) name the same i-th child */
  def agreeing(u: Ui): Boolean = u match
    case _: Row | _: Column | _: Box | _: Scroll | _: Form | _: Items => true
    case _ => false

  /** a path passes only through agreeing nodes (the last node may be anything) */
  def agrees(u: Ui, p: List[Int]): Boolean = p match
    case Nil => true
    case i :: rest => agreeing(u) && Ui.plate.children(u).lift(i).exists(agrees(_, rest))

  test("Zipper.at agrees with Ui.path — preview and set — on every path through agreeing nodes") {
    val mark = Text("MARK")
    for salt <- 0 to 2; tab <- 0 to 1; open <- Vector(true, false) do
      val t = tree(salt, tab, open)
      val checked = paths(t).filter(agrees(t, _))
      assert(checked.length > 10, "the trees must give the law something to check")
      for p <- checked do
        assertEquals(Zipper.at[Ui](p).preview(t), Ui.path(p).preview(t), s"preview at $p")
        assertEquals(Zipper.at[Ui](p).set(mark)(t), Ui.path(p).set(mark)(t), s"set at $p")
      // and past the children, both refuse
      for p <- Vector(List(9), List(0, 0), List(2, 5)) do
        assertEquals(Zipper.at[Ui](p).preview(t), None)
        assertEquals(Ui.path(p).preview(t), None)
  }

  test("where they part: a Modal's body is child 1 on the patch path and child 0 for the zipper; a Table has rows for the zipper and nothing for the path") {
    val body = Text("body")
    val m = Modal("t", body, "m")
    assertEquals(Ui.path(List(1)).preview(m), Some(body))
    assertEquals(Ui.path(List(0)).preview(m), None)
    assertEquals(Zipper.at[Ui](List(0)).preview(m), Some(body))
    assertEquals(Zipper.at[Ui](List(1)).preview(m), None)
    val tbl = Table(Vector("a"), Vector(Vector(Text("1")), Vector(Text("2"))), "tbl")
    assertEquals(Ui.path(List(0)).preview(tbl), None)
    assertEquals(Zipper.at[Ui](List(1)).preview(tbl), Some(Text("2")))
    // a Tabs page off screen is reachable — the structural walk, on purpose
    val tabs = Tabs(Vector("one", "two"), 0, Vector(Text("A"), Text("B")), "tabs")
    assertEquals(Zipper(tabs: Ui).at(List(1)).map(_.focus), Some(Text("B")))
  }

  test("an edit through the cursor is Ui.map's edit at that one node, and the rest is shared") {
    val t = tree(1)
    val z = Zipper(t).at(List(2, 1, 0)).get      // Items > second Row > its Text
    assertEquals(z.focus, Text("i1"))
    val edited = z.set(Text("I1")).root
    assertEquals(edited, Ui.path(List(2, 1, 0)).set(Text("I1"))(t))
    assert(Ui.plate.children(edited)(1) eq Ui.plate.children(t)(1))   // the Form, untouched
    assert(Ui.plate.children(edited)(3) eq Ui.plate.children(t)(3))   // the Table, untouched
  }
}
