package okay2

import okay2.Optic._
import ZipperFixtures._

/** the cursor's laws over a rose tree, the plate built from a traversal
 * against the hand plate, and `State.zoom(Zipper.focus)` — the Scala 3
 * core's okay-optics TestZipper */
class TestZipper extends munit.FunSuite {

  val tree: Rose = Rose("root", Vector(
    Rose("a", Vector(Rose("a0"), Rose("a1", Vector(Rose("a10"))))),
    Rose("b"),
    Rose("c", Vector(Rose("c0"), Rose("c1"), Rose("c2")))))

  /** every root-first path into `t`, the root's own included */
  def paths(t: Rose, prefix: List[Int] = Nil): Vector[List[Int]] =
    prefix +: t.kids.zipWithIndex.flatMap { case (k, i) => paths(k, prefix :+ i) }

  def same(x: Zipper[Rose], y: Zipper[Rose]): Boolean = x.focus == y.focus && x.path == y.path && x.root == y.root

  def cursors: Vector[Zipper[Rose]] = paths(tree).flatMap(p => Zipper(tree).at(p))

  test("down then up is the input cursor, and the root is the input tree itself") {
    for (z <- cursors; i <- z.focus.kids.indices) {
      val back = z.down(i).flatMap(_.up).get
      assert(same(back, z))
      assert(back.root eq tree, s"a walk without edits rebuilt the tree at ${z.path}")
    }
  }

  test("moves that do not exist answer None; nothing throws") {
    val z = Zipper(tree)
    assertEquals(z.up, None)
    assertEquals(z.left, None)
    assertEquals(z.right, None)
    assertEquals(z.down(3), None)
    assertEquals(z.down(-1), None)
    assertEquals(z.at(List(0, 0, 0)), None)
    assertEquals(z.at(List(0, 1, 0)).map(_.focus.label), Some("a10"))
  }

  test("left then right, right then left: identity where both exist") {
    for (z <- cursors) {
      z.right.flatMap(_.left).foreach(back => assert(same(back, z)))
      z.left.flatMap(_.right).foreach(back => assert(same(back, z)))
    }
  }

  test("modify at a focus then root is the hand rebuild; every other subtree is shared") {
    val edited = Zipper(tree).at(List(2, 1)).get.modify(r => r.copy(label = r.label.toUpperCase)).root
    val expected = tree.copy(kids = tree.kids.updated(2, tree.kids(2).copy(kids = tree.kids(2).kids.updated(1, Rose("C1")))))
    assertEquals(edited, expected)
    assert(edited.kids(0) eq tree.kids(0))
    assert(edited.kids(1) eq tree.kids(1))
    assert(edited.kids(2).kids(0) eq tree.kids(2).kids(0))
  }

  test("an edit survives a sideways move and an edit below survives up") {
    val z = Zipper(tree).at(List(2, 0)).get.set(Rose("X")).right.get.set(Rose("Y")).root
    assertEquals(z.kids(2).kids.map(_.label), Vector("X", "Y", "c2"))
    val deep = Zipper(tree).at(List(0, 1, 0)).get.set(Rose("Z")).up.get.up.get.root
    assertEquals(deep.kids(0).kids(1).kids(0).label, "Z")
    assert(deep.kids(2) eq tree.kids(2))
  }

  test("at(path) is repeated down, and path reads it back") {
    for (p <- paths(tree)) {
      val byAt = Zipper(tree).at(p).get
      val byDown = p.foldLeft(Zipper(tree))((z, i) => z.down(i).get)
      assert(same(byAt, byDown))
      assertEquals(byAt.path, p)
      assertEquals(byAt.index, p.lastOption)
      assertEquals(byAt.isTop, p.isEmpty)
    }
  }

  test("Zipper.focus: GetPut, PutGet, PutPut at every depth") {
    val l = Zipper.focus[Rose]
    val v = Rose("v"); val w = Rose("w")
    for (z <- cursors) {
      assert(same(l.set(l.get(z))(z), z))
      assertEquals(l.get(l.set(v)(z)), v)
      assert(same(l.set(w)(l.set(v)(z)), l.set(w)(z)))
    }
  }

  test("Zipper.at is an affine on the tree: preview and set agree with the cursor") {
    for (p <- paths(tree) :+ List(9) :+ List(0, 0, 0)) {
      val a = Zipper.at[Rose](p)
      assertEquals(a.preview(tree), Zipper(tree).at(p).map(_.focus))
      assertEquals(a.set(Rose("N"))(tree), Zipper(tree).at(p).map(_.set(Rose("N")).root).getOrElse(tree))
    }
  }

  test("State.zoom(Zipper.focus): a program at the focus, the frames untouched") {
    val z = Zipper(tree).at(List(0, 1)).get
    val prog: Int ! State[Rose] = State.modify[Rose](r => r.copy(label = r.label + "!")).map(_.kids.length)
    val (after, n) = State.run(z)(State.zoom[Zipper[Rose], Rose, Int, Pure](Zipper.focus[Rose])(prog))
    assertEquals(n, 1)
    assertEquals(after.path, z.path)
    assertEquals(after.root, z.modify(r => r.copy(label = r.label + "!")).root)
  }

  test("Plate.of(traversal) agrees with the hand plate on every node") {
    val ofTr = Plate.of(kids)
    for (p <- paths(tree)) {
      val node = Zipper(tree).at(p).get.focus
      assertEquals(ofTr.children(node), hand.children(node))
      val replaced = node.kids.map(k => k.copy(label = k.label + "'"))
      assertEquals(ofTr.withChildren(node, replaced), hand.withChildren(node, replaced))
    }
    val viaTr = Zipper(tree).at(List(2, 1))(ofTr).get.set(Rose("C1")).root(ofTr)
    assertEquals(viaTr, Zipper(tree).at(List(2, 1)).get.set(Rose("C1")).root)
  }
}
