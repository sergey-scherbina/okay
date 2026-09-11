package okay.ui

import okay.*
import okay.given

/**
 * The tree's optics (specs/optics.md stage 2): `everywhere`, `shown`,
 * `key` and `path`, the laws that tie each to the hand-written walk it
 * replaces, and the one place a traversal CANNOT do what `Ui.map` does.
 */
class TestUiOptic extends munit.FunSuite {

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

  test("everywhere IS map, for every f that keeps a node's children — and here is one that does not") {
    // the call sites' kind of f: local, shape-preserving
    val fs: Vector[Ui => Ui] = Vector(
      { case t: Text => t.copy(s = t.s.toUpperCase); case u => u },
      { case i: Input => i.copy(value = "x"); case u => u },
      { case b: Button => b.copy(label = b.label + "!"); case u => u },
      { case c: Check => c.copy(on = !c.on); case u => u },
      identity)
    for f <- fs; salt <- 0 to 2; tab <- 0 to 1 do
      val t = tree(salt, tab)
      assertEquals(everywhere.modify(f)(t), Ui.map(t, f), "the traversal and the rewrite disagreed")

    // and the difference, named: an `f` that REPLACES a node's children
    val one = Column(Vector(Text("a")), "c")
    val replace: Ui => Ui = { case Column(_, k) => Column(Vector(Text("REPLACED")), k); case u => u }
    assertEquals(Ui.map(one, replace), Column(Vector(Text("REPLACED")), "c"))   // bottom-up: f's children win
    assertEquals(everywhere.modify(replace)(one), Column(Vector(Text("a")), "c")) // top-down: the traversed ones do
    // which is why `everywhere` is top-down at all: a bottom-up rewrite
    // applies f to the REBUILT node, and that is a bind, not an
    // Applicative — a traversal cannot express it
  }

  test("key: every node a key names — one on a well-formed tree, all of them on a malformed one") {
    val t = tree(0)
    assertEquals(key("name").modify { case i: Input => i.copy(value = "typed"); case u => u }(t),
      Ui.map(t, { case i: Input if i.key == "name" => i.copy(value = "typed"); case u => u }))
    assertEquals(key("name").toVector(t).length, 1)
    assertEquals(key("nosuch").toVector(t), Vector.empty)
    // a key is unique on a well-formed tree — Ui.keys reads them as a
    // set — but a view is an ordinary function and nothing enforces it.
    // A traversal rewrites every match, which is what `map` did.
    val twice = Column(Vector(Input("a", "dup"), Input("b", "dup")), "c")
    assertEquals(key("dup").toVector(twice).length, 2)
    assertEquals(key("dup").modify { case i: Input => i.copy(value = "z"); case u => u }(twice),
      Column(Vector(Input("z", "dup"), Input("z", "dup")), "c"))
    assertEquals(Ui.keys(twice), Set("dup"))   // the capability list says one
  }

  test("path names exactly the node a Patch at that path touches") {
    val t = tree(0)
    val paths = Vector(List(0), List(1, 0), List(2, 0), List(6, 0), List(5, 1))
    for p <- paths do
      val focus = path(p).preview(t)
      assert(focus.isDefined, s"no focus at $p")
      // a SetText through Ui.patch and through the optic are the same edit
      val viaPatch = Ui.patch(t, Patch.SetText(p, "X"))
      val viaOptic = path(p).modify { case Text(_, st) => Text("X", st); case u => u }(t)
      assertEquals(viaOptic, viaPatch, s"the optic and the patch disagreed at $p")
    // a path into a leaf previews nothing, as the patch walk refuses it
    assertEquals(path(List(0, 0)).preview(t), None)
    assertEquals(path(List(99)).preview(t), None)
    assertEquals(path(Nil).preview(t), Some(t))
  }

  test("shown skips what is not on screen; everywhere does not — and that is why a hidden form cannot be submitted") {
    val t = tree(0, tab = 0, open = false)
    def keysOf(o: Traversal[Ui, Ui, Ui, Ui]) = o.toVector(t).flatMap(Ui.keyOf).toSet
    assert(keysOf(everywhere)("hf"), "the structural traversal sees the hidden tab's form")
    assert(!keysOf(shown)("hf"), "the shown traversal does not")
    assert(keysOf(everywhere)("deep"), "the structural traversal sees the closed disclosure's body")
    assert(!keysOf(shown)("deep"), "the shown traversal does not")
    // and the consequence, which is the capability rule: submit refuses
    assertEquals(Ui.submit(t, "hf"), None)
    assertEquals(Ui.submit(t, "f").isDefined, true)
    // open the tab and it is submittable — the same tree, one index apart
    assertEquals(Ui.submit(tree(0, tab = 1), "hf").isDefined, true)
  }

  test("shown agrees with the capability list it is the reading of") {
    for tab <- 0 to 1; open <- Vector(true, false) do
      val t = tree(0, tab, open)
      // every key `keys` admits is on a node `shown` visits
      val visible = shown.toVector(t).flatMap(Ui.keyOf).toSet
      val tabKeys = Set(Ui.tabKey("tabs", 0), Ui.tabKey("tabs", 1))
      assertEquals(Ui.keys(t) -- tabKeys -- visible, Set.empty[String],
        "a key was a capability but its node is not shown")
  }
}
