package okay.ui

import okay.*
import okay.given

/**
 * Stage 0 of specs/frontend.md: two vocabulary levels in one tree.
 * The laws that make them one — a semantic node's keys equal its
 * lowering's, the diff commutes with lowering, and diff-then-patch
 * holds on every new node — plus each host drawing the new level L.
 */
class TestVocab extends munit.FunSuite {

  import Ui.*

  private val Esc = "\u001b"

  def apply(from: Ui, patches: Vector[Patch]): Ui = patches.foldLeft(from)(Ui.patch)

  // a screen using every semantic node, keyed throughout
  def screen(salt: Int, tab: Int = 0, rows: Int = 2): Ui =
    Box(Vector(
      Text(s"title $salt", Style(tone = Tone.Emphasis, size = Size.Large)),
      Form(Vector(
        Input("ada", "name", "Name"),
        Input("", "pw", "Password", InputKind.Secret),
        Check(salt % 2 == 0, "ok", "Ok")), "Save", "f"),
      Items((0 until rows).toVector.map(i => Box(Vector(Text(s"item $i $salt"), Button("open", s"i$i")),
        Dir.Horizontal, key = s"it$i")), "list"),
      Table(Vector("a", "b"), Vector(
        Vector(Text("1"), Button("x", "t1")),
        Vector(Text(salt.toString), Button("y", "t2"))), "tbl"),
      Tabs(Vector("one", "two"), tab, Vector(Button("first", "p1"), Button("second", "p2")), "tabs"),
      Modal("Sure?", Box(Vector(Button("yes", "yes"), Button("no", "no")), Dir.Horizontal, gap = 1), "m"),
      Scroll(Image("/x.png", "an image"), "sc")),
      Dir.Vertical, weights = Vector.empty, gap = 1, pad = 2, key = "root")

  test("lower is total: no semantic node survives an empty vocabulary; level L is a fixed point") {
    def semanticNodes(u: Ui): Int = u match
      case Row(c, _) => c.map(semanticNodes).sum
      case Column(c, _) => c.map(semanticNodes).sum
      case Box(c, _, _, _, _, _) => c.map(semanticNodes).sum
      case Scroll(c, _) => semanticNodes(c)
      case _: Form | _: Items | _: Table | _: Tabs | _: Modal => 1
      case _ => 0
    val low = Ui.lower(screen(0), Set.empty)
    assertEquals(semanticNodes(low), 0)
    assertEquals(Ui.lower(low, Set.empty), low)
    // a claimed node is sent as itself
    val partial = Ui.lower(screen(0), Set(Vocab.table))
    assertEquals(semanticNodes(partial), 1)
    assertEquals(Ui.lower(screen(0), Vocab.all), screen(0))
  }

  test("the keys law: keys(s) == keys(lower(s)) — update cannot tell how the client drew it") {
    for tab <- 0 to 1; salt <- 0 to 1 do
      val s = screen(salt, tab)
      assertEquals(Ui.keys(s), Ui.keys(Ui.lower(s, Set.empty)))
      assertEquals(Ui.keys(s), Ui.keys(Ui.lower(s, Set(Vocab.form, Vocab.tabs))))
    // only the SELECTED tab's page is a capability; the tab buttons always are
    val k0 = Ui.keys(screen(0, tab = 0))
    assert(k0("p1") && !k0("p2") && k0(Ui.tabKey("tabs", 1)), k0.toString)
    val k1 = Ui.keys(screen(0, tab = 1))
    assert(k1("p2") && !k1("p1"), k1.toString)
    // the form's submit is the form's key
    assert(k0("f") && k0("name") && k0("pw"))
    // and focusable agrees with keys on the interactive widgets
    assertEquals(Ui.focusable(screen(0)).flatMap(Ui.keyOf).toSet, k0)
  }

  test("diff-then-patch on every new node, and the diff commutes with lowering") {
    val pairs = Vector(
      (screen(0), screen(1)),                 // content edits everywhere
      (screen(0), screen(0, tab = 1)),        // tabs switch
      (screen(0), screen(0, rows = 3)),       // a keyed item appears
      (screen(1, rows = 3), screen(0, rows = 1)),
      (Box(Vector(Text("a")), Dir.Horizontal, Vector(1, 2)), Box(Vector(Text("a")), Dir.Horizontal, Vector(2, 1))),
      (Scroll(Text("x")), Scroll(Text("y"))),
      (Image("/a", "a"), Image("/b", "b")),
      (Modal("t", Text("a"), "m"), Modal("t", Text("b"), "m")),
      (Form(Vector(Input("", "n")), "go", "f"), Form(Vector(Input("v", "n")), "go", "f")))
    for (a, b) <- pairs do
      assertEquals(apply(a, Ui.diff(a, b)), b, s"the law broke on $a -> $b")
      for vocab <- Vector(Set.empty[String], Set(Vocab.form), Vocab.all) do
        val (la, lb) = (Ui.lower(a, vocab), Ui.lower(b, vocab))
        assertEquals(apply(la, Ui.diff(la, lb)), lb, s"lowered law broke ($vocab) on $a -> $b")
        // lowering commutes with patching: lower(patch(a)) == lower(b)
        assertEquals(Ui.lower(apply(a, Ui.diff(a, b)), vocab), lb)
    // narrowness: an edit inside a form is a SetValue, not a Replace,
    // and the same patch on the lowered pair has the same path
    val (fa, fb) = (Form(Vector(Input("", "n")), "go", "f"), Form(Vector(Input("v", "n")), "go", "f"))
    assertEquals(Ui.diff(fa, fb), Vector(Patch.SetValue(List(0), "v")))
    assertEquals(Ui.diff(Ui.lower(fa, Set.empty), Ui.lower(fb, Set.empty)), Ui.diff(fa, fb))
  }

  test("the wire round-trips every new shape") {
    for u <- Vector(screen(0), screen(1, 1, 3), Ui.lower(screen(0), Set.empty), Ui.lower(screen(0), Set(Vocab.table))) do
      assertEquals(Protocol.treeOf(Protocol.line(Protocol.Msg.Tree(u))), Some(u))
      assertEquals(Protocol.ofBytes(Protocol.bytes(Protocol.Msg.Tree(u))), Some(Protocol.Msg.Tree(u)))
    for p <- Ui.diff(screen(0), screen(1, 1, 3)) do
      assertEquals(Protocol.patchOf(Protocol.line(Protocol.Msg.Patch(p))), Some(p))
      assertEquals(Protocol.ofBytes(Protocol.bytes(Protocol.Msg.Patch(p))), Some(Protocol.Msg.Patch(p)))
  }

  test("the terminal: weights divide the row, gap and pad are spaces, tokens are the terminal's idiom") {
    val row = Box(Vector(Text("ab"), Text("cd")), Dir.Horizontal, weights = Vector(1, 3))
    // natural width 4, shares 1 and 3: "ab" keeps 2, "cd" is padded to 3
    assertEquals(Frame.render(row), Vector("abcd "))
    assertEquals(Frame.render(Box(Vector(Text("a"), Text("b")), Dir.Horizontal, gap = 2)), Vector("a  b"))
    assertEquals(Frame.render(Box(Vector(Text("a"), Text("b")), Dir.Vertical, gap = 1, pad = 1)),
      Vector(" a", " ", " b"))
    assertEquals(Frame.render(Text("x", Style(tone = Tone.Emphasis))), Frame.render(Text("x", Style(bold = true))))
    assertEquals(Frame.render(Text("x", Style(tone = Tone.Muted))), Frame.render(Text("x", Style(dim = true))))
    assertEquals(Frame.render(Text("x", Style(tone = Tone.Danger))), Vector(s"$Esc[31mx$Esc[0m"))
    assertEquals(Frame.render(Input("secret", "pw", kind = InputKind.Secret)), Vector("[******]"))
    assertEquals(Frame.render(Image("/x", "photo")), Vector("[image: photo]"))
    // a semantic node draws as its lowering
    val tabs = Tabs(Vector("one", "two"), 1, Vector(Text("A"), Text("B")), "t")
    assertEquals(Frame.render(tabs), Frame.render(Ui.lower(tabs, Set.empty)))
    assertEquals(Frame.render(tabs), Vector("[ one ][=two=]", "B"))
  }

  test("React: weights are flex, tokens are classes, kinds are input types, semantics lower") {
    val e = React.elem(Box(Vector(Text("a", Style(tone = Tone.Danger, size = Size.Small)), Text("b")),
      Dir.Horizontal, weights = Vector(1, 2), gap = 1, key = "k"))
    assertEquals(e.props, Vector("data-key" -> "k", "className" -> "okay-box okay-h", "style" -> "gap:1ch", "data-w" -> "1 2"))
    assertEquals(e.children(0).props, Vector("className" -> "okay-tone-danger okay-size-small", "style" -> "flex:1"))
    assertEquals(e.children(1).props, Vector("style" -> "flex:2"))
    assertEquals(React.elem(Input("", "pw", kind = InputKind.Secret)).props,
      Vector("data-key" -> "pw", "type" -> "password", "value" -> ""))
    assertEquals(React.elem(Input("", "n", kind = InputKind.Multiline)).tag, "textarea")
    assertEquals(React.elem(Image("/x", "y")), Elem("img", Vector("src" -> "/x", "alt" -> "y")))
    assertEquals(React.elem(Button("go", "g", Role.Primary)).props, Vector("data-key" -> "g", "className" -> "okay-primary"))
    val form = Form(Vector(Input("", "n")), "Save", "f")
    assertEquals(React.elem(form), React.elem(Ui.lower(form, Set.empty)))
    // events through a semantic node reach the same keys
    assertEquals(React.event(form, "f", "click", ""), Some(Event.Pressed("f")))
    assertEquals(React.event(form, "n", "input", "x"), Some(Event.Edited("n", "x")))
  }

  test("Wire.serve lowers for the client's vocabulary: a level-L client never sees a semantic node") {
    val view: Int => Ui = n => Form(Vector(Input(n.toString, "n")), "Save", "f")
    def first(vocab: Set[String]): String =
      val (out, _) = !.run(Writer.run(through(Writer.of(List(Protocol.line(Protocol.hello(vocab)))))(
        Wire.serve(0)(view)((s, _) => s))))
      out.head
    assert(!first(Set.empty).contains("\"Form\""), first(Set.empty))
    assert(first(Set(Vocab.form)).contains("\"Form\""), first(Set(Vocab.form)))
  }
}
