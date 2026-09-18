package okay.ui

/**
 * ui-browser-vocab (specs/ui-product.md stage 1): the browser claims
 * the one semantic node whose element structure costs the patch
 * consumers nothing, and the three renderers that serve a browser —
 * `React.elem`, `Html`, `LiveJs` — claim the SAME set, because a page
 * served both ways is one page only if they do.
 */
class TestBrowserVocab extends munit.FunSuite {

  import Ui.*

  private val table = Table(Vector("id", "note"),
    Vector(Vector(Text("c-1"), Text("a long sentence")),
           Vector(Text("c-2"), Link("open", "/c-2"))),
    "cases", Vector(1, 9))

  test("a Table is a real table: colgroup from the weights, a header, a body") {
    val html = Html.render(table)
    assert(html.startsWith("""<table data-key="cases" class="okay-table">"""), html)
    assert(html.contains("""<colgroup><col style="width:10%"><col style="width:90%"></colgroup>"""), html)
    assert(html.contains("""<thead><tr><th scope="col">id</th><th scope="col">note</th></tr></thead>"""), html)
    assert(html.contains("""<tbody><tr><td><span>c-1</span></td>"""), html)
    // `col` is a void element: a closing tag would not be HTML
    assert(!html.contains("</col>"), html)
    // and the cell keeps its own rendering — the link is still a link
    assert(html.contains("""<a href="/c-2">open</a>"""), html)
  }

  test("empty weights write no colgroup — the even split the lowering always gave") {
    val plain = Table(Vector("a"), Vector(Vector(Text("x"))), "t")
    assert(!Html.render(plain).contains("colgroup"), Html.render(plain))
    // and a vector that is not the header's length is ignored, as `lower` ignores it
    val wrong = Table(Vector("a", "b"), Vector(Vector(Text("x"), Text("y"))), "t", Vector(3))
    assert(!Html.render(wrong).contains("colgroup"), Html.render(wrong))
  }

  test("the two roads agree on every semantic node, nested and alone") {
    // the scriptless road renders the tree itself; the socket road
    // renders what `Wire.serve` sends a client whose hello says
    // exactly this — so the vocabulary is the ONE constant both read
    val trees = Vector[(String, Ui)](
      "Table" -> table,
      "Items" -> Items(Vector(Link("a", "/a")), "i"),
      "Tabs" -> Tabs(Vector("one"), 0, Vector(table), "tb"),
      "Modal" -> Modal("title", table, "m"),
      "Disclosure" -> Disclosure("more", true, table, "d"),
      "Table in Items" -> Items(Vector(table), "i"))
    for (name, tree) <- trees do
      assertEquals(Html.render(tree), clue(Html.render(Ui.lower(tree, React.Vocabulary))),
        s"the two roads disagree on a $name")
  }

  test("the capability list and the tab order do not move when a node is claimed") {
    // the law that lets `update` not know how the client drew it
    assertEquals(Ui.keys(table), Ui.keys(Ui.lower(table, Set.empty)))
    val withWidget = Table(Vector("h"), Vector(Vector(Button("go", "go"))), "t")
    assertEquals(Ui.keys(withWidget), Set("go"))
    assertEquals(Ui.focusable(withWidget).length, 1)
    assertEquals(React.event(withWidget, "go", "click", ""), Some(Event.Pressed("go")))
  }

  test("live.js builds every node the vocabulary claims, and says exactly that set") {
    val js = LiveJs.source
    for name <- React.Vocabulary do
      val shape = name.capitalize   // the protocol's case name for the node
      assert(js.contains(s"""case "$shape":"""), s"live.js claims $name and cannot build it")
    val expected = React.Vocabulary.toVector.sorted.map(v => "\"" + v + "\"").mkString(", ")
    assert(js.contains(s"""vocab: [$expected]"""), "live.js says a different set than it draws")
  }
}
