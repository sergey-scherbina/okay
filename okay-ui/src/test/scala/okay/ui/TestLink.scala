package okay.ui

/** ui-link: navigation in the vocabulary. A Link carries no key —
 * going somewhere is the client's own act, not a capability — and is
 * DEFINED by its lowering, like every level-S node.
 */
class TestLink extends munit.FunSuite:

  private val link = Ui.Link("cases .csv", "/api/cases.csv?of=cases")

  test("the law: a Link's keys are its lowering's, and both are empty") {
    assertEquals(Ui.keys(link), Set.empty[String])
    assertEquals(Ui.keys(Ui.lower(link, Set.empty)), Ui.keys(link))
    // ...so nothing about it may ever be posted
    assert(!Wire.permitted(link, Event.Pressed("cases .csv")))
    assert(!Wire.permitted(Ui.Column(Vector(link)), Event.Pressed("/api/cases.csv?of=cases")))
  }

  test("the lowering says what it means where nothing can be clicked: the label AND where it points") {
    assertEquals(Ui.lower(link, Set.empty), Ui.Text("cases .csv — /api/cases.csv?of=cases"))
    // a claiming client keeps the node itself
    assertEquals(Ui.lower(link, Set(Ui.Vocab.link)), link)
    // an empty href is just a label
    assertEquals(Ui.lower(Ui.Link("nowhere", ""), Set.empty), Ui.Text("nowhere"))
  }

  test("a browser claims it: an anchor in React's element tree and in the HTML host") {
    assertEquals(React.elem(link), Elem("a", Vector("href" -> "/api/cases.csv?of=cases"), text = Some("cases .csv")))
    assertEquals(Html.render(link), """<a href="/api/cases.csv?of=cases">cases .csv</a>""")
    // the href is escaped like any other attribute
    assert(Html.render(Ui.Link("x", "/a?b=1&c=\"2\"")).contains("&amp;") , Html.render(Ui.Link("x", "/a?b=1&c=\"2\"")))
  }

  test("a terminal shows the lowering, so an analyst can still copy the URL") {
    val frame = Frame.render(Ui.Column(Vector(link)), None).mkString("\n")
    assert(frame.contains("cases .csv") && frame.contains("/api/cases.csv?of=cases"), frame)
  }

  test("it crosses the wire: the derived Schema spells it like every other node") {
    val line = Protocol.line(Protocol.Msg.Tree(link))
    assertEquals(Protocol.treeOf(line), Some(link))
  }

  test("inside a tree it is invisible to the fold: no focusable, no form field") {
    val tree = Ui.Column(Vector(link, Ui.Button("go", "go")))
    assertEquals(Ui.focusable(tree).size, 1)
    assertEquals(Ui.keys(tree), Set("go"))
  }
