package okay.ui

/**
 * ui-html-css (specs/ui-product.md stage 4): the level-L stylesheet
 * lives beside the tree whose classes it names.
 *
 * The guard that matters is the first test: a token added to the tree
 * without a rule here is a FAILING TEST rather than a page that
 * quietly renders it unstyled — which is how a product ends up
 * writing the token map a second time.
 */
class TestHtmlCss extends munit.FunSuite {

  import Ui.*

  /** every class the renderer can write, from a tree that uses every
   * node and every token */
  private val everything: Ui = Column(Vector(
    Text("plain"),
    Text("b", Style(bold = true)), Text("d", Style(dim = true)),
    Text("e", Style(tone = Tone.Emphasis)), Text("m", Style(tone = Tone.Muted)),
    Text("x", Style(tone = Tone.Danger)),
    Text("s", Style(size = Size.Small)), Text("l", Style(size = Size.Large)),
    Text("id", Style(kind = Kind.Ident)), Text("7", Style(kind = Kind.Number)),
    Text("9", Style(align = Align.End)),
    Row(Vector(Text("r")), "r"),
    Box(Vector(Text("h")), Dir.Horizontal, key = "bh"),
    Box(Vector(Text("v")), Dir.Vertical, key = "bv"),
    Scroll(Text("sc"), "sc"),
    Image("/i.png", "i"),
    Button("p", "p", Role.Primary), Button("dg", "dg", Role.Danger),
    Button("ac", "ac", Role.Active), Button("pl", "pl"),
    Input("", "in", "In"), Check(false, "ck", "Ck"),
    Select(Vector("a"), 0, "sel"),
    Form(Vector(Input("", "f1", "F1")), "Save", "f"),
    Table(Vector("h"), Vector(Vector(Text("c"))), "t", Vector(1)),
    Link("go", "/go")), "app")

  private def classesOf(e: Elem): Set[String] =
    e.props.collectFirst { case ("className", c) => c.split(" ").toSet }.getOrElse(Set.empty) ++
      e.children.flatMap(classesOf)

  test("every class the renderer writes has a rule — a token without one fails here") {
    val written = classesOf(React.elem(everything))
    // the check is not vacuous: these three are really in the set the
    // walk found, and a name nothing writes is really absent from the
    // file (a `contains` that matched anything would pass either way)
    assert(written.contains("okay-table") && written.contains("okay-kind-ident") &&
      written.contains("okay-align-end"), written.toString)
    assert(written.size > 12, written.toString)
    assert(!Html.css.contains(".okay-nothing-writes-this"))
    val missing = written.filterNot(c => Html.css.contains("." + c))
    assertEquals(missing, Set.empty[String], s"classes with no rule: $missing")
  }

  test("the base rules are the ones a page must not have to write") {
    // a cell WRAPS and never ellipsizes; the header keeps word rules
    assert(Html.css.contains("overflow-wrap: anywhere"), Html.css)
    assert(Html.css.contains(".okay-table th") && Html.css.contains("overflow-wrap: normal"), Html.css)
    // an identifier is monospaced, a number's figures are tabular
    assert(Html.css.contains("font-variant-numeric: tabular-nums"), Html.css)
    // and a Box's own weights are never restated here: layout is the
    // tree's, and React writes the flex inline
    assert(!Html.css.contains("flex-grow"), Html.css)
  }

  test("theming is custom properties, not a fork: the six a product sets") {
    for v <- Vector("--okay-fg", "--okay-muted", "--okay-accent", "--okay-danger", "--okay-line", "--okay-base") do
      assert(Html.css.contains(v + ":"), s"$v is not a knob")
    // the tones READ the properties rather than naming a colour twice
    assert(Html.css.contains("color: var(--okay-danger)"), Html.css)
    assert(Html.css.contains("color: var(--okay-muted)"), Html.css)
  }
}
