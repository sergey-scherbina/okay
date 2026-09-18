package okay.ui

import scala.scalajs.js

/**
 * THE BROWSER CLIENT, EXECUTED (ui-livejs-verified).
 *
 * Three renderers serve a browser — `React.elem`, `Html.render` and
 * `LiveJs.build` — and a page served both ways is one page only if
 * they agree. Until this file existed NOTHING RAN the third one. The
 * suite checked that the text of `live.js` CONTAINED `case "Table":`
 * and compared the two Scala renderers with each other; a wrong class
 * name, a missed style token or a wrong attribute passed green.
 *
 * So this runs `live.js` under Node against the same fake document
 * `TestDom` already uses for the Scala.js backend, turns what it
 * built back into an `Elem`, and compares that with `React.elem` of
 * the same tree. That catches what a shared name table never could:
 * a `Table`'s column percentages and a `Box`'s weights are
 * ARITHMETIC, and arithmetic is exactly what two implementations get
 * differently.
 */
class TestLiveJsDom extends munit.FunSuite {

  import Ui.*

  /** the document `live.js` builds into: `TestDom`'s own fake, plus
   * the three properties this client touches and the Scala.js backend
   * does not — `className`, `dataset` and `style` */
  class Doc extends js.Object {
    var created = 0
    def createElement(tag: String): js.Dynamic = {
      created += 1
      val n = FakeNode(tag).asInstanceOf[js.Dynamic]
      n.className = ""
      n.href = ""
      n.src = ""
      n.alt = ""
      n.`type` = ""
      n.dataset = js.Dictionary[String]().asInstanceOf[js.Any]
      n.style = js.Dictionary[String]().asInstanceOf[js.Any]
      n
    }
  }

  private lazy val built: js.Dynamic = {
    // `window` is made in JAVASCRIPT: Scala.js refuses to load the
    // global scope as a value, so the one line that aliases it is the
    // one line that has to be eval'd
    js.eval("globalThis.window = globalThis;")
    // and the document is INSTALLED by a function eval'd for the
    // purpose: assigning through `js.Dynamic.global` lands somewhere
    // the eval'd script's bare `document` does not see
    js.eval("(function (d) { globalThis.document = d; })")
      .asInstanceOf[js.Function1[js.Any, Unit]](new Doc().asInstanceOf[js.Any])
    js.eval(LiveJs.source)
    js.Dynamic.global.okayBuild
  }

  /** what `live.js` made, as the shape `React.elem` speaks.
   *
   * The mapping is deliberately dumb: every property the client sets
   * becomes the prop `React` names, in `React`'s own order, so a
   * disagreement is a disagreement about the TREE and never about
   * this function's taste. */
  def elemOf(n: js.Dynamic): Elem = {
    val tag = n.tagName.asInstanceOf[String].toLowerCase
    val props = Vector.newBuilder[(String, String)]
    val cls = Option(n.className.asInstanceOf[String]).getOrElse("")
    val dataset = n.dataset.asInstanceOf[js.Dictionary[String]]
    val style = n.style.asInstanceOf[js.Dictionary[String]]

    dataset.get("key").foreach(v => props += ("data-key" -> v))
    if (cls.nonEmpty) props += ("className" -> cls)
    val styleText = style.toSeq.sortBy(_._1).map((k, v) => s"$k:$v").mkString(";")
    if (styleText.nonEmpty) props += ("style" -> styleText)
    dataset.get("w").foreach(v => props += ("data-w" -> v))
    dataset.get("form").foreach(v => props += ("data-form" -> v))
    dataset.get("live").foreach(v => props += ("data-live" -> v))
    val href = Option(n.href.asInstanceOf[String]).getOrElse("")
    if (href.nonEmpty) props += ("href" -> href)
    val kind = Option(n.`type`.asInstanceOf[String]).getOrElse("")
    if (kind.nonEmpty) props += ("type" -> kind)
    val src = Option(n.src.asInstanceOf[String]).getOrElse("")
    if (src.nonEmpty) props += ("src" -> src)
    val alt = Option(n.alt.asInstanceOf[String]).getOrElse("")
    if (alt.nonEmpty) props += ("alt" -> alt)

    // AND WHAT WENT THROUGH setAttribute, which is how `src`, `alt`
    // and a `th`'s `scope` arrive — reading only properties made the
    // first run disagree about three nodes it in fact agreed on
    val attrs = n.attrList.asInstanceOf[String]
    if (attrs.nonEmpty) attrs.split(",").foreach { kv =>
      val i = kv.indexOf('=')
      if (i > 0) props += (kv.take(i) -> kv.drop(i + 1))
    }

    val kids = n.childNodes.asInstanceOf[js.Array[js.Dynamic]].toVector.map(elemOf)
    val text = Option(n.textContent.asInstanceOf[String]).filter(_.nonEmpty)
    Elem(tag, props.result(), kids, if (kids.isEmpty) text else None)
  }

  /** the two renderers, on one tree, compared as STRUCTURE.
   *
   * Only the properties both sides carry are compared — the client
   * sets `value`, `checked` and `selectedIndex` as live DOM state
   * where `React` names them as props, and that difference is the
   * backend's, not a disagreement about the tree. */
  def agree(name: String, ui: Ui): Unit = {
    // THE TREE AS THE WIRE CARRIES IT: `Wire.serve` lowers to the
    // vocabulary the client's hello claims, so both renderers are
    // given the very same value and any difference is theirs
    import Protocol.given
    val lowered = Ui.lower(ui, React.Vocabulary)
    val json = okay.codec.Json.write(lowered)
    val mine = elemOf(built.asInstanceOf[js.Function1[js.Any, js.Dynamic]](js.JSON.parse(json)))
    assertEquals(strip(mine), strip(React.elem(lowered)),
      s"live.js and React.elem disagree on $name")
  }

  /** the props that are DOM state rather than description */
  private def strip(e: Elem): Elem =
    // SORTED, because the ORDER of attributes on an element is not
    // meaning — two renderers that set the same attributes in a
    // different order have not disagreed about anything
    val keep = e.props.filterNot((k, _) => k == "value" || k == "checked").sortBy(_._1)
    // an element with no children and no text is the same element
    // whether a renderer calls that `Some("")` or `None` — the empty
    // header of the last column is not a disagreement
    e.copy(props = keep, text = e.text.filter(_.nonEmpty), children = e.children.map(strip))

  test("a text carries every style token, or it carries none") {
    agree("a plain text", Text("hi"))
    agree("a bold text", Text("hi", Style(bold = true)))
    agree("an identifier", Text("0xabc", Style(kind = Kind.Ident)))
    agree("a danger note", Text("no", Style(tone = Tone.Danger)))
    agree("every token at once",
      Text("x", Style(bold = true, dim = true, tone = Tone.Danger,
        size = Size.Large, kind = Kind.Number, align = Align.End)))
  }

  test("a row, a column and their keys") {
    agree("a row", Row(Vector(Text("a"), Text("b")), "who"))
    agree("a column", Column(Vector(Text("a")), "body"))
    agree("a keyless row", Row(Vector(Text("a"))))
  }

  test("A BOX'S WEIGHTS ARE ARITHMETIC, which is what a name table would miss") {
    agree("a weighted box",
      Box(Vector(Text("a"), Text("b"), Text("c")), Dir.Horizontal, Vector(3, 7, 5)))
    agree("a box with gap and pad",
      Box(Vector(Text("a")), Dir.Vertical, Vector.empty, gap = 2, pad = 1))
    // weights that do not match the children are ignored by BOTH, and
    // that agreement is the thing worth pinning
    agree("weights of the wrong length",
      Box(Vector(Text("a"), Text("b")), Dir.Horizontal, Vector(3)))
  }

  test("a button, a link and an image") {
    agree("a plain button", Button("go", "go"))
    agree("a primary button", Button("go", "go", Role.Primary))
    agree("a link", Link("open", "/c-1"))
    agree("an image", Image("/i.png", "a picture"))
  }

  test("an input of each kind, and a check, and a select") {
    agree("a text input", Input("x", "q"))
    agree("a secret", Input("", "k", kind = InputKind.Secret))
    agree("a number", Input("1", "n", kind = InputKind.Number))
    agree("a multiline", Input("x", "m", kind = InputKind.Multiline))
    agree("a labelled input", Input("x", "q", label = "subject"))
    agree("a live input", Input("x", "q", live = true))
    agree("a check", Check(true, "on"))
    agree("a labelled check", Check(false, "on", "unassigned"))
    agree("a select", Select(Vector("a", "b"), 1, "pick"))
  }

  test("a scroll and a form") {
    agree("a scroll", Scroll(Text("x"), "wide"))
    agree("a form", Form(Vector(Input("x", "q")), "save", "filters"))
  }

  test("A TABLE'S COLUMN PERCENTAGES ARE ARITHMETIC, and both must round alike") {
    val table = Table(Vector("case", "subject"),
      Vector(Vector(Text("c-1"), Text("0xabc"))), "cases", Vector(3, 7))
    agree("a table with weights", table)
    agree("a table with none",
      Table(Vector("a"), Vector(Vector(Text("x"))), "t"))
    // the shares this product actually ships, where 3/38 rounds to 7%
    agree("nine columns",
      Table(Vector("case", "subject", "money", "alerts", "weight", "opened", "held by", "status", ""),
        Vector(Vector.fill(9)(Text("x"))), "cases", Vector(3, 7, 5, 3, 3, 7, 2, 5, 3)))
  }

  test("the semantic nodes the browser claims, nested and alone") {
    val table = Table(Vector("h"), Vector(Vector(Text("x"))), "t")
    agree("items", Items(Vector(Link("a", "/a")), "i"))
    agree("tabs", Tabs(Vector("one", "two"), 1, Vector(table, Text("second")), "tb"))
    agree("a modal", Modal("title", table, "m"))
    agree("a disclosure, open", Disclosure("more", true, table, "d"))
    agree("a disclosure, shut", Disclosure("more", false, table, "d"))
    agree("a table inside items", Items(Vector(table), "i"))
  }

  test("A LIVE PRESS DOES NOT ALSO SUBMIT THE FORM UNDER IT") {
    // every button of the plain road is a <button> inside
    // <form method=post>, so it is a submit button. Without
    // preventDefault a live press did BOTH — sent the event up the
    // socket AND reloaded the page — and the socket road was
    // invisible because every press looked like the scriptless one.
    val js = LiveJs.source
    assert(js.contains("ev.preventDefault()"), "a live press still submits the form")
    // and it is in the CLICK handler, after the button is known to be
    // ours: preventing every click on the page would take the plain
    // road's links with it
    val click = js.substring(js.indexOf("addEventListener(\"click\""))
    val guard = click.indexOf("return;")
    val prevent = click.indexOf("ev.preventDefault()")
    assert(prevent > guard && guard > 0,
      "preventDefault must come after the guard that says the button is ours")
  }

  test("only the click handler prevents a default: typing and choosing do not") {
    val js = LiveJs.source
    assertEquals("ev.preventDefault()".r.findAllIn(js).size, 1)
  }

  test("a real page's worth of tree, all at once") {
    agree("a page", Column(Vector(
      Text("okay-watch", Style(bold = true, size = Size.Large)),
      Row(Vector(Text("ann", Style(dim = true)), Button("refresh", "refresh")), "who"),
      Scroll(Table(Vector("case", "subject"),
        Vector(Vector(Text("c-1", Style(kind = Kind.Ident)), Button("open…", "open:c-1"))),
        "cases", Vector(3, 7))),
      Form(Vector(Input("", "q", label = "subject"), Check(false, "u", "unassigned")),
        "filter", "filters")), "page"))
  }
}
