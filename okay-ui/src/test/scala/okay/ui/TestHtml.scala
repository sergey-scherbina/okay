package okay.ui

/** specs/ui-html.md: the HTML host -- a tree rendered as HTML, and a
 * browser's POST read back as events. Moved here from okay-script's
 * TestLive/TestLivePlain, which asserted the same rules through
 * `Live` and so needed the container for three pure functions.
 */
class TestHtml extends munit.FunSuite:

  test("render: every shape to the HTML React.elem implies, escaped") {
    val tree = Ui.Column(Vector(
      Ui.Text("a <b>", Style(bold = true)),
      Ui.Row(Vector(Ui.Button("go", "k1")), "r"),
      Ui.Input("v\"", "k2", "name"),
      Ui.Check(true, "k3", "yes"),
      Ui.Select(Vector("x", "y"), 1, "k4"),
    ))
    assertEquals(Html.render(tree),
      """<div class="okay-col">""" +
        """<span class="okay-bold">a &lt;b&gt;</span>""" +
        """<div data-key="r" class="okay-row"><button data-key="k1">go</button></div>""" +
        """<label><span>name</span><input data-key="k2" value="v&quot;"></label>""" +
        """<label><input data-key="k3" type="checkbox" checked><span>yes</span></label>""" +
        """<select data-key="k4" value="y"><option value="x">x</option><option value="y">y</option></select>""" +
        "</div>")
  }

  test("form: one form, the hidden mount field, every field named -- the textarea too -- and no script") {
    val h = Html.form("c", Ui.Column(Vector(Ui.Input("", "note", "", InputKind.Multiline), Ui.Button("+1", "inc"))), "/counter")
    assert(h.startsWith("""<form method="post" action="/counter" id="okay-live-c" class="okay-plain"><input type="hidden" name="__okay_plain" value="c">"""), h)
    assert(h.contains("""<textarea data-key="note" value="" name="note"></textarea>"""), h)
    assert(h.contains("""<button data-key="inc" name="__press" value="inc">+1</button>"""), h)
    assert(h.endsWith("</form>") && !h.contains("<script"), h)
  }

  private val shown = Ui.Column(Vector(
    Ui.Input("Ann", "name"), Ui.Input("", "note", "", InputKind.Multiline),
    Ui.Check(true, "gift"), Ui.Check(false, "rush"),
    Ui.Select(Vector("x", "y", "z"), 1, "pick"),
    Ui.Button("go", "go")))

  test("events: unchanged fields are nothing, changed ones are one event each, an unposted checkbox is false only when it was on") {
    assertEquals(Html.events(shown, Map("name" -> "Ann", "note" -> "", "gift" -> "on", "pick" -> "y", "__press" -> "go")),
      Vector(Event.Pressed("go")))
    assertEquals(Html.events(shown, Map("name" -> "Bob", "note" -> "hi", "rush" -> "on", "pick" -> "z", "__press" -> "go")),
      Vector(Event.Edited("name", "Bob"), Event.Edited("note", "hi"), Event.Toggled("gift", false), Event.Toggled("rush", true),
        Event.Chosen("pick", 2), Event.Pressed("go")))
    // a Select posted with a value that is no option is nothing
    assertEquals(Html.events(shown, Map("name" -> "Ann", "gift" -> "on", "pick" -> "q")), Vector.empty)
    // a GET carries no fields: the identity
    assertEquals(Html.events(shown, Map.empty), Vector(Event.Toggled("gift", false)))
  }

  test("events: the capability rule -- a press naming a key the tree did not show is dropped") {
    assertEquals(Html.events(shown, Map("name" -> "Ann", "gift" -> "on", "__press" -> "ghost")), Vector.empty)
    // ...and a field nobody showed is not an event either
    assertEquals(Html.events(shown, Map("name" -> "Ann", "gift" -> "on", "ghost" -> "x")), Vector.empty)
  }

  test("events: a Form's own button folds that form's edits into ONE Submitted and leaves the others plain") {
    val tree = Ui.Column(Vector(
      Ui.Input("", "outside"),
      Ui.Form(Vector(Ui.Input("", "name"), Ui.Input("", "qty")), "Submit", "f")))
    assertEquals(Html.events(tree, Map("outside" -> "o", "name" -> "Ann", "qty" -> "2", "__press" -> "f")),
      Vector(Event.Edited("outside", "o"),
        Event.Submitted("f", Vector(Event.Edited("name", "Ann"), Event.Edited("qty", "2")))))
  }

  test("events: a button inside a form that is NOT the form's own is a Pressed after the fields' plain edits") {
    val tree = Ui.Form(Vector(Ui.Input("", "items[0]"), Ui.Button("+", "items$add")), "Submit", "f")
    assertEquals(Html.events(tree, Map("items[0]" -> "a", "__press" -> "items$add")),
      Vector(Event.Edited("items[0]", "a"), Event.Pressed("items$add")))
  }

  test("step: the events folded -- a counter steps once per shown press, and not at all for an unshown one") {
    val view: Int => Ui = n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc")))
    val update: (Int, Event) => Int = (n, e) => e match
      case Event.Pressed("inc") => n + 1
      case _ => n
    assertEquals(Html.step(view, update)(0, Map("__press" -> "inc")), 1)
    assertEquals(Html.step(view, update)(3, Map("__press" -> "dec")), 3)
    assertEquals(Html.step(view, update)(3, Map.empty), 3)
  }
