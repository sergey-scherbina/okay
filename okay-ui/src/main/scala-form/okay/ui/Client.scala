package okay.ui

import okay.js.{Js, Stmt}
import okay.js.Js.*

/**
 * THE BROWSER CLIENT, AS A SCALA VALUE (specs/js.md).
 *
 * It was 290 lines of hand-written JavaScript in a string literal, and
 * that was a deliberate trade: no build step and no artifact, because
 * the page IS the deployment. This keeps that trade — the jar still
 * carries one string — and pays a different price for a different
 * thing: the program is now a `Js` tree, so it is typed, composable,
 * printable, diffable, and every string in it is escaped by the
 * printer rather than by whoever typed it.
 *
 * WHAT MADE THE REWRITE SAFE was landing the check first.
 * `TestLiveJsDom` executes this client under Node against a fake
 * document and compares the DOM it builds with what `React.elem`
 * describes, node by node. Rewriting 290 lines of working JavaScript
 * with nothing but a string search for `case "Table":` would have
 * been reckless; with an executing comparison it is ordinary work.
 *
 * The shape is the old file's, deliberately: a reader who knew the
 * JavaScript can still find everything, and `git` can still tell what
 * changed.
 */
object Client:

  // ---- the shorthands, so the tree reads as the JavaScript it is ----

  private val doc = Name("document")
  private val win = Name("window")
  private val el = Name("el")
  private val f = Name("f")
  private val i = Name("i")
  private val u = Name("u")

  private def create(tag: Js): Js = Call(Field(doc, "createElement"), Vector(tag))
  private def create(tag: String): Js = create(Str(tag))
  private def set(target: Js, value: Js): Stmt = Stmt.Set(target, value)
  private def ret(value: Js): Stmt = Stmt.Return(Some(value))
  private def call(target: Js, args: Js*): Stmt = Stmt.Do(Call(target, args.toVector))
  private def data(of: Js, name: String): Js = Field(Field(of, "dataset"), name)
  private def style(of: Js, name: String): Js = Field(Field(of, "style"), name)
  private def len(of: Js): Js = Field(of, "length")
  private def append(parent: Js, child: Js): Stmt = call(Field(parent, "appendChild"), child)

  /** `for (i = 0; i < n; i++) body` with the counter already declared */
  private def times(counter: Js, n: Js, body: Stmt*): Stmt =
    Stmt.For(Some(Stmt.Set(counter, Num(0))), Some(Bin("<", counter, n)),
      Some(Unary("++", counter)), body.toVector)

  /** `for (var j = 0; j < n; j++) body` with its own counter */
  private def loop(name: String, n: Js, body: Stmt*): Stmt =
    Stmt.For(Some(Stmt.Var(name, Num(0))), Some(Bin("<", Name(name), n)),
      Some(Unary("++", Name(name))), body.toVector)

  private def ifSet(cond: Js, body: Stmt*): Stmt = Stmt.If(cond, body.toVector)

  // ---- the node builder ---------------------------------------------

  /** one `case "X": … return el;` of the builder's switch */
  private def node(name: String)(body: Stmt*): (Js, Vector[Stmt]) =
    (Str(name), body.toVector)

  private def styleClasses: Vector[Stmt] =
    val st = Name("st")
    Classes.flags.map { (flagName, _) =>
      ifSet(Field(st, flagName), call(Field(Name("cls"), "push"), Str(Classes.Prefix + flagName)))
    } ++ Classes.choices.map { c =>
      ifSet(Bin("&&", Field(st, c.field), Bin("!==", Field(st, c.field), Str(c.none))),
        call(Field(Name("cls"), "push"),
          Bin("+", Str(s"${Classes.Prefix}${c.field}-"), Field(st, c.field))))
    }

  private def buildBody: Vector[Stmt] = Vector(
    Stmt.Var("el", Undefined),
    Stmt.Var("i", Num(0)),
    Stmt.Var("t", Call(Name("k"), Vector(u))),
    Stmt.Var("f", Index(u, Name("t"))),
    Stmt.Switch(Name("t"), Vector(

      node("Link")(
        set(el, create("a")),
        set(Field(el, "href"), Field(f, "href")),
        set(Field(el, "textContent"), Field(f, "label")),
        ret(el)),

      node("Text")(
        set(el, create("span")),
        Stmt.Var("cls", Arr(Vector.empty)),
        Stmt.Var("st", Bin("||", Field(f, "style"), Obj(Vector.empty))),
        Stmt.Comment("the six style tokens come from okay.ui.Classes, which\n" +
          "React.elem reads too — one table, so a page cannot look\n" +
          "right one way and wrong the other"),
        Stmt.Block(styleClasses),
        ifSet(len(Name("cls")),
          set(Field(el, "className"), Call(Field(Name("cls"), "join"), Vector(Str(" "))))),
        set(Field(el, "textContent"), Field(f, "s")),
        ret(el)),

      // a fall-through: Row and Column differ by one class name
      (Str("Row"), Vector.empty),
      node("Column")(
        set(el, create("div")),
        set(Field(el, "className"),
          Ternary(Bin("===", Name("t"), Str("Row")), Str("okay-row"), Str("okay-col"))),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        times(i, len(Field(f, "children")),
          append(el, Call(Name("build"), Vector(Index(Field(f, "children"), i))))),
        ret(el)),

      node("Box")(
        set(el, create("div")),
        set(Field(el, "className"),
          Ternary(Bin("===", Field(f, "dir"), Str("h")),
            Str("okay-box okay-h"), Str("okay-box okay-v"))),
        ifSet(Field(f, "gap"), set(style(el, "gap"), Bin("+", Field(f, "gap"), Str("ch")))),
        ifSet(Field(f, "pad"), set(style(el, "padding"), Bin("+", Field(f, "pad"), Str("ch")))),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        Stmt.Comment("the weights ride on the box as data-w, so a patch that\n" +
          "replaces one child can give it its flex without holding\n" +
          "the tree (TestDom found a Replace losing it)"),
        ifSet(Bin("&&", Field(f, "weights"),
          Bin("===", len(Field(f, "weights")), len(Field(f, "children")))),
          set(data(el, "w"), Call(Field(Field(f, "weights"), "join"), Vector(Str(" "))))),
        times(i, len(Field(f, "children")),
          append(el, Call(Name("weighed"),
            Vector(el, i, Call(Name("build"), Vector(Index(Field(f, "children"), i))))))),
        ret(el)),

      node("Form")(
        Stmt.Comment("the hybrid rule: the DOM holds the fields' values; the\n" +
          "button keyed like the form sends them once as Submitted"),
        set(el, create("div")),
        set(Field(el, "className"), Str("okay-form")),
        set(data(el, "form"), Field(f, "key")),
        times(i, len(Field(f, "fields")),
          append(el, Call(Name("build"), Vector(Index(Field(f, "fields"), i))))),
        append(el, Call(Name("build"), Vector(Obj(Vector("Button" -> Obj(Vector(
          "label" -> Field(f, "submit"), "key" -> Field(f, "key"),
          "role" -> Str("primary")))))))),
        ret(el)),

      node("Scroll")(
        set(el, create("div")),
        set(Field(el, "className"), Str("okay-scroll")),
        set(style(el, "overflow"), Str("auto")),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        append(el, Call(Name("build"), Vector(Field(f, "child")))),
        ret(el)),

      node("Image")(
        set(el, create("img")),
        set(Field(el, "src"), Field(f, "src")),
        set(Field(el, "alt"), Field(f, "alt")),
        ret(el)),

      node("Button")(
        set(el, create("button")),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        ifSet(Bin("&&", Field(f, "role"), Bin("!==", Field(f, "role"), Str("plain"))),
          set(Field(el, "className"), Bin("+", Str("okay-"), Field(f, "role")))),
        set(Field(el, "textContent"), Field(f, "label")),
        ret(el)),

      node("Input")(
        Stmt.If(Bin("===", Field(f, "kind"), Str("multiline")),
          Vector(set(el, create("textarea"))),
          Vector(
            set(el, create("input")),
            Stmt.If(Bin("===", Field(f, "kind"), Str("secret")),
              Vector(set(Field(el, "type"), Str("password"))),
              Vector(Stmt.If(Bin("===", Field(f, "kind"), Str("number")),
                Vector(set(Field(el, "type"), Str("number")))))))),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        ifSet(Field(f, "live"), set(data(el, "live"), Str("1"))),
        set(Field(el, "value"), Field(f, "value")),
        ifSet(Unary("!", Field(f, "label")), ret(el)),
        Stmt.Var("lab", create("label")),
        Stmt.Var("sp", create("span")),
        set(Field(Name("sp"), "textContent"), Field(f, "label")),
        append(Name("lab"), Name("sp")),
        append(Name("lab"), el),
        ret(Name("lab"))),

      node("Check")(
        set(el, create("input")),
        set(Field(el, "type"), Str("checkbox")),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        set(Field(el, "checked"), Unary("!", Unary("!", Field(f, "on")))),
        ifSet(Unary("!", Field(f, "label")), ret(el)),
        Stmt.Var("lab2", create("label")),
        Stmt.Var("sp2", create("span")),
        set(Field(Name("sp2"), "textContent"), Field(f, "label")),
        append(Name("lab2"), el),
        append(Name("lab2"), Name("sp2")),
        ret(Name("lab2"))),

      node("Table")(
        Stmt.Comment("the browser draws a REAL table (ui-browser-vocab): the\n" +
          "header is readable as a header, and a column's width is\n" +
          "said once in <col> instead of inline on every cell"),
        set(el, create("table")),
        set(Field(el, "className"), Str("okay-table")),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        Stmt.Var("hdr", Bin("||", Field(f, "header"), Arr(Vector.empty))),
        Stmt.Var("wts", Bin("||", Field(f, "weights"), Arr(Vector.empty))),
        Stmt.Var("tot", Num(0)),
        times(i, len(Name("wts")),
          set(Name("tot"), Bin("+", Name("tot"), Index(Name("wts"), i)))),
        ifSet(Bin("&&", Bin("&&", len(Name("hdr")),
          Bin("===", len(Name("wts")), len(Name("hdr")))), Bin(">", Name("tot"), Num(0))),
          Stmt.Var("cg", create("colgroup")),
          times(i, len(Name("wts")),
            Stmt.Var("cl", create("col")),
            set(style(Name("cl"), "width"),
              Bin("+", Call(Field(Name("Math"), "floor"), Vector(
                Bin("/", Bin("*", Index(Name("wts"), i), Num(100)), Name("tot")))), Str("%"))),
            append(Name("cg"), Name("cl"))),
          append(el, Name("cg"))),
        ifSet(len(Name("hdr")),
          Stmt.Var("thd", create("thead")),
          Stmt.Var("hr", create("tr")),
          times(i, len(Name("hdr")),
            Stmt.Var("hc", create("th")),
            call(Field(Name("hc"), "setAttribute"), Str("scope"), Str("col")),
            set(Field(Name("hc"), "textContent"), Index(Name("hdr"), i)),
            append(Name("hr"), Name("hc"))),
          append(Name("thd"), Name("hr")),
          append(el, Name("thd"))),
        Stmt.Var("tb", create("tbody")),
        times(i, len(Field(f, "rows")),
          Stmt.Var("rw", create("tr")),
          loop("j", len(Index(Field(f, "rows"), i)),
            Stmt.Var("cel", create("td")),
            append(Name("cel"), Call(Name("build"),
              Vector(Index(Index(Field(f, "rows"), i), Name("j"))))),
            append(Name("rw"), Name("cel"))),
          append(Name("tb"), Name("rw"))),
        append(el, Name("tb")),
        ret(el)),

      node("Select")(
        set(el, create("select")),
        ifSet(Field(f, "key"), set(data(el, "key"), Field(f, "key"))),
        times(i, len(Field(f, "options")),
          Stmt.Var("o", create("option")),
          set(Field(Name("o"), "value"), Index(Field(f, "options"), i)),
          set(Field(Name("o"), "textContent"), Index(Field(f, "options"), i)),
          append(el, Name("o"))),
        set(Field(el, "selectedIndex"), Field(f, "selected")),
        ret(el)))),

    ret(Call(Field(doc, "createTextNode"), Vector(Str("")))))

  // ---- the helpers the builder and the patcher share -----------------

  private def kBody: Vector[Stmt] = Vector(
    Stmt.Comment("a node is {\"Case\": {fields}}: this is its case name.\n" +
      "`Object.keys(u)[0]` rather than a for-in, because the tree\n" +
      "says what it means and a for-in with a return inside is the\n" +
      "one shape a reader has to stop at"),
    ret(Index(Call(Field(Name("Object"), "keys"), Vector(u)), Num(0))))

  private def weighedBody: Vector[Stmt] =
    val par = Name("par")
    val ch = Name("ch")
    Vector(
      ifSet(Bin("&&", Field(par, "dataset"), data(par, "w")),
        Stmt.Var("w", Call(Field(data(par, "w"), "split"), Vector(Str(" ")))),
        ifSet(Index(Name("w"), i), set(style(ch, "flex"), Index(Name("w"), i)))),
      ret(ch))

  private def editableBody: Vector[Stmt] =
    val n = Name("n")
    val isField = (x: Js) => Bin("||", Bin("||",
      Bin("===", x, Str("input")), Bin("===", x, Str("select"))),
      Bin("===", x, Str("textarea")))
    Vector(
      Stmt.Var("tag", Call(Field(Field(n, "tagName"), "toLowerCase"), Vector.empty)),
      ifSet(isField(Name("tag")), ret(n)),
      loop("i", len(Field(n, "childNodes")),
        Stmt.Var("t", Bin("&&", Field(Index(Field(n, "childNodes"), i), "tagName"),
          Call(Field(Field(Index(Field(n, "childNodes"), i), "tagName"), "toLowerCase"),
            Vector.empty))),
        ifSet(Bin("||", Bin("===", Name("t"), Str("input")),
          Bin("===", Name("t"), Str("textarea"))),
          ret(Index(Field(n, "childNodes"), i)))),
      ret(n))

  // ---- the live session ----------------------------------------------

  private val root = Name("root")
  private val ws = Name("ws")

  private def atBody: Vector[Stmt] =
    val path = Name("path")
    Vector(
      Stmt.Var("n", Index(Field(root, "childNodes"), Num(0))),
      loop("i", len(path),
        set(Name("n"), Index(Field(Name("n"), "childNodes"), Index(path, i)))),
      ret(Name("n")))

  private def applyBody: Vector[Stmt] =
    val p = Name("p")
    val n = Name("n")
    val par = Name("par")
    val last = Index(Field(f, "path"), Bin("-", len(Field(f, "path")), Num(1)))
    Vector(
      Stmt.Var("n", Undefined),
      Stmt.Var("par", Undefined),
      Stmt.Var("snap", Undefined),
      Stmt.Var("t", Call(Name("k"), Vector(p))),
      Stmt.Var("f", Index(p, Name("t"))),
      Stmt.Switch(Name("t"), Vector(
        (Str("Replace"), Vector(
          Stmt.If(Bin("===", len(Field(f, "path")), Num(0)),
            Vector(
              Stmt.Var("b", Call(Name("build"), Vector(Field(f, "ui")))),
              Stmt.If(len(Field(root, "childNodes")),
                Vector(call(Field(root, "replaceChild"), Name("b"),
                  Index(Field(root, "childNodes"), Num(0)))),
                Vector(append(root, Name("b"))))),
            Vector(
              set(par, Call(Name("at"), Vector(
                Call(Field(Field(f, "path"), "slice"), Vector(Num(0), Num(-1)))))),
              call(Field(par, "replaceChild"),
                Call(Name("weighed"), Vector(par, last,
                  Call(Name("build"), Vector(Field(f, "ui"))))),
                Index(Field(par, "childNodes"), last)))),
          Stmt.Break)),
        (Str("SetText"), Vector(
          set(Field(Call(Name("at"), Vector(Field(f, "path"))), "textContent"), Field(f, "s")),
          Stmt.Break)),
        (Str("SetValue"), Vector(
          set(Field(Call(Name("editable"),
            Vector(Call(Name("at"), Vector(Field(f, "path"))))), "value"), Field(f, "s")),
          Stmt.Break)),
        (Str("SetChecked"), Vector(
          set(Field(Call(Name("editable"),
            Vector(Call(Name("at"), Vector(Field(f, "path"))))), "checked"), Field(f, "on")),
          Stmt.Break)),
        (Str("SetSelected"), Vector(
          set(Field(Call(Name("at"), Vector(Field(f, "path"))), "selectedIndex"),
            Field(f, "index")),
          Stmt.Break)),
        (Str("Remove"), Vector(
          set(n, Call(Name("at"), Vector(Field(f, "path")))),
          call(Field(n, "removeChild"), Index(Field(n, "childNodes"), Field(f, "index"))),
          Stmt.Break)),
        (Str("Reorder"), Vector(
          set(n, Call(Name("at"), Vector(Field(f, "path")))),
          set(Name("snap"), Call(Field(Field(Field(Name("Array"), "prototype"), "slice"), "call"),
            Vector(Field(n, "childNodes")))),
          loop("i", len(Field(f, "order")),
            append(n, Index(Name("snap"), Index(Field(f, "order"), i)))),
          Stmt.Break)),
        (Str("Insert"), Vector(
          set(n, Call(Name("at"), Vector(Field(f, "path")))),
          call(Field(n, "insertBefore"),
            Call(Name("weighed"), Vector(n, Field(f, "index"),
              Call(Name("build"), Vector(Field(f, "ui"))))),
            Ternary(Bin("<", Field(f, "index"), len(Field(n, "childNodes"))),
              Index(Field(n, "childNodes"), Field(f, "index")), Null)),
          Stmt.Break)))))

  private def liveBody: Vector[Stmt] =
    val id = Name("id")
    val e = Name("e")
    val loc = Name("location")
    Vector(
      Stmt.Var("root", Call(Field(doc, "getElementById"),
        Vector(Bin("+", Str("okay-live-"), id)))),
      ifSet(Unary("!", root), Stmt.Return(None)),
      Stmt.Var("at", Fun(Vector("path"), atBody)),
      Stmt.Var("apply", Fun(Vector("p"), applyBody)),
      Stmt.Var("proto", Ternary(Bin("===", Field(loc, "protocol"), Str("https:")),
        Str("wss://"), Str("ws://"))),
      Stmt.Var("sep", Ternary(Field(loc, "search"), Str("&"), Str("?"))),
      Stmt.Var("ws", New(Name("WebSocket"), Vector(
        Bin("+", Bin("+", Bin("+", Bin("+", Bin("+", Bin("+",
          Name("proto"), Field(loc, "host")), Field(loc, "pathname")),
          Field(loc, "search")), Name("sep")), Str("__live=")),
          Call(Name("encodeURIComponent"), Vector(id)))))),
      Stmt.Comment("an event before the socket opens is KEPT, not dropped\n" +
        "(ui-mobile found a tap racing the connection): the hello\n" +
        "goes first, then the queue"),
      Stmt.Var("queue", Arr(Vector.empty)),
      Stmt.Var("send", Fun(Vector("o"), Vector(
        Stmt.If(Bin("===", Field(ws, "readyState"), Num(1)),
          Vector(call(Field(ws, "send"),
            Call(Field(Name("JSON"), "stringify"), Vector(Name("o"))))),
          Vector(call(Field(Name("queue"), "push"), Name("o"))))))),
      Stmt.Var("event", Fun(Vector("e"), Vector(
        call(Name("send"), Obj(Vector("Event" -> Obj(Vector("event" -> e)))))))),
      set(Field(ws, "onopen"), Fun(Vector.empty, Vector(
        Stmt.Comment("what a browser really has of its OWN: the set is\n" +
          "generated from React.Vocabulary, so the socket road and\n" +
          "the scriptless road cannot claim different things"),
        call(Field(ws, "send"), Call(Field(Name("JSON"), "stringify"), Vector(
          Obj(Vector("Hello" -> Obj(Vector(
            "vocab" -> Arr(React.Vocabulary.toVector.sorted.map(Str(_))),
            "version" -> Num(Protocol.version)))))))),
        Stmt.Var("q", Name("queue")),
        set(Name("queue"), Arr(Vector.empty)),
        loop("i", len(Name("q")),
          call(Field(ws, "send"),
            Call(Field(Name("JSON"), "stringify"), Vector(Index(Name("q"), i)))))))),
      set(Field(ws, "onmessage"), Fun(Vector("m"), Vector(
        Stmt.Var("j", Call(Field(Name("JSON"), "parse"), Vector(Field(Name("m"), "data")))),
        Stmt.Var("t", Call(Name("k"), Vector(Name("j")))),
        Stmt.If(Bin("===", Name("t"), Str("Tree")),
          Vector(
            Stmt.While(Field(root, "firstChild"),
              Vector(call(Field(root, "removeChild"), Field(root, "firstChild")))),
            append(root, Call(Name("build"), Vector(Field(Field(Name("j"), "Tree"), "ui"))))),
          Vector(Stmt.If(Bin("===", Name("t"), Str("Patch")),
            Vector(call(Name("apply"), Field(Field(Name("j"), "Patch"), "patch"))),
            Vector(Stmt.If(Bin("===", Name("t"), Str("Close")),
              Vector(call(Field(ws, "close")))))))))))) ++
      Vector(set(Field(ws, "onclose"), Fun(Vector.empty, Vector(
        call(Name("setTimeout"), Fun(Vector.empty, Vector(
          call(Field(Name("location"), "reload")))), Num(1000)))))) ++
      listeners

  /** TYPING: inside a form the DOM keeps the value and only an input
   * marked live speaks, because a keystroke per character up a socket
   * is a cost nobody asked for. */
  private def onInput: Vector[Stmt] =
    val key = data(el, "key")
    val notAField = Unary("!", Bin("||",
      Bin("===", Field(el, "tagName"), Str("TEXTAREA")),
      Bin("&&", Bin("===", Field(el, "tagName"), Str("INPUT")),
        Bin("!==", Field(el, "type"), Str("checkbox")))))
    Vector(
      Stmt.Var("el", Call(Name("keyed"), Vector(Field(Name("ev"), "target")))),
      ifSet(Bin("||", Unary("!", el), notAField), Stmt.Return(None)),
      ifSet(Bin("&&", Call(Name("formOf"), Vector(el)), Unary("!", data(el, "live"))),
        Stmt.Return(None)),
      call(Name("event"), Obj(Vector("Edited" -> Obj(Vector(
        "key" -> key, "value" -> Field(el, "value")))))))

  /** A CHECK OR A SELECT outside a form: those two settle on `change`
   * rather than on every keystroke. */
  private def onChange: Vector[Stmt] =
    val key = data(el, "key")
    Vector(
      Stmt.Var("el", Call(Name("keyed"), Vector(Field(Name("ev"), "target")))),
      ifSet(Bin("||", Unary("!", el), Call(Name("formOf"), Vector(el))), Stmt.Return(None)),
      Stmt.If(Bin("&&", Bin("===", Field(el, "tagName"), Str("INPUT")),
        Bin("===", Field(el, "type"), Str("checkbox"))),
        Vector(call(Name("event"), Obj(Vector("Toggled" -> Obj(Vector(
          "key" -> key, "on" -> Field(el, "checked"))))))),
        Vector(Stmt.If(Bin("===", Field(el, "tagName"), Str("SELECT")),
          Vector(call(Name("event"), Obj(Vector("Chosen" -> Obj(Vector(
            "key" -> key, "index" -> Field(el, "selectedIndex"))))))))))) 

  /** A PRESS: the button's own key, or the form's if the button is
   * the one keyed like it — that is the whole hybrid rule, and the
   * edits ride with it so a form is submitted once. */
  private def onClick: Vector[Stmt] =
    val form = Name("form")
    val key = data(el, "key")
    Vector(
      Stmt.Var("el", Call(Name("keyed"), Vector(Field(Name("ev"), "target")))),
      ifSet(Bin("||", Unary("!", el), Bin("!==", Field(el, "tagName"), Str("BUTTON"))),
        Stmt.Return(None)),
      Stmt.Comment("AND THE FORM UNDER IT DOES NOT SUBMIT. Every button of\n" +
        "the plain road is a <button> inside <form method=post>, so\n" +
        "it is a submit button — and without this a live press did\n" +
        "BOTH: it sent the event up the socket and reloaded the whole\n" +
        "page. The socket road was invisible because every press\n" +
        "looked exactly like the scriptless one.\n" +
        "\n" +
        "Here is the right place for it: this listener exists only\n" +
        "when the script ran, so a browser with no script still\n" +
        "submits the form and nothing is taken away."),
      call(Field(Name("ev"), "preventDefault")),
      Stmt.Var("form", Call(Name("formOf"), Vector(el))),
      Stmt.If(Bin("&&", form, Bin("===", data(form, "form"), key)),
        Vector(call(Name("event"), Obj(Vector("Submitted" -> Obj(Vector(
          "key" -> key, "edits" -> Call(Name("edits"), Vector(form)))))))),
        Vector(call(Name("event"), Obj(Vector("Pressed" -> Obj(Vector("key" -> key))))))))

  /** WHAT A FORM HOLDS, read off the DOM at the moment of submit.
   *
   * The hybrid rule: inside a form the browser keeps the values and
   * nothing is sent until the form's own button is pressed, so this
   * is where they are gathered. */
  private def editsBody: Vector[Stmt] =
    val e = Name("e")
    val out = Name("out")
    val tag = Name("tag")
    def push(kind: String, fields: (String, Js)*): Stmt =
      call(Field(out, "push"), Obj(Vector(kind -> Obj(fields.toVector))))
    val chosen = Stmt.If(Bin("===", tag, Str("SELECT")),
      Vector(push("Chosen", "key" -> data(e, "key"), "index" -> Field(e, "selectedIndex"))))
    val edited = Stmt.If(
      Bin("||", Bin("===", tag, Str("INPUT")), Bin("===", tag, Str("TEXTAREA"))),
      Vector(push("Edited", "key" -> data(e, "key"), "value" -> Field(e, "value"))),
      Vector(chosen))
    val toggled = Stmt.If(
      Bin("&&", Bin("===", tag, Str("INPUT")), Bin("===", Field(e, "type"), Str("checkbox"))),
      Vector(push("Toggled", "key" -> data(e, "key"), "on" -> Field(e, "checked"))),
      Vector(edited))
    Vector(
      Stmt.Var("out", Arr(Vector.empty)),
      Stmt.Var("els", Call(Field(Name("form"), "querySelectorAll"), Vector(Str("[data-key]")))),
      loop("i", len(Name("els")),
        Stmt.Var("e", Index(Name("els"), i)),
        Stmt.Var("tag", Field(e, "tagName")),
        toggled),
      ret(out))

  /** the three delegated listeners and the farewell */
  private def listeners: Vector[Stmt] =
    def walkUp(mark: String): Vector[Stmt] = Vector(
      Stmt.While(Bin("&&", el, Bin("!==", el, root)), Vector(
        ifSet(Bin("&&", Field(el, "dataset"), data(el, mark)), ret(el)),
        set(el, Field(el, "parentNode")))),
      ret(Null))
    Vector(
      Stmt.Var("keyed", Fun(Vector("el"), walkUp("key"))),
      Stmt.Var("formOf", Fun(Vector("el"), walkUp("form"))),
      Stmt.Var("edits", Fun(Vector("form"), editsBody)),

      call(Field(root, "addEventListener"), Str("click"), Fun(Vector("ev"), onClick)),

      Stmt.Comment("inside a form the DOM keeps the value; only a live input speaks"),
      call(Field(root, "addEventListener"), Str("input"), Fun(Vector("ev"), onInput)),
      call(Field(root, "addEventListener"), Str("change"), Fun(Vector("ev"), onChange)),

      call(Field(win, "addEventListener"), Str("beforeunload"),
        Fun(Vector.empty, Vector(
          call(Name("event"), Obj(Vector("Closed" -> Obj(Vector.empty))))))))

  // ---- the whole program ----------------------------------------------

  /** every statement of the client, in the order the file had them */
  def program: Vector[Stmt] = Vector(
    Stmt.Comment("okay-ui's browser client: a patch consumer speaking\n" +
      "okay.ui.Protocol (docs/protocol/frontend.md).\n" +
      "GENERATED from okay.ui.Client — do not edit this file."),
    Stmt.Var("k", Fun(Vector("u"), kBody)),
    Stmt.Var("build", Fun(Vector("u"), buildBody)),
    Stmt.Var("weighed", Fun(Vector("par", "i", "ch"), weighedBody)),
    Stmt.Var("editable", Fun(Vector("n"), editableBody)),
    Stmt.Comment("THE NODE BUILDER IS REACHABLE, so a test can run it.\n" +
      "Nothing in a browser calls this; it exists because until it\n" +
      "did, nothing executed this file at all (ui-livejs-verified)."),
    set(Field(win, "okayBuild"), Name("build")),
    set(Field(win, "okayLive"), Fun(Vector("id"), liveBody)))

  /** the program, wrapped so it declares nothing globally */
  def source: String =
    Js.print(Vector(Stmt.Do(Call(
      Fun(Vector.empty, program), Vector.empty))))
