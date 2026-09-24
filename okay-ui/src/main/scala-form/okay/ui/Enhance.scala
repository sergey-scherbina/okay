package okay.ui

import okay.js.{Js, Stmt}
import okay.js.Js.*

/**
 * WHAT AN APP DOES THAT A PAGE DOES NOT, for the HTML host
 * (specs/ui-app.md), as a `Js` program like the live client — typed,
 * printed, no escape hatch:
 *
 * - every `select` opens OUR list, under its field, at its width, in
 *   the page's font — the `select` itself is left in place, because a
 *   live page patches its tree by path;
 * - a form in `main.okay-main` (not `data-hard`, not the live road's
 *   own) is sent by `fetch`, with a spinner beside its button, and only
 *   the frame's content is replaced — the scroll stays;
 * - `<meta name="okay-refresh" content="N">` keeps a page fresh by
 *   fetching it, not by a reload that repaints and jumps;
 * - on the live road a pressed button spins until the mount's next patch.
 *
 * A page without the script is the same page: every one of these is
 * a road the plain HTML already has.
 */
object Enhance:

  // ---- shorthands ---------------------------------------------------

  private val doc = Name("document")
  private val win = Name("window")
  private def v(n: String): Js = Name(n)
  private def does(j: Js): Stmt = Stmt.Do(j)
  private def let(n: String, j: Js): Stmt = Stmt.Var(n, j)
  private def set(t: Js, j: Js): Stmt = Stmt.Set(t, j)
  private def when(c: Js)(body: Stmt*): Stmt = Stmt.If(c, body.toVector)
  private val done: Stmt = Stmt.Return(None)
  private def listen(on: Js, event: String, capture: Boolean)(params: String*)(body: Stmt*): Stmt =
    does(on.dot("addEventListener").of(Str(event), Fun(params.toVector, body.toVector), Bool(capture)))
  private def q(on: Js, selector: String): Js = on.dot("querySelector").of(Str(selector))
  private def px(j: Js): Js = j + Str("px")
  private def event(name: String): Js = New(v("Event"), Vector(Str(name), obj("bubbles" -> Bool(true))))

  /** the main, the frame's content */
  val Main = "main.okay-main"

  // ---- the dropdown ---------------------------------------------------

  private def picker: Vector[Stmt] =
    val sel = v("sel"); val p = v("p"); val r = v("r"); val cs = v("cs"); val e = v("e")
    Vector(
      let("open", Null),
      Stmt.Comment("the list, gone"),
      let("close", fun()(when(v("open"))(does(v("open").dot("remove").of()), set(v("open"), Null)))),
      Stmt.Comment("one choice: the value set and `input` + `change` fired, as the engine's own list would"),
      let("choice", fun("sel", "i")(
        let("d", doc.dot("createElement").of(Str("div"))),
        set(v("d").dot("textContent"), sel.dot("options").at(v("i")).dot("text")),
        when(v("i") === sel.dot("selectedIndex"))(set(v("d").dot("className"), Str("okay-on"))),
        listen(v("d"), "mousedown", false)("ev")(
          does(v("ev").dot("preventDefault").of()), does(v("ev").dot("stopPropagation").of()),
          when(sel.dot("selectedIndex") !== v("i"))(
            set(sel.dot("selectedIndex"), v("i")),
            does(sel.dot("dispatchEvent").of(event("input"))),
            does(sel.dot("dispatchEvent").of(event("change")))),
          does(v("close").of())),
        Stmt.Return(Some(v("d"))))),
      listen(doc, "mousedown", true)("e")(
        let("sel", Ternary(e.dot("target").dot("closest"), e.dot("target").dot("closest").of(Str("select")), Null)),
        when(v("open"))(
          when(v("open").dot("contains").of(e.dot("target")))(done),
          does(v("close").of())),
        when(sel.not || sel.dot("disabled") || sel.dot("multiple"))(done),
        does(e.dot("preventDefault").of()),
        does(sel.dot("focus").of()),
        let("r", sel.dot("getBoundingClientRect").of()),
        let("cs", v("getComputedStyle").of(sel)),
        let("p", doc.dot("createElement").of(Str("div"))),
        set(p.dot("className"), Str("okay-pick")),
        set(p.dot("style").dot("left"), px(r.dot("left") + win.dot("scrollX"))),
        set(p.dot("style").dot("top"), px(r.dot("bottom") + win.dot("scrollY") + Num(4))),
        set(p.dot("style").dot("minWidth"), px(r.dot("width"))),
        set(p.dot("style").dot("fontSize"), cs.dot("fontSize")),
        set(p.dot("style").dot("fontFamily"), cs.dot("fontFamily")),
        Stmt.For(Some(let("i", Num(0))), Some(Bin("<", v("i"), sel.dot("options").dot("length"))), Some(Unary("++", v("i"))),
          Vector(does(p.dot("appendChild").of(v("choice").of(sel, v("i")))))),
        does(doc.dot("body").dot("appendChild").of(p)),
        set(v("open"), p),
        Stmt.Comment("no room below: above the field"),
        let("pr", p.dot("getBoundingClientRect").of()),
        when(Bin(">", v("pr").dot("bottom"), win.dot("innerHeight")) && Bin(">", r.dot("top"), v("pr").dot("height")))(
          set(p.dot("style").dot("top"), px(Bin("-", Bin("-", r.dot("top") + win.dot("scrollY"), v("pr").dot("height")), Num(4)))))),
      listen(doc, "keydown", false)("e")(when(e.dot("key") === Str("Escape"))(does(v("close").of()))),
      listen(win, "scroll", true)("e")(when(v("open") && (e.dot("target") !== v("open")))(does(v("close").of()))),
      does(win.dot("addEventListener").of(Str("resize"), v("close"))))

  // ---- the press that does not reload -----------------------------------

  private def presses: Vector[Stmt] =
    val f = v("f"); val b = v("b"); val d = v("d"); val here = v("here"); val m = v("m"); val e = v("e")
    Vector(
      Stmt.Comment("a button at work: dimmed, a spinner beside it"),
      let("busy", fun("b")(
        when(b.not)(Stmt.Return(Some(Null))),
        does(b.dot("classList").dot("add").of(Str("okay-busy"))),
        let("s", doc.dot("createElement").of(Str("span"))),
        set(v("s").dot("className"), Str("okay-spin")),
        does(b.dot("insertAdjacentElement").of(Str("afterend"), v("s"))),
        Stmt.Return(Some(v("s"))))),
      let("timer", Null),
      Stmt.Comment("a page that keeps itself fresh: fetched again, not reloaded"),
      let("fresh", fun("d")(
        does(v("clearTimeout").of(v("timer"))),
        let("m", q(d, """meta[name="okay-refresh"]""")),
        when(m)(set(v("timer"), v("setTimeout").of(fun()(
          does(v("fetch").of(Name("location").dot("href"), obj("cache" -> Str("no-store")))
            .dot("then").of(fun("x")(Stmt.Return(Some(v("x").dot("text").of()))))
            .dot("then").of(fun("h")(does(v("swap").of(v("h"), Null, Bool(true)))))
            .dot("catch").of(fun()(does(v("fresh").of(d)))))),
          Bin("*", Bin("||", Unary("+", m.dot("content")), Num(5)), Num(1000))))))),
      Stmt.Comment("the answer's content in place of this one's; the scroll kept on the same page"),
      let("swap", fun("html", "url", "same")(
        let("d", New(v("DOMParser"), Vector.empty).dot("parseFromString").of(v("html"), Str("text/html"))),
        let("m", q(d, Main)),
        let("here", q(doc, Main)),
        when(m.not || here.not)(
          does(doc.dot("open").of()), does(doc.dot("write").of(v("html"))), does(doc.dot("close").of()), done),
        let("y", win.dot("scrollY")),
        set(here.dot("innerHTML"), m.dot("innerHTML")),
        set(doc.dot("title"), d.dot("title")),
        let("s1", q(d, "nav.okay-side")), let("s2", q(doc, "nav.okay-side")),
        when(v("s1") && v("s2"))(set(v("s2").dot("innerHTML"), v("s1").dot("innerHTML"))),
        when(v("url") && (v("url") !== Name("location").dot("href")))(
          does(Name("history").dot("pushState").of(Null, Str(""), v("url")))),
        does(win.dot("scrollTo").of(Num(0), Ternary(v("same"), v("y"), Num(0)))),
        does(v("fresh").of(d)))),
      listen(doc, "submit", false)("e")(
        when(e.dot("defaultPrevented"))(done),
        let("f", e.dot("target")),
        when(f.dot("closest").of(Str(Main)).not || f.dot("classList").dot("contains").of(Str(Html.PlainClass)) ||
          f.dot("hasAttribute").of(Str("data-hard")))(done),
        does(e.dot("preventDefault").of()),
        let("b", e.dot("submitter")),
        let("s", v("busy").of(b)),
        let("t0", Name("Date").dot("now").of()),
        let("body", New(v("URLSearchParams"), Vector(New(v("FormData"), Vector(f))))),
        when(b && b.dot("name"))(does(v("body").dot("set").of(b.dot("name"), b.dot("value")))),
        let("get", Bin("===", Bin("||", f.dot("getAttribute").of(Str("method")), Str("get")).dot("toLowerCase").of(), Str("get"))),
        let("to", f.dot("action")),
        let("req", Ternary(v("get"),
          v("fetch").of(v("to") + Ternary(Bin("<", v("to").dot("indexOf").of(Str("?")), Num(0)), Str("?"), Str("&")) +
            v("body").dot("toString").of()),
          v("fetch").of(v("to"), obj("method" -> Str("POST"),
            "headers" -> obj("content-type" -> Str("application/x-www-form-urlencoded")),
            "body" -> v("body").dot("toString").of())))),
        does(v("req").dot("then").of(texted).dot("then").of(answered).dot("catch").of(failed))),
      does(win.dot("addEventListener").of(Str("popstate"), fun()(does(Name("location").dot("reload").of())))),
      does(v("fresh").of(doc)))

  /** the answer's text and where it came from */
  private val texted: Js =
    fun("x")(Stmt.Return(Some(v("x").dot("text").of().dot("then").of(
      fun("h")(Stmt.Return(Some(arr(v("h"), v("x").dot("url")))))))))

  /** swapped in — after at least 450 ms, so the spinner is seen */
  private val answered: Js =
    val base = (u: Js) => u.dot("split").of(Str("?")).at(Num(0))
    val later = fun()(
      let("same", base(v("r").at(Num(1))) === base(Name("location").dot("href"))),
      does(v("swap").of(v("r").at(Num(0)), v("r").at(Num(1)), v("same"))))
    val wait = v("Math").dot("max").of(Num(0), Bin("-", Num(450), Bin("-", Name("Date").dot("now").of(), v("t0"))))
    fun("r")(does(v("setTimeout").of(later, wait)))

  /** the press failed: the button back as it was */
  private val failed: Js =
    fun()(
      when(v("s"))(does(v("s").dot("remove").of())),
      when(v("b"))(does(v("b").dot("classList").dot("remove").of(Str("okay-busy")))))

  // ---- the live road: a press spins until the patch -----------------------

  private def live: Vector[Stmt] =
    val e = v("e"); val b = v("b")
    Vector(
      let("spun", arr()),
      let("unspin", fun()(
        Stmt.While(v("spun").dot("length"), Vector(
          let("x", v("spun").dot("pop").of()),
          does(v("x").at(Num(0)).dot("classList").dot("remove").of(Str("okay-busy"))),
          does(v("x").at(Num(1)).dot("remove").of()))))),
      listen(doc, "click", true)("e")(
        let("b", Ternary(e.dot("target").dot("closest"), e.dot("target").dot("closest").of(Str("button")), Null)),
        when(b.not || b.dot("closest").of(Str(s"[id^=\"${Html.LivePrefix}\"]")).not)(done),
        let("s", v("busy").of(b)),
        does(v("spun").dot("push").of(arr(b, v("s")))),
        does(v("setTimeout").of(v("unspin"), Num(10000)))),
      Stmt.Comment("the mount changed: the answer came (our own spinner's coming and going is not an answer)"),
      when(v("MutationObserver"))(
        let("watch", New(v("MutationObserver"), Vector(changed))),
        does(v("watch").dot("observe").of(doc.dot("documentElement"),
          obj("childList" -> Bool(true), "subtree" -> Bool(true), "characterData" -> Bool(true))))))

  private val changed: Js =
    val n = v("n")
    val rec = v("rs").at(v("i"))
    fun("rs")(
      Stmt.For(Some(let("i", Num(0))), Some(Bin("<", v("i"), v("rs").dot("length"))), Some(Unary("++", v("i"))), Vector(
        let("n", Bin("||", rec.dot("addedNodes").at(Num(0)), rec.dot("removedNodes").at(Num(0)))),
        when((n && (n.dot("className") === Str("okay-spin"))).not)(does(v("unspin").of()), done))))

  /** the whole program, once per page however often it is run */
  def program: Vector[Stmt] = Vector(
    does(Call(Fun(Vector.empty,
      Vector(when(win.dot("__okayEnhance"))(done), set(win.dot("__okayEnhance"), Num(1))) ++
        picker ++ presses ++ live), Vector.empty)))

  def script: String = Js.print(program)

  /** the classes the program writes, and the look of our list */
  val css: String =
    """select { -webkit-appearance: none; appearance: none; font-family: inherit; color: inherit;
      |  padding: 7px 32px 7px 11px; border: 1px solid var(--okay-line); border-radius: 6px;
      |  background: transparent no-repeat right 11px center / 10px 6px;
      |  background-image: url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='10' height='6'><path d='M1 1l4 4 4-4' stroke='%23888' stroke-width='1.6' fill='none' stroke-linecap='round'/></svg>"); }
      |.okay-pick { position: absolute; z-index: 99999; box-sizing: border-box; max-height: 20rem; overflow: auto;
      |  padding: 4px; border: 1px solid var(--okay-line); border-radius: 8px; box-shadow: 0 10px 28px rgba(0,0,0,.2);
      |  background: Canvas; color: CanvasText; }
      |.okay-pick div { padding: 8px 12px; border-radius: 5px; white-space: nowrap; cursor: default; }
      |.okay-pick div.okay-on { font-weight: 600; }
      |.okay-pick div:hover { background: var(--okay-accent); color: #fff; }
      |.okay-spin { display: inline-block; width: 0.9em; height: 0.9em; margin-left: 9px; vertical-align: -0.12em;
      |  border-radius: 50%; border: 2px solid var(--okay-line); border-top-color: var(--okay-accent);
      |  animation: okay-spin 0.7s linear infinite; }
      |@keyframes okay-spin { to { transform: rotate(360deg); } }
      |.okay-busy { opacity: 0.6; pointer-events: none; }
      |""".stripMargin
