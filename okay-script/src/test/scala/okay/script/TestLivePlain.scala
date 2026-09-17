package okay.script

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{Body, Http, Request, Response as HttpResponse}
import okay.script.api.{Live, Session}
import okay.ui.{Event, Ui}

import java.nio.file.{Files, Path}

/** script-live-plain: a Live app on the plain road -- the tree as one
 * `<form method="post">`, every press a POST, no script. See
 * specs/okay-script.md "The plain road of a Live app".
 */
class TestLivePlain extends munit.FunSuite:

  private def counter: Live[Int] =
    Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc")), "root"))(
      (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })

  /** an app that records every event it was given */
  private def recorder(view: Ui): Live[Vector[Event]] =
    Live(Vector.empty[Event])(_ => view)((es, e) => es :+ e)

  test("step on the counter: a shown press steps once; an unshown key and a GET step nothing") {
    val app = counter
    assertEquals(Live.step(app, 0, Map("__press" -> "inc")), 1)
    assertEquals(Live.step(app, 3, Map("__press" -> "dec")), 3)
    assertEquals(Live.step(app, 3, Map.empty), 3)
  }

  test("step diffs the post against the shown tree: unchanged is nothing, changed is an event, an unposted checkbox is false only when it was on") {
    val shown = Ui.Column(Vector(
      Ui.Input("Ann", "name"), Ui.Input("", "note", "", okay.ui.InputKind.Multiline),
      Ui.Check(true, "gift"), Ui.Check(false, "rush"),
      Ui.Select(Vector("x", "y", "z"), 1, "pick"),
      Ui.Button("go", "go")))
    val app = recorder(shown)
    // everything posted back as shown: no event but the press
    assertEquals(Live.step(app, Vector.empty, Map("name" -> "Ann", "note" -> "", "gift" -> "on", "pick" -> "y", "__press" -> "go")),
      Vector(Event.Pressed("go")))
    // every field changed, the checkbox dropped, the other ticked, the select by option text
    assertEquals(Live.step(app, Vector.empty, Map("name" -> "Bob", "note" -> "hi", "rush" -> "on", "pick" -> "z", "__press" -> "go")),
      Vector(Event.Edited("name", "Bob"), Event.Edited("note", "hi"), Event.Toggled("gift", false), Event.Toggled("rush", true),
        Event.Chosen("pick", 2), Event.Pressed("go")))
    // a Select posted with a value that is no option is nothing
    assertEquals(Live.step(app, Vector.empty, Map("name" -> "Ann", "gift" -> "on", "pick" -> "q")), Vector.empty)
  }

  final case class Order(name: String, qty: Int)
  given Schema[Order] = Schema.derived
  final case class Cart(items: List[String])
  given Schema[Cart] = Schema.derived

  test("step on a Form node: the form's own button submits its fields as ONE Submitted; a + inside it is a press after plain edits") {
    var placed = Vector.empty[Order]
    val app = Live.form[Order](o => { placed :+= o; s"ordered ${o.qty}" })
    val s1 = Live.step(app, app.init, Map("name" -> "Ann", "qty" -> "2", "__press" -> Live.SubmitKey))
    assertEquals(placed, Vector(Order("Ann", 2)))
    assertEquals(s1.message, Some("ordered 2"))
    // the same through a recorder: what update was handed
    val fields = Ui.focusable(okay.ui.Form.of[Order](okay.codec.Json.JObj(Vector.empty))).filterNot(_.isInstanceOf[Ui.Button])
    val rec = recorder(Ui.Column(Vector(Ui.Input("", "outside"), Ui.Form(fields, "Submit", "f"))))
    assertEquals(Live.step(rec, Vector.empty, Map("outside" -> "o", "name" -> "Ann", "qty" -> "2", "__press" -> "f")),
      Vector(Event.Edited("outside", "o"), Event.Submitted("f", Vector(Event.Edited("name", "Ann"), Event.Edited("qty", "2")))))
    // a list's + is an edit, not a submit: nothing placed, the draft grows
    var carts = Vector.empty[Cart]
    val cart = Live.form[Cart](c => { carts :+= c; "ok" })
    val grown = Live.step(cart, cart.init, Map("__press" -> "items$add"))
    assertEquals(carts, Vector.empty)
    assert(Live.html(cart.view(grown), named = true).contains("""name="items[0]""""), Live.html(cart.view(grown), named = true))
    // ...and its edits travel as plain Edited before that press
    val recCart = recorder(Ui.Form(Vector(Ui.Input("", "items[0]"), Ui.Button("+", "items$add")), "Submit", "f"))
    assertEquals(Live.step(recCart, Vector.empty, Map("items[0]" -> "a", "__press" -> "items$add")),
      Vector(Event.Edited("items[0]", "a"), Event.Pressed("items$add")))
    // a Submitted naming a form that is not shown is dropped: the press is not a form's, so it is a Pressed of an unshown key -> dropped
    assertEquals(Live.step(recCart, Vector.empty, Map("__press" -> "ghost")), Vector.empty)
  }

  test("plain: one form, the hidden mount field, every field named -- the textarea too -- and no script") {
    val h = Live.plain("c", Ui.Column(Vector(Ui.Input("", "note", "", okay.ui.InputKind.Multiline), Ui.Button("+1", "inc"))), "/counter")
    assert(h.startsWith("""<form method="post" action="/counter" id="okay-live-c" class="okay-plain"><input type="hidden" name="__okay_plain" value="c">"""), h)
    assert(h.contains("""<textarea data-key="note" value="" name="note"></textarea>"""), h)
    assert(h.contains("""<button data-key="inc" name="__press" value="inc">+1</button>"""), h)
    assert(h.endsWith("</form>") && !h.contains("<script"), h)
  }

  test("post: keyed state is kept between posts, another mount's post steps nothing, and a durable app's state is read back by a fresh instance") {
    val app = counter
    val inc = Map(Live.PlainField -> "c", "__press" -> "inc")
    assertEquals(app.post(Some("k"), "c", inc), 1)
    assertEquals(app.post(Some("k"), "c", inc), 2)
    assertEquals(app.post(Some("k"), "c", Map(Live.PlainField -> "other", "__press" -> "inc")), 2)
    assertEquals(app.post(Some("k"), "c", Map.empty), 2)
    assertEquals(app.post(Some("other"), "c", Map.empty), 0)
    assertEquals(app.resumedFor("k"), Some(2))
    val sess = Session.detached
    val durable = Live.durable(0)(counter.view)(counter.update)
    assertEquals(durable.post(Some("k"), "c", inc, Some(sess), "c"), 1)
    assert(sess.get("okay.live.c").isDefined, sess.attributes.toString)
    val fresh = Live.durable(0)(counter.view)(counter.update)
    assertEquals(fresh.post(None, "c", Map.empty, Some(sess), "c"), 1)
  }

  test("through a Site: GET is the form with no script; two POSTs with the cookie reach 2; a POST without it starts over") {
    val root = Files.createTempDirectory("okay-script-live-plain-")
    Files.writeString(root.resolve("counter.md"),
      """```scala declare
        |import okay.ui.*
        |import okay.script.api.*
        |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc"))))(
        |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })
        |```
        |${mountPlain("counter", counter)}
        |""".stripMargin): Unit
    val site = Site(root)
    def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
    val form = Seq("Content-Type" -> "application/x-www-form-urlencoded")
    try
      val get = site.handle(Request.get("/counter"))
      val body = text(get)
      assert(body.contains("""<form method="post" action="/counter" id="okay-live-counter""""), body)
      assert(body.contains("""name="__okay_plain" value="counter"""") && body.contains("count: 0") && !body.contains("<script"), body)
      val cookie = get.headers.collect { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(s"${Site.SessionCookie}=") => v }
        .head.drop(Site.SessionCookie.length + 1).takeWhile(_ != ';')
      val withCookie = form :+ ("Cookie" -> s"${Site.SessionCookie}=$cookie")
      val one = text(site.handle(Request.post("/counter", Body.Text("__okay_plain=counter&__press=inc"), withCookie)))
      assert(one.contains("count: 1"), one)
      val two = text(site.handle(Request.post("/counter", Body.Text("__okay_plain=counter&__press=inc"), withCookie)))
      assert(two.contains("count: 2"), two)
      val fresh = text(site.handle(Request.post("/counter", Body.Text("__okay_plain=counter&__press=inc"), form)))
      assert(fresh.contains("count: 1"), fresh)
      val again = text(site.handle(Request.get("/counter", withCookie)))
      assert(again.contains("count: 2"), again)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
