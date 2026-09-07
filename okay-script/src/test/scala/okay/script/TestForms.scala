package okay.script

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{Body, Http, Request, Response as HttpResponse}
import okay.script.api.{Forms, Live}
import okay.ui.Event

import java.nio.file.{Files, Path}

/** okay-script-forms: a form from a Schema on both roads -- the plain
 * <form method=post> and the Live app. See specs/okay-script.md
 * "Typed forms".
 */
class TestForms extends munit.FunSuite:

  final case class Order(name: String, qty: Int, gift: Boolean, note: Option[String])
  given Schema[Order] = Schema.derived

  final case class Cart(items: List[String])
  given Schema[Cart] = Schema.derived

  enum Pay:
    case Card(number: String)
    case Invoice(company: String)
  given Schema[Pay] = Schema.derived

  final case class Paid(name: String, pay: Pay)
  given Schema[Paid] = Schema.derived

  private val enough: okay.ui.Form.Check[Order] = o => if o.qty > 0 then Vector.empty else Vector("qty" -> "at least one")

  test("Forms.html names every field by its key, posts to action, carries the submit button and the errors") {
    val h = Forms.html[Order]("/checkout", submit = "Place order")
    assert(h.startsWith("""<form method="post" action="/checkout">"""), h)
    assert(h.contains("""name="name""""), h)
    assert(h.contains("""name="qty""""), h)
    assert(h.contains("""type="checkbox" name="gift" value="on""""), h)
    assert(h.endsWith("""<button type="submit">Place order</button></form>"""), h)
    val withErrors = Forms.html[Order]("/checkout", Forms.Draft(okay.codec.Json.JObj(Vector.empty), Vector("qty" -> "required", "" -> "no stock")))
    assert(withErrors.contains("! required") && withErrors.contains("! no stock"), withErrors)
  }

  test("Forms.read: a good post is Right; a missing required field, a failing check, an unposted checkbox") {
    assertEquals(Forms.read[Order](Map("name" -> "Ann", "qty" -> "2", "gift" -> "on", "note" -> "")), Right(Order("Ann", 2, true, None)))
    assertEquals(Forms.read[Order](Map("name" -> "Ann", "qty" -> "2")), Right(Order("Ann", 2, false, None)))
    Forms.read[Order](Map("name" -> "Ann")) match
      case Left(d) => assert(d.errors.exists(_._1 == "qty"), d.errors.toString)
      case Right(o) => fail(s"read $o without a qty")
    Forms.read[Order](Map("name" -> "Ann", "qty" -> "0"), enough) match
      case Left(d) => assertEquals(d.errors, Vector("qty" -> "at least one"))
      case Right(o) => fail(s"the check let $o through")
  }

  test("Forms.read: a list's + is an edit, not a submit -- the Draft is one item longer; a sum's case knob changes the fields below") {
    Forms.read[Cart](Map("__press" -> "items$add")) match
      case Left(d) =>
        assert(Forms.html[Cart]("/cart", d).contains("""name="items[0]""""), Forms.html[Cart]("/cart", d))
      case Right(c) => fail(s"a + submitted $c")
    Forms.read[Cart](Map("items[0]" -> "a", "items[1]" -> "b", "__press" -> "items$add")) match
      case Left(d) => assert(Forms.html[Cart]("/cart", d).contains("""name="items[2]""""), Forms.html[Cart]("/cart", d))
      case Right(c) => fail(s"a + submitted $c")
    assertEquals(Forms.read[Cart](Map("items[0]" -> "a", "items[1]" -> "b")), Right(Cart(List("a", "b"))))
    assertEquals(Forms.read[Paid](Map("name" -> "Ann", "pay.$case" -> "Invoice", "pay.company" -> "ACME")), Right(Paid("Ann", Pay.Invoice("ACME"))))
    assertEquals(Forms.read[Paid](Map("name" -> "Ann", "pay.$case" -> "Card", "pay.number" -> "4111")), Right(Paid("Ann", Pay.Card("4111"))))
  }

  test("Live.form: edits fold in; a bad submit shows the error under its field and calls nothing; a good one calls submit once and clears") {
    var placed = Vector.empty[Order]
    val app = Live.form[Order](o => { placed :+= o; s"ordered ${o.qty}" }, enough)
    val s1 = Seq(Event.Edited("name", "Ann"), Event.Edited("qty", "0")).foldLeft(app.init)(app.update)
    val s2 = app.update(s1, Event.Pressed(Live.SubmitKey))
    assertEquals(s2.errors, Vector("qty" -> "at least one"))
    assertEquals(placed, Vector.empty)
    assert(Live.html(app.view(s2)).contains("! at least one"))
    val s3 = app.update(app.update(s2, Event.Edited("qty", "3")), Event.Pressed(Live.SubmitKey))
    assertEquals(placed, Vector(Order("Ann", 3, false, None)))
    assertEquals(s3.message, Some("ordered 3"))
    assertEquals(s3.value, Forms.defaults[Order])
  }

  test("the plain road through a Site: the checkout page renders the form on GET and the order on POST") {
    val root = Files.createTempDirectory("okay-script-forms-")
    Files.writeString(root.resolve("checkout.md"),
      """```scala declare
        |import okay.script.api.*
        |import okay.codec.Schema
        |final case class Order(name: String, qty: Int)
        |given Schema[Order] = Schema.derived
        |```
        |```scala
        |import okay.script.api.*
        |val posted = if Web.current.method == "POST" then Forms.read[Order](Web.current.form) else Left(Forms.Draft.empty)
        |posted match
        |  case Right(o) => println(s"<p>thanks ${o.name}, ${o.qty} pcs</p>")
        |  case Left(d) => println(Forms.html[Order]("/checkout", d))
        |```
        |""".stripMargin): Unit
    val site = Site(root)
    def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
    try
      val get = text(site.handle(Request.get("/checkout")))
      assert(get.contains("""<form method="post" action="/checkout">""") && get.contains("""name="qty""""), get)
      val form = Seq("Content-Type" -> "application/x-www-form-urlencoded")
      val bad = text(site.handle(Request.post("/checkout", Body.Text("name=Ann&qty=lots"), form)))
      assert(bad.contains("<form") && bad.contains("! ") && bad.contains("""value="Ann""""), bad)
      val ok = text(site.handle(Request.post("/checkout", Body.Text("name=Ann&qty=2"), form)))
      assert(ok.contains("thanks Ann, 2 pcs"), ok)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
