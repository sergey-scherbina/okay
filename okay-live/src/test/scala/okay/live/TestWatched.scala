package okay.live

import okay.*
import okay.given
import okay.codec.{Json, JsonOptic, Schema}
import Json.*

/**
 * specs/optics-outside.md stage 10 — subscribe to a lens: a
 * subscriber sees exactly the DISTINCT consecutive values of its
 * focus over the document's history, and nothing of anyone else's.
 */
class TestWatched extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Customer(name: String, address: Address)
  final case class Line(sku: String, qty: Int)
  final case class Order(id: Int, customer: Customer, lines: Vector[Line])
  given Schema[Address] = Schema.derived
  given Schema[Customer] = Schema.derived
  given Schema[Line] = Schema.derived
  given Schema[Order] = Schema.derived

  val order = Order(7, Customer("ada", Address("Wrocław", 50001)), Vector(Line("a", 2), Line("b", 1)))
  def enc[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))

  test("an unknown key is refused by name; the root is the empty key") {
    val w = Watched[Order](enc(order))
    assert(w.subscribe("customer.nosuch").left.exists(_.contains("nosuch")))
    assertEquals(w.focus(""), Right(Some(enc(order))))
    assertEquals(w.focus("customer.address.city"), Right(Some(JStr("Wrocław"))))
  }

  test("a subscriber is told its part when it changes, and NOT when another part does") {
    val w = Watched[Order](enc(order))
    val address = w.subscribe("customer.address").toOption.get
    val whole = w.subscribe("").toOption.get
    // an unrelated change: the whole-document subscriber sees it, the address one must not
    assertEquals(w.set("lines[0].qty", JNum(5)), Right(()))
    assertEquals(whole.receiveBlocking().flatMap(j => JsonOptic.path(summon[Schema[Order]], "lines[0].qty").flatMap(_.preview(j))), Some(JNum(5)))
    // then a related one: the FIRST thing the address subscriber receives is this, so the unrelated one never reached it
    assertEquals(w.set("customer.address.city", JStr("Kraków")), Right(()))
    assertEquals(address.receiveBlocking(), Some(enc(Address("Kraków", 50001))))
    assertEquals(whole.receiveBlocking().map(j => JsonOptic.path(summon[Schema[Order]], "customer.address.city").flatMap(_.preview(j))), Some(Some(JStr("Kraków"))))
    // a write that changes nothing tells nobody: the next thing seen is the next real change
    assertEquals(w.set("customer.address.city", JStr("Kraków")), Right(()))
    assertEquals(w.set("customer.address.zip", JNum(30001)), Right(()))
    assertEquals(address.receiveBlocking(), Some(enc(Address("Kraków", 30001))))
  }

  test("THE LAW: over a history of edits, a subscriber receives exactly the distinct consecutive values of its focus") {
    val w = Watched[Order](enc(order))
    val keys = Vector("customer.address", "customer.name", "lines[1]", "lines", "")
    val subs = keys.map(k => k -> w.subscribe(k).toOption.get)
    val edits: Vector[(String, Json)] = Vector(
      "customer.name" -> JStr("grace"), "lines[1].qty" -> JNum(9), "customer.address.zip" -> JNum(1),
      "customer.name" -> JStr("grace"), "lines[0].sku" -> JStr("z"), "customer.address.city" -> JStr("Gdańsk"),
      "lines[1].qty" -> JNum(9), "id" -> JNum(8))
    // the expected sequence per key, computed from the history by the same lens
    val docs = edits.scanLeft(enc(order)) { case (d, (k, v)) => JsonOptic.path(summon[Schema[Order]], k).get.set(v)(d) }
    for (k, v) <- edits do assertEquals(w.set(k, v), Right(()))
    for (k, ch) <- subs do
      val at = w.lens(k).toOption.get
      val values = docs.map(at.preview)
      val expected = values.zip(values.tail).collect { case (a, b) if a != b => b.getOrElse(JNull) }
      val got = Vector.fill(expected.length)(ch.receiveBlocking().get)
      assertEquals(got, expected, s"subscriber of `$k`")
    assertEquals(w.get, docs.last)
  }

  test("put: the typed door replaces the whole document through the codec") {
    val w = Watched[Order](enc(order))
    val name = w.subscribe("customer.name").toOption.get
    w.put(order.copy(customer = order.customer.copy(name = "bob")))
    assertEquals(name.receiveBlocking(), Some(JStr("bob")))
    assertEquals(w.get, enc(order.copy(customer = order.customer.copy(name = "bob"))))
  }
}
