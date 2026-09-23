package okay.codec

import okay.*
import okay.given
import Json.*

/**
 * specs/optics-outside.md stage 7 — a projection policy: the audit
 * names exactly the fields the projection removes, on every document.
 */
class TestPolicy extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Customer(name: String, email: String, address: Address)
  final case class Line(sku: String, qty: Int, price: Double)
  enum Shape derives Schema:
    case Circle(r: Double, secret: String)
    case Square(side: Double)
  final case class Order(id: Int, customer: Customer, lines: Vector[Line], shape: Shape, note: Option[String])
  given Schema[Address] = Schema.derived
  given Schema[Customer] = Schema.derived
  given Schema[Line] = Schema.derived
  given Schema[Order] = Schema.derived

  val order = Order(7, Customer("ada", "ada@x", Address("Wrocław", 50001)),
    Vector(Line("a", 2, 9.5), Line("b", 1, 100.0)), Shape.Circle(1.5, "s"), Some("hi"))
  def enc[A](a: A)(using s: Schema[A]): Json = Json.parse(Json.write(a))
  val doc = enc(order)

  val policy: Policy[Order] = Policy.hide[Order]("customer.email", "lines.price", "shape.secret", "note").toOption.get

  /** every dotted key present in a document, lists element-wise */
  def keysOf(j: Json, prefix: String = ""): Set[String] = j match
    case JObj(fs) => fs.flatMap((k, v) => Set(if prefix.isEmpty then k else s"$prefix.$k") ++ keysOf(v, if prefix.isEmpty then k else s"$prefix.$k")).toSet
    case JArr(vs) => vs.flatMap(v => keysOf(v, prefix)).toSet
    case _ => Set.empty

  test("a key the schema does not write is refused by name, at construction") {
    assertEquals(Policy.hide[Order]("nosuch").left.map(_.contains("nosuch")), Left(true))
    assertEquals(Policy.hide[Order]("customer.nosuch").isLeft, true)
    assertEquals(Policy.hide[Order]("lines.nosuch").isLeft, true)
    assertEquals(Policy.hide[Order]("customer").isRight, true)      // a whole sub-record may be hidden
  }

  test("DESCRIBE needs no document: touches is the declaration") {
    assertEquals(policy.touches, Set("customer.email", "lines.price", "shape.secret", "note"))
  }

  test("THE LAW: the keys project removes are exactly touches, restricted to what the document has") {
    val projected = policy.project(doc)
    val removed = keysOf(doc) -- keysOf(projected)
    // the sum's case wrapper is a level the key does not spell: shape.Circle.secret in the document
    def unwrapCase(k: String) = k.replace("shape.Circle.", "shape.").replace("shape.Square.", "shape.")
    assertEquals(removed.map(unwrapCase), policy.touches)
    // the other case, and an absent note: the codec writes None as
    // `"note": null`, so the key IS there and is removed; the Circle's
    // secret is not, because there is no Circle — fewer keys removed,
    // every one of them touched
    val square = enc(order.copy(shape = Shape.Square(2.0), note = None))
    val removed2 = (keysOf(square) -- keysOf(policy.project(square))).map(unwrapCase)
    assertEquals(removed2, Set("customer.email", "lines.price", "note"))
    assert(removed2.subsetOf(policy.touches))
  }

  test("through a list every element loses the field; the rest of each element stays") {
    policy.project(doc) match
      case JObj(fs) => fs.collectFirst { case ("lines", JArr(vs)) => vs } match
        case Some(vs) =>
          assertEquals(vs.map { case JObj(f) => f.map(_._1); case _ => Vector() }, Vector(Vector("sku", "qty"), Vector("sku", "qty")))
        case None => fail("lines gone")
      case _ => fail("not an object")
  }

  test("redact keeps the keys and replaces the values; the optic of a key sees the same values") {
    val red = policy.redact(doc)
    assertEquals(keysOf(red), keysOf(doc))
    assertEquals(policy.optic("lines.price").map(_.toVector(doc)), Some(Vector(JNum(9.5), JNum(100.0))))
    assertEquals(policy.optic("lines.price").map(_.toVector(red)), Some(Vector(JStr("[redacted]"), JStr("[redacted]"))))
    assertEquals(policy.optic("customer.email").map(_.toVector(red)), Some(Vector(JStr("[redacted]"))))
    assertEquals(policy.optic("id"), None)
  }

  test("text: what an embedding or a log line may see has no price and no email") {
    val t = policy.text(order)
    assert(!t.contains("ada@x") && !t.contains("9.5") && !t.contains("\"s\""), t)
    assert(t.contains("Wrocław") && t.contains("\"sku\""), t)
  }
}
