package okay2.codec

import okay2.Optic._
import Json._

final case class PoAddress(city: String, zip: Int)
final case class PoCustomer(name: String, email: String, address: PoAddress)
final case class PoLine(sku: String, qty: Int, price: Double)
sealed trait PoShape
object PoShape {
  final case class Circle(r: Double, secret: String) extends PoShape
  final case class Square(side: Double) extends PoShape
}
final case class PoOrder(id: Int, customer: PoCustomer, lines: Vector[PoLine], shape: PoShape, note: Option[String])
object PoOrder { implicit lazy val schema: Schema[PoOrder] = Schema.derived }

/**
 * A projection policy (okay-codec's TestPolicy, specs/optics-outside.md
 * stage 7): the audit names exactly the fields the projection removes.
 */
class TestPolicy extends munit.FunSuite {

  val order = PoOrder(7, PoCustomer("ada", "ada@x", PoAddress("Wrocław", 50001)),
    Vector(PoLine("a", 2, 9.5), PoLine("b", 1, 100.0)), PoShape.Circle(1.5, "s"), Some("hi"))
  def enc[A](a: A)(implicit s: Schema[A]): Json = Json.parse(Json.write(a))
  val doc = enc(order)

  val policy: Policy[PoOrder] = Policy.hide[PoOrder]("customer.email", "lines.price", "shape.secret", "note").toOption.get

  /** every dotted key present in a document, lists element-wise */
  def keysOf(j: Json, prefix: String = ""): Set[String] = j match {
    case JObj(fs) => fs.flatMap { case (k, v) =>
      val key = if (prefix.isEmpty) k else s"$prefix.$k"
      Set(key) ++ keysOf(v, key)
    }.toSet
    case JArr(vs) => vs.flatMap(v => keysOf(v, prefix)).toSet
    case _ => Set.empty
  }

  test("a key the schema does not write is refused by name, at construction") {
    assertEquals(Policy.hide[PoOrder]("nosuch").left.map(_.contains("nosuch")), Left(true))
    assertEquals(Policy.hide[PoOrder]("customer.nosuch").isLeft, true)
    assertEquals(Policy.hide[PoOrder]("lines.nosuch").isLeft, true)
    assertEquals(Policy.hide[PoOrder]("customer").isRight, true)
  }

  test("DESCRIBE needs no document: touches is the declaration") {
    assertEquals(policy.touches, Set("customer.email", "lines.price", "shape.secret", "note"))
  }

  test("THE LAW: the keys project removes are exactly touches, restricted to what the document has") {
    val removed = keysOf(doc) -- keysOf(policy.project(doc))
    def unwrapCase(k: String) = k.replace("shape.Circle.", "shape.").replace("shape.Square.", "shape.")
    assertEquals(removed.map(unwrapCase), policy.touches)
    // the other case, and an absent note (written as null, so present and removed)
    val square = enc(order.copy(shape = PoShape.Square(2.0), note = None))
    val removed2 = (keysOf(square) -- keysOf(policy.project(square))).map(unwrapCase)
    assertEquals(removed2, Set("customer.email", "lines.price", "note"))
    assert(removed2.subsetOf(policy.touches))
  }

  test("through a list every element loses the field; the rest of each element stays") {
    policy.project(doc) match {
      case JObj(fs) => fs.collectFirst { case ("lines", JArr(vs)) => vs } match {
        case Some(vs) =>
          assertEquals(vs.map { case JObj(f) => f.map(_._1); case _ => Vector() }, Vector(Vector("sku", "qty"), Vector("sku", "qty")))
        case None => fail("lines gone")
      }
      case _ => fail("not an object")
    }
  }

  test("redact keeps the keys and replaces the values; the optic of a key sees the same values") {
    val red = policy.redact(doc)
    assertEquals(keysOf(red), keysOf(doc))
    assertEquals(policy.optic("lines.price").map(_.toVector(doc)), Some(Vector[Json](JNum(9.5), JNum(100.0))))
    assertEquals(policy.optic("lines.price").map(_.toVector(red)), Some(Vector[Json](JStr("[redacted]"), JStr("[redacted]"))))
    assertEquals(policy.optic("customer.email").map(_.toVector(red)), Some(Vector[Json](JStr("[redacted]"))))
    assertEquals(policy.optic("id"), None)
  }

  test("text: what an embedding or a log line may see has no price and no email") {
    val t = policy.text(order)
    assert(!t.contains("ada@x") && !t.contains("9.5") && !t.contains("\"s\""), t)
    assert(t.contains("Wrocław") && t.contains("\"sku\""), t)
  }
}
