package okay2.codec

sealed trait CpEventV1
object CpEventV1 {
  final case class Placed(id: String) extends CpEventV1
  final case class Cancelled(id: String) extends CpEventV1
}
sealed trait CpEventV2
object CpEventV2 {
  final case class Placed(id: String) extends CpEventV2
  final case class Cancelled(id: String) extends CpEventV2
  final case class Refunded(id: String, amount: Long) extends CpEventV2
}
final case class CpTree(label: String, kids: Vector[CpTree])
object CpTree {
  implicit lazy val schema: Schema[CpTree] = Schema.derived
}
final case class CpTreeV2(label: String, kids: Vector[CpTreeV2], note: Option[String])
object CpTreeV2 {
  implicit lazy val schema: Schema[CpTreeV2] = Schema.derived
}

final case class CpV1(id: String, total: Long)
final case class CpV2Added(id: String, total: Long, currency: String)
final case class CpV2AddedOptional(id: String, total: Long, currency: Option[String])
final case class CpV2AddedDefault(id: String, total: Long, currency: String = "EUR")
final case class CpV2Removed(id: String)
final case class CpV2Retyped(id: String, total: String)

/**
 * Whether the other side still reads our messages (okay-codec's
 * TestCompat). Every verdict is checked against what the DECODERS do:
 * encode with one schema, decode with the other, on both wires, and
 * assert that the report predicted the outcome.
 */
class TestCompat extends munit.FunSuite {
  import Compat._

  private val v1: Schema[CpV1] = Schema.derived
  private val v2Added: Schema[CpV2Added] = Schema.derived
  private val v2AddedOptional: Schema[CpV2AddedOptional] = Schema.derived
  private val v2AddedDefault: Schema[CpV2AddedDefault] = Schema.derived
  private val v2Removed: Schema[CpV2Removed] = Schema.derived
  private val v2Retyped: Schema[CpV2Retyped] = Schema.derived
  private val eventV1: Schema[CpEventV1] = implicitly
  private val eventV2: Schema[CpEventV2] = implicitly

  /** does a value written by `from` decode as `to`? Asked of BOTH
   * wires, which must agree (cbor-unknown-fields) */
  private def reads[A, B](from: Schema[A], to: Schema[B], a: A): Boolean = {
    val json = Json.decode(to)(Json.parse(Json.encode(from)(a))).isRight
    val cbor = Cbor.read[B](Cbor.write(a)(from))(to).isRight
    assertEquals(cbor, json, "the two wires must answer alike")
    json
  }

  /** what the report SAYS is what the decoders DO */
  private def agrees[A, B](old: Schema[A], next: Schema[B], oldValue: A, newValue: B): Unit = {
    val r = compare(old, next)
    assertEquals(r.backward.compatible, reads(old, next, oldValue), s"backward: ${r.render}")
    assertEquals(r.forward.compatible, reads(next, old, newValue), s"forward: ${r.render}")
  }

  test("no change: compatible both ways, nothing to say") {
    val r = compare(v1, v1)
    assert(r.isEmpty)
    assert(r.rolling.compatible)
    assertEquals(r.render, "no change\nbackward (new reader, old bytes): compatible\nforward (old reader, new bytes): compatible\n")
  }

  test("a new REQUIRED field: the new reader cannot read old bytes, either wire") {
    val r = compare(v1, v2Added)
    assertEquals(r.changes, Vector[Change](Change.FieldAdded("", "currency", optional = false, defaulted = false)))
    assert(!r.backward.compatible)
    assert(r.backward.reasons.head.contains("currency is new and required"))
    agrees(v1, v2Added, CpV1("o1", 10), CpV2Added("o1", 10, "EUR"))
  }

  test("a new OPTIONAL field is safe in BOTH directions, on both wires") {
    val r = compare(v1, v2AddedOptional)
    assertEquals(r.changes, Vector[Change](Change.FieldAdded("", "currency", optional = true, defaulted = false)))
    assert(r.backward.compatible)
    assert(r.forward.compatible, "an old reader skips a field it does not declare")
    agrees(v1, v2AddedOptional, CpV1("o1", 10), CpV2AddedOptional("o1", 10, Some("EUR")))
  }

  test("a new DEFAULTED field reads like an optional one — the declaration is the fallback") {
    val r = compare(v1, v2AddedDefault)
    assertEquals(r.changes, Vector[Change](Change.FieldAdded("", "currency", optional = false, defaulted = true)))
    assert(r.rolling.compatible)
    agrees(v1, v2AddedDefault, CpV1("o1", 10), CpV2AddedDefault("o1", 10))
  }

  test("a REMOVED required field: only the old reader is hurt, and it is hurt on both wires") {
    val r = compare(v1, v2Removed)
    assertEquals(r.changes, Vector[Change](Change.FieldRemoved("", "total", optional = false, defaulted = false)))
    assert(r.backward.compatible)
    assert(!r.forward.compatible)
    agrees(v1, v2Removed, CpV1("o1", 10), CpV2Removed("o1"))
  }

  test("a RETYPED field is incompatible in both directions and names both types") {
    val r = compare(v1, v2Retyped)
    assertEquals(r.changes, Vector[Change](Change.TypeChanged("total", "Long", "String")))
    assert(!r.backward.compatible && !r.forward.compatible)
    assert(r.backward.reasons.head.contains("total changed from Long to String"))
    agrees(v1, v2Retyped, CpV1("o1", 10), CpV2Retyped("o1", "10"))
  }

  test("a new CASE: the new reader reads old bytes, the old reader refuses the new case") {
    val r = compare(eventV1, eventV2)
    assertEquals(r.changes, Vector[Change](Change.CaseAdded("", "Refunded")))
    assert(r.backward.compatible)
    assert(!r.forward.compatible)
    assert(r.forward.reasons.head.contains("refuses a case it does not know"))
    agrees[CpEventV1, CpEventV2](eventV1, eventV2, CpEventV1.Placed("o1"), CpEventV2.Refunded("o1", 5))
    assert(reads[CpEventV2, CpEventV1](eventV2, eventV1, CpEventV2.Placed("o1")))
  }

  test("a REMOVED case is the mirror: the new reader refuses what the old one still writes") {
    val r = compare(eventV2, eventV1)
    assertEquals(r.changes, Vector[Change](Change.CaseRemoved("", "Refunded")))
    assert(!r.backward.compatible)
    assert(r.forward.compatible)
    agrees[CpEventV2, CpEventV1](eventV2, eventV1, CpEventV2.Refunded("o1", 5), CpEventV1.Placed("o1"))
  }

  test("a change inside a nested collection is found and its PATH names it") {
    final case class Line(sku: String)
    final case class LineV2(sku: String, qty: Option[Int])
    final case class OrderV1(id: String, lines: Vector[Line])
    final case class OrderV2(id: String, lines: Vector[LineV2])
    implicit val line: Schema[Line] = Schema.derived
    implicit val lineV2: Schema[LineV2] = Schema.derived
    val orderV1: Schema[OrderV1] = Schema.derived
    val orderV2: Schema[OrderV2] = Schema.derived
    val r = compare(orderV1, orderV2)
    assertEquals(r.changes, Vector[Change](Change.FieldAdded("lines.", "qty", optional = true, defaulted = false)))
    assert(r.backward.compatible && r.forward.compatible)
    agrees(orderV1, orderV2, OrderV1("o1", Vector(Line("a"))), OrderV2("o1", Vector(LineV2("a", Some(2)))))
  }

  test("a wrapper does not exist to the wire, so it is no change at all") {
    final case class Wrapped(v: String)
    val wrapped: Schema[Wrapped] = Schema.wrap[Wrapped, String](Wrapped(_), _.v)
    val string: Schema[String] = implicitly
    assert(compare(wrapped, string).isEmpty)
    assert(compare(string, wrapped).isEmpty)
    assert(reads(wrapped, string, Wrapped("x")))
    assert(reads(string, wrapped, "x"))
  }

  test("a self-referential type terminates and reports its change once") {
    val r = compare(CpTree.schema, CpTreeV2.schema)
    assertEquals(r.changes, Vector[Change](Change.FieldAdded("", "note", optional = true, defaulted = false)))
    agrees(CpTree.schema, CpTreeV2.schema,
      CpTree("root", Vector(CpTree("kid", Vector.empty))),
      CpTreeV2("root", Vector(CpTreeV2("kid", Vector.empty, None)), Some("n")))
  }

  test("List and Vector are the same array on both wires: no change") {
    val l: Schema[List[Int]] = implicitly
    val v: Schema[Vector[Int]] = implicitly
    assert(compare(l, v).isEmpty)
    assert(reads(l, v, List(1, 2, 3)))
    assert(reads(v, l, Vector(1, 2, 3)))
  }

  test("a different shape at the root is named, not silently walked") {
    val r = compare(v1, eventV1)
    assertEquals(r.changes, Vector[Change](Change.ShapeChanged("the root", "CpV1", "CpEventV1")))
    assert(!r.rolling.compatible)
    agrees[CpV1, CpEventV1](v1, eventV1, CpV1("o1", 10), CpEventV1.Placed("o1"))
  }

  test("render prints the changes and both verdicts") {
    val out = compare(v1, v2Added).render
    assert(out.startsWith("1 change(s):"), out)
    assert(out.contains("backward (new reader, old bytes): INCOMPATIBLE"), out)
    assert(out.contains("currency is new and required"), out)
    assert(out.contains("forward (old reader, new bytes): compatible"), out)
  }
}
