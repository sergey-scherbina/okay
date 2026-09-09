package okay.codec

/**
 * Whether the other side still reads our messages (specs/codecs.md,
 * "Compatibility between two versions"). Every verdict here is
 * checked against what the DECODERS in this module actually do: the
 * test encodes with one schema and decodes with the other, and
 * asserts that the report predicted the outcome. A rule nobody can
 * demonstrate is a rule nobody should trust.
 */
class TestCompat extends munit.FunSuite:
  import Compat.*

  // ── the two versions of one message, in several shapes ───────────

  final case class V1(id: String, total: Long)
  final case class V2Added(id: String, total: Long, currency: String)
  final case class V2AddedOptional(id: String, total: Long, currency: Option[String])
  final case class V2AddedDefault(id: String, total: Long, currency: String = "EUR")
  final case class V2Removed(id: String)
  final case class V2Retyped(id: String, total: String)

  given Schema[V1] = Schema.derived
  given Schema[V2Added] = Schema.derived
  given Schema[V2AddedOptional] = Schema.derived
  given Schema[V2AddedDefault] = Schema.derived
  given Schema[V2Removed] = Schema.derived
  given Schema[V2Retyped] = Schema.derived

  /** does a value written by `from` decode as `to`, on this wire? */
  def reads[A, B](from: Schema[A], to: Schema[B], a: A, wire: Wire): Boolean = wire match
    case Wire.Json => Json.decode(to)(Json.parse(Json.encode(from)(a))).isRight
    case Wire.Cbor => Cbor.read(Cbor.write(a)(using from))(using to).isRight

  /** the whole point: what the report SAYS is what the decoders DO */
  def agrees[A, B](old: Schema[A], next: Schema[B], oldValue: A, newValue: B): Unit =
    val r = compare(old, next)
    for wire <- Wire.values do
      assertEquals(r.backward(wire).compatible, reads(old, next, oldValue, wire),
        s"backward on $wire: the report says ${r.backward(wire)} — ${r.render}")
      assertEquals(r.forward(wire).compatible, reads(next, old, newValue, wire),
        s"forward on $wire: the report says ${r.forward(wire)} — ${r.render}")

  test("no change: compatible both ways, nothing to say") {
    val r = compare(summon[Schema[V1]], summon[Schema[V1]])
    assert(r.isEmpty)
    assert(r.rolling(Wire.Json).compatible && r.rolling(Wire.Cbor).compatible)
    assertEquals(r.render, "no change\nJson backward (new reader, old bytes): compatible\nJson forward (old reader, new bytes): compatible\nCbor backward (new reader, old bytes): compatible\nCbor forward (old reader, new bytes): compatible\n")
  }

  test("a new REQUIRED field: the new reader cannot read old bytes, either wire") {
    val r = compare(summon[Schema[V1]], summon[Schema[V2Added]])
    assertEquals(r.changes, Vector(Change.FieldAdded("", "currency", optional = false, defaulted = false)))
    assert(!r.backward(Wire.Json).compatible)
    assert(!r.backward(Wire.Cbor).compatible)
    assert(r.backward(Wire.Json).reasons.head.contains("currency is new and required"))
    agrees(summon[Schema[V1]], summon[Schema[V2Added]], V1("o1", 10), V2Added("o1", 10, "EUR"))
  }

  test("a new OPTIONAL field: backward is fine; forward differs BY WIRE — Json ignores it, Cbor refuses") {
    val r = compare(summon[Schema[V1]], summon[Schema[V2AddedOptional]])
    assertEquals(r.changes, Vector(Change.FieldAdded("", "currency", optional = true, defaulted = false)))
    assert(r.backward(Wire.Json).compatible && r.backward(Wire.Cbor).compatible)
    assert(r.forward(Wire.Json).compatible, "Json ignores a field the reader does not declare")
    assert(!r.forward(Wire.Cbor).compatible, "Cbor refuses it by name")
    assert(r.forward(Wire.Cbor).reasons.head.contains("refuses a field it does not declare"))
    agrees(summon[Schema[V1]], summon[Schema[V2AddedOptional]], V1("o1", 10), V2AddedOptional("o1", 10, Some("EUR")))
  }

  test("a new DEFAULTED field reads like an optional one — the declaration is the fallback") {
    val r = compare(summon[Schema[V1]], summon[Schema[V2AddedDefault]])
    assertEquals(r.changes, Vector(Change.FieldAdded("", "currency", optional = false, defaulted = true)))
    assert(r.backward(Wire.Json).compatible && r.backward(Wire.Cbor).compatible)
    agrees(summon[Schema[V1]], summon[Schema[V2AddedDefault]], V1("o1", 10), V2AddedDefault("o1", 10))
  }

  test("a REMOVED required field: the old reader cannot read new bytes; Cbor also refuses backward") {
    val r = compare(summon[Schema[V1]], summon[Schema[V2Removed]])
    assertEquals(r.changes, Vector(Change.FieldRemoved("", "total", optional = false, defaulted = false)))
    assert(r.backward(Wire.Json).compatible, "Json skips the field the new reader dropped")
    assert(!r.backward(Wire.Cbor).compatible)
    assert(!r.forward(Wire.Json).compatible && !r.forward(Wire.Cbor).compatible)
    agrees(summon[Schema[V1]], summon[Schema[V2Removed]], V1("o1", 10), V2Removed("o1"))
  }

  test("a RETYPED field is incompatible in both directions and names both types") {
    val r = compare(summon[Schema[V1]], summon[Schema[V2Retyped]])
    assertEquals(r.changes, Vector(Change.TypeChanged("total", "Long", "String")))
    for wire <- Wire.values do
      assert(!r.backward(wire).compatible && !r.forward(wire).compatible)
    assert(r.backward(Wire.Json).reasons.head.contains("total changed from Long to String"))
    agrees(summon[Schema[V1]], summon[Schema[V2Retyped]], V1("o1", 10), V2Retyped("o1", "10"))
  }

  // ── sums ─────────────────────────────────────────────────────────

  enum EventV1 derives Schema:
    case Placed(id: String)
    case Cancelled(id: String)
  enum EventV2 derives Schema:
    case Placed(id: String)
    case Cancelled(id: String)
    case Refunded(id: String, amount: Long)

  test("a new CASE: the new reader reads old bytes, the old reader refuses the new case") {
    val r = compare(summon[Schema[EventV1]], summon[Schema[EventV2]])
    assertEquals(r.changes, Vector(Change.CaseAdded("", "Refunded")))
    assert(r.backward(Wire.Json).compatible && r.backward(Wire.Cbor).compatible)
    assert(!r.forward(Wire.Json).compatible && !r.forward(Wire.Cbor).compatible)
    assert(r.forward(Wire.Cbor).reasons.head.contains("refuses a case it does not know"))
    agrees(summon[Schema[EventV1]], summon[Schema[EventV2]], EventV1.Placed("o1"), EventV2.Refunded("o1", 5))
    // and a case both know still round-trips across the versions
    assert(reads(summon[Schema[EventV2]], summon[Schema[EventV1]], EventV2.Placed("o1"), Wire.Json))
  }

  test("a REMOVED case is the mirror: the new reader refuses what the old one still writes") {
    val r = compare(summon[Schema[EventV2]], summon[Schema[EventV1]])
    assertEquals(r.changes, Vector(Change.CaseRemoved("", "Refunded")))
    assert(!r.backward(Wire.Json).compatible)
    assert(r.forward(Wire.Json).compatible && r.forward(Wire.Cbor).compatible)
  }

  // ── nesting, wrappers, recursion ─────────────────────────────────

  final case class Line(sku: String)
  final case class LineV2(sku: String, qty: Option[Int])
  final case class OrderV1(id: String, lines: Vector[Line])
  final case class OrderV2(id: String, lines: Vector[LineV2])
  given Schema[Line] = Schema.derived
  given Schema[LineV2] = Schema.derived
  given Schema[OrderV1] = Schema.derived
  given Schema[OrderV2] = Schema.derived

  test("a change inside a nested collection is found and its PATH names it") {
    val r = compare(summon[Schema[OrderV1]], summon[Schema[OrderV2]])
    assertEquals(r.changes, Vector(Change.FieldAdded("lines.", "qty", optional = true, defaulted = false)))
    assert(r.backward(Wire.Cbor).compatible)
    assert(!r.forward(Wire.Cbor).compatible)
    agrees(summon[Schema[OrderV1]], summon[Schema[OrderV2]],
      OrderV1("o1", Vector(Line("a"))), OrderV2("o1", Vector(LineV2("a", Some(2)))))
  }

  final case class Wrapped(v: String)
  test("a wrapper does not exist to the wire, so it is no change at all") {
    given Schema[Wrapped] = Schema.wrap[Wrapped, String](Wrapped(_), _.v)
    assert(compare(summon[Schema[Wrapped]], summon[Schema[String]]).isEmpty)
    assert(compare(summon[Schema[String]], summon[Schema[Wrapped]]).isEmpty)
  }

  final case class Tree(label: String, kids: Vector[Tree])
  final case class TreeV2(label: String, kids: Vector[TreeV2], note: Option[String])
  object Tree:
    given Schema[Tree] = Schema.derived
  object TreeV2:
    given Schema[TreeV2] = Schema.derived

  test("a self-referential type terminates and reports its change once") {
    val r = compare(summon[Schema[Tree]], summon[Schema[TreeV2]])
    assertEquals(r.changes, Vector(Change.FieldAdded("", "note", optional = true, defaulted = false)))
  }

  test("List and Vector are the same array on both wires: no change") {
    assert(compare(summon[Schema[List[Int]]], summon[Schema[Vector[Int]]]).isEmpty)
  }

  test("a different shape at the root is named, not silently walked") {
    val r = compare(summon[Schema[V1]], summon[Schema[EventV1]])
    assertEquals(r.changes, Vector(Change.ShapeChanged("the root", "V1", "EventV1")))
    assert(!r.rolling(Wire.Json).compatible)
  }

  test("render prints the changes and both verdicts per wire") {
    val out = compare(summon[Schema[V1]], summon[Schema[V2Added]]).render
    assert(out.startsWith("1 change(s):"), out)
    assert(out.contains("Json backward (new reader, old bytes): INCOMPATIBLE"), out)
    assert(out.contains("currency is new and required"), out)
    assert(out.contains("Cbor forward (old reader, new bytes): INCOMPATIBLE"), out)
  }
