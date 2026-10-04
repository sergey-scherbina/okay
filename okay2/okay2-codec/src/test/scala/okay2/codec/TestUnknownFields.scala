package okay2.codec

sealed trait UfE1
object UfE1 {
  final case class A(x: Int) extends UfE1
}
sealed trait UfE2
object UfE2 {
  final case class A(x: Int) extends UfE2
  final case class B(y: Int) extends UfE2
}

final case class UfV1(id: String, total: Long)
final case class UfV2(id: String, total: Long, currency: String, tags: Vector[String])

/**
 * One Schema, one value, one answer on either wire (okay-codec's
 * TestUnknownFields, cbor-unknown-fields): a field the reader does not
 * declare is SKIPPED by CBOR as by JSON, so adding a field is not a
 * breaking change for every reader already deployed.
 */
class TestUnknownFields extends munit.FunSuite {

  implicit val v1: Schema[UfV1] = Schema.derived
  implicit val v2: Schema[UfV2] = Schema.derived

  private val newer = UfV2("o1", 10, "EUR", Vector("gift", "rush"))

  test("a field the reader does not declare is SKIPPED on both wires, and the rest decodes") {
    val fromJson = Json.decode(v1)(Json.parse(Json.encode(v2)(newer)))
    val fromCbor = Cbor.read[UfV1](Cbor.write(newer))
    assertEquals(fromJson, Right(UfV1("o1", 10)))
    assertEquals(fromCbor, Right(UfV1("o1", 10)), "CBOR must answer what JSON answers")
  }

  test("every shape of skipped value: the reader lands exactly on the next field") {
    final case class Known(a: Int, z: String)
    final case class WithInt(a: Int, skipped: Long, z: String)
    final case class WithText(a: Int, skipped: String, z: String)
    final case class WithBytes(a: Int, skipped: Array[Byte], z: String)
    final case class WithBool(a: Int, skipped: Boolean, z: String)
    final case class WithDouble(a: Int, skipped: Double, z: String)
    final case class WithList(a: Int, skipped: List[Int], z: String)
    final case class WithOpt(a: Int, skipped: Option[Int], z: String)
    final case class Nested(p: String, q: Vector[Long])
    final case class WithProduct(a: Int, skipped: Nested, z: String)
    implicit val known: Schema[Known] = Schema.derived
    implicit val withInt: Schema[WithInt] = Schema.derived
    implicit val withText: Schema[WithText] = Schema.derived
    implicit val withBytes: Schema[WithBytes] = Schema.derived
    implicit val withBool: Schema[WithBool] = Schema.derived
    implicit val withDouble: Schema[WithDouble] = Schema.derived
    implicit val withList: Schema[WithList] = Schema.derived
    implicit val withOpt: Schema[WithOpt] = Schema.derived
    implicit val nested: Schema[Nested] = Schema.derived
    implicit val withProduct: Schema[WithProduct] = Schema.derived

    def check(name: String, bytes: Array[Byte]): Unit =
      assertEquals(Cbor.read[Known](bytes), Right(Known(1, "end")), s"skipping a $name")

    check("positive integer", Cbor.write(WithInt(1, 7, "end")))
    check("negative integer", Cbor.write(WithInt(1, -7, "end")))
    check("text string", Cbor.write(WithText(1, "a longer string than one byte", "end")))
    check("byte string", Cbor.write(WithBytes(1, Array[Byte](1, 2, 3, 4), "end")))
    check("boolean", Cbor.write(WithBool(1, true, "end")))
    check("double", Cbor.write(WithDouble(1, 1.5, "end")))
    check("array", Cbor.write(WithList(1, List(1, 2, 3), "end")))
    check("null (an absent option)", Cbor.write(WithOpt(1, None, "end")))
    check("nested product", Cbor.write(WithProduct(1, Nested("x", Vector(1, 2)), "end")))
  }

  test("a skipped value nested VERY deep still skips — no cap, no stack overflow") {
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("deep")
    var i = 0
    while (i <= 50000) { out.arrayHeader(1); i += 1 }
    out.text("bottom")
    out.text("a"); out.text("kept")
    assertEquals(Cbor.read[OnlyA](out.toArray), Right(OnlyA("kept")))
  }

  test("a TRUNCATED unknown field is still damage, not a silent skip") {
    val whole = Cbor.write(UfV2("o1", 10, "EUR", Vector("gift")))
    assert(Cbor.read[UfV1](whole.take(whole.length - 3)).isLeft)
  }

  test("what is still refused: a case the sum does not know, and a required field nobody sent") {
    assert(Cbor.read[UfE1](Cbor.write[UfE2](UfE2.B(1))).isLeft)
    assert(Json.decode(implicitly[Schema[UfE1]])(Json.parse(Json.encode(implicitly[Schema[UfE2]])(UfE2.B(1)))).isLeft)
    assert(Cbor.read[UfV2](Cbor.write(UfV1("o1", 10))).isLeft)
  }
}
