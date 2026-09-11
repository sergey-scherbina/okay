package okay.codec

/**
 * One Schema, one value, one answer on either wire
 * (cbor-unknown-fields).
 *
 * `Json.decode` had always skipped a field it did not declare and
 * `Cbor.get` had always refused one, for the same schema and the same
 * value. Nothing chose that: no test pinned the refusal, no spec
 * stated it, and `JsonStrict` — the door that IS strict — skips
 * unknown fields by design. Operationally the refusal was the worse
 * half, because it made adding a field a breaking change for every
 * reader already deployed.
 */
class TestUnknownFields extends munit.FunSuite:

  final case class V1(id: String, total: Long)
  final case class V2(id: String, total: Long, currency: String, tags: Vector[String])
  given Schema[V1] = Schema.derived
  given Schema[V2] = Schema.derived

  val newer = V2("o1", 10, "EUR", Vector("gift", "rush"))

  test("a field the reader does not declare is SKIPPED on both wires, and the rest decodes") {
    val fromJson = Json.decode(summon[Schema[V1]])(Json.parse(Json.encode(summon[Schema[V2]])(newer)))
    val fromCbor = Cbor.read[V1](Cbor.write(newer))
    assertEquals(fromJson, Right(V1("o1", 10)))
    assertEquals(fromCbor, Right(V1("o1", 10)), "CBOR must answer what JSON answers")
    assertEquals(fromCbor, fromJson)
  }

  test("every shape of skipped value: the reader lands exactly on the next field") {
    // a product whose unknown field is each CBOR major type in turn,
    // with a DECLARED field after it — if the skip mis-counted bytes,
    // the field after it would not decode
    final case class Known(a: Int, z: String)
    given Schema[Known] = Schema.derived

    final case class WithInt(a: Int, skipped: Long, z: String)
    final case class WithNeg(a: Int, skipped: Long, z: String)
    final case class WithText(a: Int, skipped: String, z: String)
    final case class WithBytes(a: Int, skipped: Array[Byte], z: String)
    final case class WithBool(a: Int, skipped: Boolean, z: String)
    final case class WithDouble(a: Int, skipped: Double, z: String)
    final case class WithList(a: Int, skipped: List[Int], z: String)
    final case class WithOpt(a: Int, skipped: Option[Int], z: String)
    final case class Inner(p: String, q: Vector[Long])
    final case class WithProduct(a: Int, skipped: Inner, z: String)
    given Schema[WithInt] = Schema.derived
    given Schema[WithNeg] = Schema.derived
    given Schema[WithText] = Schema.derived
    given Schema[WithBytes] = Schema.derived
    given Schema[WithBool] = Schema.derived
    given Schema[WithDouble] = Schema.derived
    given Schema[WithList] = Schema.derived
    given Schema[WithOpt] = Schema.derived
    given Schema[Inner] = Schema.derived
    given Schema[WithProduct] = Schema.derived

    def check(name: String, bytes: Array[Byte]): Unit =
      assertEquals(Cbor.read[Known](bytes), Right(Known(1, "end")), s"skipping a $name")

    check("positive integer", Cbor.write(WithInt(1, 7, "end")))
    check("negative integer", Cbor.write(WithNeg(1, -7, "end")))
    check("text string", Cbor.write(WithText(1, "a longer string than one byte", "end")))
    check("byte string", Cbor.write(WithBytes(1, Array[Byte](1, 2, 3, 4), "end")))
    check("boolean", Cbor.write(WithBool(1, true, "end")))
    check("double", Cbor.write(WithDouble(1, 1.5, "end")))
    check("array", Cbor.write(WithList(1, List(1, 2, 3), "end")))
    check("null (an absent option)", Cbor.write(WithOpt(1, None, "end")))
    check("nested product", Cbor.write(WithProduct(1, Inner("x", Vector(1, 2)), "end")))
  }

  test("a skipped value nested VERY deep still skips — no cap, no stack overflow (remove-codecs-maxdepth)") {
    // a skip recurses on the depth of the INPUT, which the sender
    // chose — this USED to be bounded (Codecs.maxDepth) because that
    // recursion cost native stack; cbor-skip-threshold-trampoline gave
    // it a Cont.defer trampoline past Codecs.NativeThreshold instead,
    // so the cap could be removed rather than merely raised
    // (remove-codecs-maxdepth) — this is the same document that used
    // to be refused, now decoding correctly
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("deep")
    for _ <- 0 to 50000 do out.arrayHeader(1)
    out.text("bottom")
    out.text("a"); out.text("kept")

    final case class OnlyA(a: String)
    given Schema[OnlyA] = Schema.derived
    assertEquals(Cbor.read[OnlyA](out.toArray), Right(OnlyA("kept")))
  }

  test("a TRUNCATED unknown field is still damage, not a silent skip") {
    val whole = Cbor.write(V2("o1", 10, "EUR", Vector("gift")))
    val cut = whole.take(whole.length - 3)
    assert(Cbor.read[V1](cut).isLeft, "a cut-off value must not read as a successful skip")
  }

  test("what is still refused: a case the sum does not know, and a required field nobody sent") {
    enum E1 derives Schema:
      case A(x: Int)
    enum E2 derives Schema:
      case A(x: Int)
      case B(y: Int)
    // an unknown CASE stays a refusal on both wires — a case is which
    // value this IS, not an extra detail about it
    assert(Cbor.read[E1](Cbor.write[E2](E2.B(1))).isLeft)
    assert(Json.decode(summon[Schema[E1]])(Json.parse(Json.encode(summon[Schema[E2]])(E2.B(1)))).isLeft)
    // and a required field that was never sent is still named
    assert(Cbor.read[V2](Cbor.write(V1("o1", 10))).isLeft)
  }
