package okay.codec

/**
 * schema-bigint (specs/codecs.md, "Big integers"): an unbounded integer
 * as a Schema primitive. CBOR writes RFC 8949's preferred serialization
 * — a plain integer across the whole 64-bit unsigned range, a tag 2/3
 * bignum past it — which is how Plutus Data and the Cardano ledger
 * write integers; JSON carries it as a string of digits, because
 * `JNum` is a Double.
 */
class TestBigInt extends munit.FunSuite:

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xFF}%02x").mkString
  private def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  // ---- the defect found on the way: SLong read a uint64 as a negative

  test("SLong refuses a CBOR uint64 past Long.MaxValue instead of wrapping it negative") {
    // RFC 8949 Appendix A: 18446744073709551615 = 1bffffffffffffffff
    val r = Cbor.read[Long](unhex("1bffffffffffffffff"))
    assert(r.isLeft, s"decoded as $r")
  }

  test("SLong refuses a CBOR negative past Long.MinValue") {
    // -18446744073709551616 = 3bffffffffffffffff
    assert(Cbor.read[Long](unhex("3bffffffffffffffff")).isLeft)
  }

  test("SLong still reads its own extremes") {
    assertEquals(Cbor.read[Long](Cbor.write(Long.MaxValue)), Right(Long.MaxValue))
    assertEquals(Cbor.read[Long](Cbor.write(Long.MinValue)), Right(Long.MinValue))
  }

  // ---- SBigInt: the bytes a node would write, RFC 8949 Appendix A

  private val two64 = BigInt(1) << 64

  test("CBOR writes the preferred serialization: plain integer to 2^64-1, bignum past it") {
    val vectors = List(
      BigInt(0) -> "00",
      BigInt(-1) -> "20",
      BigInt(1000000) -> "1a000f4240",
      (two64 - 1) -> "1bffffffffffffffff",           // uint64 max: still a plain integer
      two64 -> "c249010000000000000000",              // tag 2
      -two64 -> "3bffffffffffffffff",                 // -2^64: still a plain negative
      (-two64 - 1) -> "c349010000000000000000")       // tag 3
    vectors.foreach { (v, h) =>
      assertEquals(hex(Cbor.write(v)), h, s"writing $v")
      assertEquals(Cbor.read[BigInt](unhex(h)), Right(v), s"reading $h")
    }
  }

  test("CBOR reads a bignum a conforming encoder sent for a SMALL value (not preferred, still valid)") {
    // tag 2 over h'01' is 1; tag 3 over h'00' is -1
    assertEquals(Cbor.read[BigInt](unhex("c24101")), Right(BigInt(1)))
    assertEquals(Cbor.read[BigInt](unhex("c34100")), Right(BigInt(-1)))
  }

  test("CBOR refuses a tag that is not a bignum") {
    assert(Cbor.read[BigInt](unhex("c11a514b67b0")).isLeft)   // tag 1, epoch time
  }

  private val boundary: List[BigInt] = List(
    BigInt(0), BigInt(1), BigInt(-1),
    BigInt(Long.MaxValue), BigInt(Long.MinValue), BigInt(Long.MaxValue) + 1, BigInt(Long.MinValue) - 1,
    two64 - 1, two64, -two64, -two64 - 1,
    (BigInt(1) << 300) + 12345, -(BigInt(1) << 300))

  test("round-trips CBOR, lossless JSON and strict JSON at every boundary") {
    boundary.foreach { v =>
      assertEquals(Cbor.read[BigInt](Cbor.write(v)), Right(v), s"cbor $v")
      assertEquals(Json.read[BigInt](Json.write(v)), Right(v), s"json $v")
      assertEquals(JsonStrict.read[BigInt](Json.write(v)), Right(v), s"strict $v")
    }
  }

  test("JSON carries it as a string of digits") {
    assertEquals(Json.write(two64 - 1), "\"18446744073709551615\"")
  }

  test("both JSON doors accept the SAME set: digits, or an exact number; refuse the rest") {
    def both(in: String) = (Json.read[BigInt](in), JsonStrict.read[BigInt](in))
    assertEquals(both("42"), (Right(BigInt(42)), Right(BigInt(42))))
    assertEquals(both("-9007199254740992"), (Right(BigInt(-9007199254740992L)), Right(BigInt(-9007199254740992L))))
    // a number past 2^53 has already been rounded by the time it is a Double
    val past = both("18446744073709551615")
    assert(past._1.isLeft && past._2.isLeft, s"$past")
    assert(past._1.left.exists(_.contains("string of digits")), s"$past")
    val frac = both("1.5")
    assert(frac._1.isLeft && frac._2.isLeft, s"$frac")
    val junk = both("\"12x\"")
    assert(junk._1.isLeft && junk._2.isLeft, s"$junk")
  }

  final case class Asset(policy: Array[Byte], name: String, quantity: BigInt)
  given Schema[Asset] = Schema.derived

  test("inside a derived product: a uint64 quantity round-trips both wires, Validate agrees") {
    val a = Asset(Array[Byte](1, 2, 3), "tok", two64 - 1)
    val c = Cbor.read[Asset](Cbor.write(a)).map(_.quantity)
    val j = Json.read[Asset](Json.write(a)).map(_.quantity)
    assertEquals(c, Right(two64 - 1))
    assertEquals(j, Right(two64 - 1))
    assert(Validate.decode(summon[Schema[Asset]])(Json.parse(Json.write(a))).isRight)
    val bad = Validate.decode(summon[Schema[Asset]])(Json.parse("""{"policy":"AQID","name":"tok","quantity":"1e3"}"""))
    assert(bad.isLeft, s"$bad")
  }

  test("JSON Schema says what the wire carries") {
    assertEquals(JsonSchema.of(Schema.SBigInt).toString,
      Json.JObj(Vector("type" -> Json.JStr("string"), "pattern" -> Json.JStr("^-?[0-9]+$"))).toString)
  }

  final case class V1(q: Long)
  final case class V2(q: BigInt)
  given Schema[V1] = Schema.derived
  given Schema[V2] = Schema.derived

  test("a digest carries it across a wire, and Long -> BigInt is a type change (the JSON wire changes)") {
    assert(Digest.compare(summon[Schema[V2]], Digest.of(summon[Schema[V2]])).changes.isEmpty)
    val r = Compat.compare(summon[Schema[V1]], summon[Schema[V2]])
    assert(r.render.contains("Long") && r.render.contains("BigInt"), r.render)
  }
