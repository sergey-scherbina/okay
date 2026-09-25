package okay2.codec

/** An unbounded integer as a Schema primitive (okay-codec's TestBigInt,
 * JSON half): JSON carries it as a string of digits because `JNum` is a
 * Double, and accepts a number only while a double still holds it
 * exactly. */
class TestBigInt extends munit.FunSuite {

  private val two64 = BigInt(1) << 64

  private val boundary: List[BigInt] = List(
    BigInt(0), BigInt(1), BigInt(-1),
    BigInt(Long.MaxValue), BigInt(Long.MinValue), BigInt(Long.MaxValue) + 1, BigInt(Long.MinValue) - 1,
    two64 - 1, two64, -two64, -two64 - 1,
    (BigInt(1) << 300) + 12345, -(BigInt(1) << 300))

  test("round-trips lossless and strict JSON at every boundary") {
    boundary.foreach { v =>
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
    val past = both("18446744073709551615")
    assert(past._1.isLeft && past._2.isLeft, s"$past")
    assert(past._1.left.exists(_.contains("string of digits")), s"$past")
    val frac = both("1.5")
    assert(frac._1.isLeft && frac._2.isLeft, s"$frac")
    val junk = both("\"12x\"")
    assert(junk._1.isLeft && junk._2.isLeft, s"$junk")
  }

  test("inside a derived product: a uint64 quantity round-trips") {
    final case class Asset(policy: Array[Byte], name: String, quantity: BigInt)
    implicit val asset: Schema[Asset] = Schema.derived
    val a = Asset(Array[Byte](1, 2, 3), "tok", two64 - 1)
    assertEquals(Json.read[Asset](Json.write(a)).map(_.quantity), Right(two64 - 1))
    assertEquals(Json.readStrict[Asset](Json.write(a)).map(_.quantity), Right(two64 - 1))
  }
}
