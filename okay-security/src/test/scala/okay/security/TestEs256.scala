package okay.security

/**
 * The dance alone — no keys, no crypto, only bytes, which is why this
 * file is SHARED and runs on every platform. The signature segment of
 * a JWT is attacker-supplied, so the hostile half of this battery is
 * the half that matters.
 */
class TestEs256 extends munit.FunSuite {

  private def raw(r: Array[Byte], s: Array[Byte]): Array[Byte] =
    (Array.fill[Byte](32 - r.length)(0) ++ r) ++ (Array.fill[Byte](32 - s.length)(0) ++ s)

  test("the small vector: r=1, s=2 is the eight-byte DER everyone can check by hand") {
    val jose = raw(Array[Byte](1), Array[Byte](2))
    val der = Es256.joseToDer(jose).get
    assertEquals(der.toSeq, Seq[Byte](0x30, 6, 0x02, 1, 1, 0x02, 1, 2))
    assertEquals(Es256.derToJose(der).get.toSeq, jose.toSeq)
  }

  test("a high-bit coordinate gains the 0x00 pad in DER and sheds it coming back") {
    val r = Array.fill[Byte](32)(0x7f); r(0) = 0x80.toByte
    val jose = raw(r, Array[Byte](5))
    val der = Es256.joseToDer(jose).get
    // r's INTEGER body is 33 bytes: the pad plus the 32-byte magnitude
    assertEquals(der(3).toInt, 33)
    assertEquals(der(4).toInt, 0)
    assertEquals(Es256.derToJose(der).get.toSeq, jose.toSeq)
  }

  test("leading zeros strip going out and pad back coming in; even r=0 round-trips") {
    for r <- Seq(Array[Byte](0), Array[Byte](1, 2, 3), Array.fill[Byte](31)(9)) do
      val jose = raw(r, Array[Byte](1))
      val der = Es256.joseToDer(jose).get
      assertEquals(Es256.derToJose(der).get.toSeq, jose.toSeq, s"r of ${r.length}")
  }

  test("every valid raw round-trips (a deterministic sweep)") {
    val rnd = new scala.util.Random(42)
    for _ <- 1 to 200 do
      val jose = new Array[Byte](64); rnd.nextBytes(jose)
      val der = Es256.joseToDer(jose).get
      assertEquals(Es256.derToJose(der).get.toSeq, jose.toSeq)
  }

  test("hostile raw refuses: lengths 0, 63, 65") {
    for n <- Seq(0, 63, 65) do
      assertEquals(Es256.joseToDer(new Array[Byte](n)), None, s"length $n")
  }

  test("hostile DER refuses: wrong tags, truncation, oversize, trailing garbage") {
    val good = Es256.joseToDer(raw(Array[Byte](1), Array[Byte](2))).get
    assertEquals(Es256.derToJose(Array.empty[Byte]), None)
    assertEquals(Es256.derToJose(good.updated(0, 0x31.toByte)), None, "wrong outer tag")
    assertEquals(Es256.derToJose(good.updated(2, 0x03.toByte)), None, "wrong inner tag")
    assertEquals(Es256.derToJose(good.dropRight(1)), None, "truncated")
    assertEquals(Es256.derToJose(good ++ Array[Byte](0)), None, "trailing garbage")
    // an integer that claims 34 bytes cannot be a P-256 coordinate
    val big = Array[Byte](0x30, 39, 0x02, 34) ++ new Array[Byte](34) ++ Array[Byte](0x02, 1, 1)
    assertEquals(Es256.derToJose(big), None, "oversize integer")
    // a non-minimal pad (0x00 before a low-bit byte) is not DER
    // the frame is honest (38 bytes) so the refusal is the INTEGER's
    val padded = Array[Byte](0x30, 38, 0x02, 33, 0) ++ Array.fill[Byte](32)(1) ++ Array[Byte](0x02, 1, 1)
    assertEquals(Es256.derToJose(padded), None, "non-minimal integer")
  }
}
