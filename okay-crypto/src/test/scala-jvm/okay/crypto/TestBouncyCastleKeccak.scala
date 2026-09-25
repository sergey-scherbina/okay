package okay.crypto

/** compress-crypto-facades: BouncyCastle's Keccak-256 beside ours, byte
 * for byte */
class TestBouncyCastleKeccak extends munit.FunSuite:

  test("with no import the given is ours; the import picks BouncyCastle") {
    assertEquals(summon[Keccak].name, "okay")
    locally {
      import BouncyCastleKeccak.given
      assertEquals(summon[Keccak].name, "bouncycastle")
    }
  }

  test("the two agree on every length across the rate, and on random inputs") {
    val rnd = scala.util.Random(7)
    for n <- 0 to 300 do
      val b = Array.fill(n)(n.toByte)
      assertEquals(BouncyCastleKeccak.hash256(b).toVector, Keccak.Okay.hash256(b).toVector, s"length $n")
    for _ <- 1 to 200 do
      val b = Array.fill(rnd.nextInt(2000))(rnd.nextInt(256).toByte)
      assertEquals(BouncyCastleKeccak.hash256(b).toVector, Keccak.Okay.hash256(b).toVector)
  }

  test("the canonical vector, through the library") {
    assertEquals(BouncyCastleKeccak.hash256(Array.emptyByteArray).map(b => f"${b & 0xff}%02x").mkString,
      "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
  }

  test("by name, and the refusal names the jar to add") {
    assertEquals(Keccaks.byName("okay").map(_.name), Right("okay"))
    assertEquals(Keccaks.byName("bouncycastle").map(_.name), Right("bouncycastle"))
    assertEquals(Keccaks.byName("sha3"), Left("unknown Keccak implementation 'sha3' (okay, bouncycastle)"))
    assertEquals(BouncyCastleKeccak.missing(), None)
    val why = BouncyCastleKeccak.missing("org.bouncycastle.NoSuchClass").getOrElse(fail("a missing class went unnoticed"))
    assert(why.contains("bcprov-jdk18on:1.78.1") && why.contains("Keccak.Okay"), why)
  }
