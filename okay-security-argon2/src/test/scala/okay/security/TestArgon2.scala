package okay.security

/**
 * The satellite's claims: the PHC form round-trips and migrates, the
 * RFC 9106 vector pins the PROVIDER against the standard rather than
 * against ourselves, and the hostile stored form — the row an
 * attacker writes — refuses without allocating what it asks for.
 */
class TestArgon2 extends munit.FunSuite {

  // small parameters: the tests prove wiring, not memory-hardness
  def quick(pw: String): String = Argon2.hash(pw.toCharArray, memoryKb = 64, iterations = 1)

  test("hash-then-verify round-trips; a wrong password refuses; salts differ") {
    val stored = quick("correct horse")
    assert(Argon2.verify("correct horse".toCharArray, stored))
    assert(!Argon2.verify("wrong".toCharArray, stored))
    assertNotEquals(quick("correct horse"), quick("correct horse"))
  }

  test("the stored form is PHC, and raised parameters ride it with no flag day") {
    val stored = quick("pw")
    assert(stored.startsWith("$argon2id$v=19$m=64,t=1,p=1$"), stored)
    val raised = Argon2.hash("pw".toCharArray, memoryKb = 128, iterations = 2)
    assert(raised.startsWith("$argon2id$v=19$m=128,t=2,p=1$"))
    // both verify through the same door — the form carries the answer
    assert(Argon2.verify("pw".toCharArray, stored))
    assert(Argon2.verify("pw".toCharArray, raised))
  }

  test("the RFC 9106 Argon2id vector — the standard's bytes, not our own") {
    import org.bouncycastle.crypto.generators.Argon2BytesGenerator
    import org.bouncycastle.crypto.params.Argon2Parameters
    val params = Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
      .withVersion(Argon2Parameters.ARGON2_VERSION_13)
      .withMemoryAsKB(32).withIterations(3).withParallelism(4)
      .withSalt(Array.fill[Byte](16)(2))
      .withSecret(Array.fill[Byte](8)(3))
      .withAdditional(Array.fill[Byte](12)(4))
      .build()
    val gen = Argon2BytesGenerator(); gen.init(params)
    val out = new Array[Byte](32)
    gen.generateBytes(Array.fill[Byte](32)(1), out)
    val expect = "0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659"
    assertEquals(out.map("%02x".format(_)).mkString, expect)
  }

  test("hostile stored forms refuse, never throw — absurd memory before allocation") {
    val bad = Seq(
      "", "garbage", "$argon2id$", "pbkdf2$210000$x$y",
      "$argon2i$v=19$m=64,t=1,p=1$AAAA$AAAA",        // wrong variant
      "$argon2id$v=19$m=64,t=1$AAAA$AAAA",            // missing p
      "$argon2id$v=19$m=lots,t=1,p=1$AAAA$AAAA",      // non-numeric
      "$argon2id$v=19$m=64,t=1,p=1$!!$AAAA",          // salt not base64
      "$argon2id$v=19$m=2147483647,t=1,p=1$AAAA$AAAA", // 2 TiB of memory, refused unallocated
      "$argon2id$v=19$m=64,t=999,p=1$AAAA$AAAA",      // absurd time
      "$argon2id$v=19$m=64,t=1,p=64$AAAA$AAAA",       // absurd lanes
    )
    for s <- bad do assert(!Argon2.verify("pw".toCharArray, s), s)
  }

  test("verifyAny reads a mixed store through one door") {
    val old = Password.hash("legacy pw".toCharArray)
    val neu = quick("modern pw")
    assert(Argon2.verifyAny("legacy pw".toCharArray, old))
    assert(Argon2.verifyAny("modern pw".toCharArray, neu))
    assert(!Argon2.verifyAny("modern pw".toCharArray, old))
    assert(!Argon2.verifyAny("legacy pw".toCharArray, neu))
  }

  test("out-of-range hash parameters are a broken invariant, so they throw") {
    intercept[IllegalArgumentException](Argon2.hash("pw".toCharArray, memoryKb = 1 << 30))
  }
}
