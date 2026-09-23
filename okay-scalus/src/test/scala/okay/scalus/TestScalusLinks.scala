package okay.scalus

/** stage 0: scalus 1.2.0 (built on Scala 3.3.8) links and runs under
 * this build's Scala 3.9 — the ledger model constructs, and its
 * blake2b is reachable */
class TestScalusLinks extends munit.FunSuite:
  test("scalus's ledger model and hashing are on the classpath and run") {
    val bs = scalus.uplc.builtin.ByteString.fromArray(Array[Byte](1, 2, 3))
    val h = scalus.uplc.builtin.platform.blake2b_256(bs)
    assertEquals(h.bytes.length, 32)
  }
