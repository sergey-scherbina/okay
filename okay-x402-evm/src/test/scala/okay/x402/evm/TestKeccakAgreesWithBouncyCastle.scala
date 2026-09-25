package okay.x402.evm

import org.bouncycastle.crypto.digests.KeccakDigest

/** keccak-pure: okay-crypto's Keccak-256 is BouncyCastle's, byte for byte,
 * on every length across the 136-byte rate and on random input */
class TestKeccakAgreesWithBouncyCastle extends munit.FunSuite:

  private def bc(b: Array[Byte]): Vector[Byte] =
    val d = KeccakDigest(256)
    d.update(b, 0, b.length)
    val out = new Array[Byte](32)
    val _ = d.doFinal(out, 0)
    out.toVector

  test("the same digest, lengths 0 to 600 and 500 random inputs") {
    val rnd = scala.util.Random(20260925)
    val inputs = (0 to 600).map(n => Array.fill[Byte](n)(rnd.nextInt().toByte)) ++
      (1 to 500).map(_ => Array.fill[Byte](rnd.nextInt(5000))(rnd.nextInt().toByte))
    for b <- inputs do assertEquals(Evm.keccak(b).toVector, bc(b), s"length ${b.length}")
  }
