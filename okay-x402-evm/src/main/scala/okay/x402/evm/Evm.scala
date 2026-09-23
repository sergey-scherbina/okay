package okay.x402.evm

import java.math.BigInteger
import org.bouncycastle.crypto.digests.KeccakDigest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECAlgorithms

/**
 * The three EVM primitives x402's `exact` scheme needs to verify a
 * payment offline: keccak-256, secp256k1 public-key RECOVERY from a
 * signature, and an Ethereum address from a key.
 */
object Evm:
  def hex(b: Array[Byte]): String = b.map(x => f"${x & 0xFF}%02x").mkString
  def unhex(s: String): Array[Byte] =
    val h = s.stripPrefix("0x")
    require(h.length % 2 == 0 && h.forall(c => Character.digit(c, 16) >= 0), s"not hex: $s")
    h.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  /** keccak-256 — Ethereum's hash, NOT the standardised SHA3-256 (the
   * padding differs) */
  def keccak(b: Array[Byte]): Array[Byte] =
    val d = KeccakDigest(256)
    d.update(b, 0, b.length)
    val out = new Array[Byte](32)
    d.doFinal(out, 0): Unit
    out

  private val curve = CustomNamedCurves.getByName("secp256k1")
  private val n = curve.getN
  private val halfN = n.shiftRight(1)

  /** the address of an uncompressed public key: the last 20 bytes of
   * keccak over its 64 coordinate bytes */
  def address(uncompressed: Array[Byte]): String =
    "0x" + hex(keccak(uncompressed.drop(1)).takeRight(20))

  /** the address of a private key */
  def addressOf(privateKey: BigInteger): String =
    address(curve.getG.multiply(privateKey).normalize().getEncoded(false))

  /**
   * The signer's address, recovered from a 65-byte `r ‖ s ‖ v` signature
   * over `digest`. Refused, not guessed: `v` must be 27 or 28, and `s`
   * must be in the LOWER half of the order — USDC's FiatToken (the
   * EIP-3009 contract x402 settles through) rejects a high-s signature,
   * so accepting one here would verify a payment that cannot settle.
   */
  def recover(digest: Array[Byte], signature: Array[Byte]): Either[String, String] =
    if signature.length != 65 then Left(s"a signature is 65 bytes, got ${signature.length}")
    else
      val r = BigInteger(1, signature.slice(0, 32))
      val s = BigInteger(1, signature.slice(32, 64))
      val v = signature(64) & 0xFF
      if v != 27 && v != 28 then Left(s"v must be 27 or 28, got $v")
      else if r.signum == 0 || r.compareTo(n) >= 0 || s.signum == 0 || s.compareTo(n) >= 0 then Left("r or s out of range")
      else if s.compareTo(halfN) > 0 then Left("high-s signature (not accepted by the token contract)")
      else
        // R has x = r (the r + n case exceeds the field for secp256k1 in practice) and the parity v - 27
        val encoded = Array[Byte](if v == 28 then 0x03 else 0x02) ++ word32(r)
        scala.util.Try(curve.getCurve.decodePoint(encoded)).toOption match
          case None => Left("r is not the x of a curve point")
          case Some(bigR) =>
            val e = BigInteger(1, digest)
            val rInv = r.modInverse(n)
            val q = ECAlgorithms.sumOfTwoMultiplies(curve.getG, e.negate().mod(n).multiply(rInv).mod(n),
              bigR, s.multiply(rInv).mod(n)).normalize()
            if q.isInfinity then Left("the recovered key is the point at infinity")
            else Right(address(q.getEncoded(false)))

  /** a big-endian unsigned 32-byte word */
  def word32(x: BigInteger): Array[Byte] =
    val b = x.toByteArray.dropWhile(_ == 0)
    require(b.length <= 32, s"$x does not fit 32 bytes")
    new Array[Byte](32 - b.length) ++ b

  /** sign `digest` (tests and tools): a deterministic RFC 6979 low-s
   * signature, `r ‖ s ‖ v` */
  def sign(digest: Array[Byte], privateKey: BigInteger): Array[Byte] =
    import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
    import org.bouncycastle.crypto.digests.SHA256Digest
    import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters}
    val params = ECDomainParameters(curve.getCurve, curve.getG, n, curve.getH)
    val signer = ECDSASigner(HMacDSAKCalculator(SHA256Digest()))
    signer.init(true, ECPrivateKeyParameters(privateKey, params))
    val rs = signer.generateSignature(digest)
    val (r, s0) = (rs(0), rs(1))
    val s = if s0.compareTo(halfN) > 0 then n.subtract(s0) else s0
    val me = addressOf(privateKey)
    val body = word32(r) ++ word32(s)
    Seq(27, 28).map(v => body :+ v.toByte).find(sig => recover(digest, sig) == Right(me))
      .getOrElse(throw IllegalStateException("no recovery id reproduces the signer"))
