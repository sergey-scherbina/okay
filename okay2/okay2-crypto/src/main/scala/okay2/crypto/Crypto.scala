package okay2.crypto

/**
 * The primitive crypto seam (okay-crypto's Crypto.scala,
 * security-crypto-split): the four operations SCRAM needs — a keyed
 * MAC, a hash, a KDF, and randomness — as a per-platform implicit, and
 * NOTHING that drags a dependency: JCA on the JVM, node:crypto on JS.
 * Platform primitives, never our own (the specs/tls.md rule). Keccak is
 * not ported; nothing on okay2 asks for it yet.
 */
trait Crypto {
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte]
  def sha256(data: Array[Byte]): Array[Byte]
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte]
  def randomBytes(n: Int): Array[Byte]
}

/** the platform's, found as the companion's implicit: `Crypto.platform`
 * is the JVM's JCA or Node's node:crypto, whichever this build links */
object Crypto {
  implicit def platform: Crypto = PlatformCrypto.platform
}
