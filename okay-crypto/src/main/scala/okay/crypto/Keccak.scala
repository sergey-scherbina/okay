package okay.crypto

/**
 * WHICH KECCAK-256 (compress-crypto-facades): the same shape as
 * okay-arrow's `ArrowCodec`. Ours is the default given — `Keccak256`, pure
 * Scala on the JVM and JS, no dependency. A library's stands behind an
 * import with an OPTIONAL dependency: on the JVM
 * `okay.crypto.BouncyCastleKeccak.given` (org.bouncycastle:bcprov, the
 * implementation `Keccak256` is checked against), refused by name when
 * the jar is not on the classpath.
 *
 * Keccak is the ONE primitive of ours to choose against: SHA-256, HMAC,
 * PBKDF2 and randomness are the platform's (`Crypto`), Argon2 is
 * BouncyCastle's, TLS is the JDK's — by specs/tls.md's rule, never our
 * own, so there is nothing else to switch.
 */
trait Keccak:
  def name: String
  /** the ORIGINAL Keccak-256 (Ethereum's), not SHA3-256: 32 bytes */
  def hash256(data: Array[Byte]): Array[Byte]

object Keccak:
  /** THE DEFAULT: ours, on every platform */
  given okay: Keccak = Okay

  object Okay extends Keccak:
    def name = "okay"
    def hash256(data: Array[Byte]): Array[Byte] = Keccak256.hash(data)
