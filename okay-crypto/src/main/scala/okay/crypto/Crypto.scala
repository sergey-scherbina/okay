package okay.crypto

/**
 * The primitive crypto seam (security-crypto-split): the four
 * operations SCRAM and password hashing need — a keyed MAC, a hash,
 * a KDF, and randomness — as a per-platform given, and NOTHING that
 * drags a dependency. This module rests on the platform's own crypto
 * (JCA on the JVM, node:crypto on JS) and on nothing else — no
 * okayHttp, no JWKS road — so a module that must not cycle through
 * the security stack (okay-pg's SCRAM) can still stand on a SHARED
 * seam instead of a private copy.
 *
 * It is deliberately the SMALL surface: the fuller crypto (RSA/ECDSA
 * signing, JWT key handles) stays in okay-security, which owns those
 * concerns and its heavier dependencies. A caller that needs only
 * the primitives depends here; a caller that needs signing depends
 * there. Platform primitives, never our own (the specs/tls.md rule).
 */
trait Crypto:
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte]
  def sha256(data: Array[Byte]): Array[Byte]
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte]
  def randomBytes(n: Int): Array[Byte]
