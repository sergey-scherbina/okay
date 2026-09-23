package okay.security

/**
 * The Crypto seam over node:crypto — the security-node stage. What
 * Node has natively is real here: HMAC-SHA256, SHA-256, PBKDF2,
 * secure random. RSA STAYS JVM, and the reason is the seam's own
 * honesty: the trait speaks `java.security` keys, which exist on JS
 * only as signatures — there is no key material to hand to Node. A
 * JWK-native key type is the follow-up IF JS ever needs RS256; the
 * service-to-service case (HS256, passwords, API keys, PKCE) is what
 * this stage serves, and serves fully.
 */
given Crypto = new Crypto:

  // the four primitives are okay-crypto's node:crypto leg, not a
  // second binding of the same module (security-crypto-dedup)
  private val primitives = okay.crypto.platform
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] = primitives.hmacSha256(key, data)
  def sha256(data: Array[Byte]): Array[Byte] = primitives.sha256(data)
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte] =
    primitives.pbkdf2(password, salt, iterations, bits)
  def randomBytes(n: Int): Array[Byte] = primitives.randomBytes(n)

  def signRsaSha256(key: Crypto.Handle, data: Array[Byte]): Array[Byte] =
    // a broken invariant, not hostile input: no RSA key can even be
    // CONSTRUCTED on this platform (rsaPublicKey answers None), so a
    // handle reaching here was smuggled
    throw UnsupportedOperationException("RSA signing is a JVM ability (security-node)")

  def verifyRsaSha256(key: Crypto.Handle, data: Array[Byte],
                      sig: Array[Byte]): Boolean =
    false   // a refusal: no verifiable key exists on this platform

  def rsaPublicKey(modulus: BigInt, exponent: BigInt): Option[Crypto.Handle] =
    None   // RS256 is JVM until a JWK-native verify arrives

  def signEcdsaSha256(key: Crypto.Handle, data: Array[Byte]): Array[Byte] =
    throw UnsupportedOperationException("ECDSA signing is a JVM ability (same door as RSA)")

  def verifyEcdsaSha256(key: Crypto.Handle, data: Array[Byte],
                        derSig: Array[Byte]): Boolean =
    false   // no constructible EC key on this platform, so no true answer

  def ecPublicKey(x: BigInt, y: BigInt): Option[Crypto.Handle] =
    None   // ES256 is JVM by the same reasoning as RSA — and the same follow-up
