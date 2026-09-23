package okay.crypto

import javax.crypto.Mac
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.SecretKeyFactory

/** the JCA leg — every primitive the JDK already ships, so the seam
 * costs zero dependencies. NAMED, because okay-security's wider Crypto
 * delegates its four primitives here (security-crypto-dedup): one
 * implementation per platform, not one per module. */
given platform: Crypto = new Crypto:
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(data)
  def sha256(data: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(data)
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte] =
    // the spec holds its own COPY of the password: cleared after use
    // (okay-security's version did this and this one did not — the
    // better of the two survived the dedup)
    val spec = PBEKeySpec(password, salt, iterations, bits)
    try SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded
    finally spec.clearPassword()
  def randomBytes(n: Int): Array[Byte] =
    val bs = new Array[Byte](n)
    java.security.SecureRandom().nextBytes(bs)
    bs
