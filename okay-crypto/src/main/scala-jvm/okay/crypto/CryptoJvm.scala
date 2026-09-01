package okay.crypto

import javax.crypto.Mac
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.SecretKeyFactory

/** the JCA leg — every primitive the JDK already ships, so the seam
 * costs zero dependencies (the same primitives okay-security's Crypto
 * uses; here without the signing surface that drags okayHttp) */
given Crypto = new Crypto:
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(data)
  def sha256(data: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(data)
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte] =
    SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
      .generateSecret(PBEKeySpec(password, salt, iterations, bits)).getEncoded
  def randomBytes(n: Int): Array[Byte] =
    val bs = new Array[Byte](n)
    java.security.SecureRandom().nextBytes(bs)
    bs
