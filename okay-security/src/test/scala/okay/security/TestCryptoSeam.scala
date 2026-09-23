package okay.security

/**
 * okay-security's `Crypto` IS an `okay.crypto.Crypto`
 * (security-crypto-dedup): the four primitives are okay-crypto's, not a
 * second copy, so a security given serves wherever the primitive seam
 * is asked (okay-pg's SCRAM) — and both answer the published vectors.
 * Shared, so it runs on the JVM (JCA) and on Node (node:crypto): the
 * JS leg of okay-crypto had no vector test of its own before this, only
 * the live SCRAM battery.
 */
class TestCryptoSeam extends munit.FunSuite:

  private def hex(bs: Array[Byte]): String = bs.map(b => "%02x".format(b & 0xff)).mkString
  private def utf8(s: String): Array[Byte] = s.getBytes("UTF-8")

  /** a caller of the PRIMITIVE seam, the way okay-pg's Scram is one */
  private def scramish(password: String)(using c: okay.crypto.Crypto): String =
    hex(c.hmacSha256(c.pbkdf2(password.toCharArray, utf8("salt"), 1, 256), utf8("Client Key")))

  test("a security Crypto serves where an okay.crypto.Crypto is asked") {
    val security: Crypto = summon[Crypto]
    assertEquals(scramish("password")(using security), scramish("password")(using okay.crypto.platform))
  }

  test("the security given answers the published vectors (sha256, hmac, pbkdf2)") {
    val c = summon[Crypto]
    assertEquals(hex(c.sha256(utf8("abc"))),
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(hex(c.hmacSha256(utf8("key"), utf8("The quick brown fox jumps over the lazy dog"))),
      "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")
    assertEquals(hex(c.pbkdf2("password".toCharArray, utf8("salt"), 1, 256)),
      "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b")
    assertEquals(c.randomBytes(16).length, 16)
  }
