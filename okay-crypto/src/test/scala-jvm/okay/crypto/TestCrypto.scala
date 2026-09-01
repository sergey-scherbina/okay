package okay.crypto

import munit.FunSuite

/**
 * The primitive seam against published vectors (security-crypto-
 * split): the JVM given must agree with the standards, because a
 * SCRAM handshake that disagrees with the server by one byte fails
 * silently as "bad password". node:crypto is exercised by okay-pg's
 * live TestPgNode, which speaks SCRAM to a real Postgres over the JS
 * given.
 */
class TestCrypto extends FunSuite:

  private val c = summon[Crypto]

  private def hex(bs: Array[Byte]): String =
    bs.map(b => "%02x".format(b & 0xff)).mkString
  private def utf8(s: String): Array[Byte] = s.getBytes("UTF-8")

  test("sha256 matches the NIST vector for \"abc\"") {
    assertEquals(hex(c.sha256(utf8("abc"))),
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  }

  test("hmacSha256 matches the RFC-style fox vector") {
    val mac = c.hmacSha256(utf8("key"), utf8("The quick brown fox jumps over the lazy dog"))
    assertEquals(hex(mac),
      "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")
  }

  test("pbkdf2 (HMAC-SHA256) matches the known password/salt/1 vector") {
    val dk = c.pbkdf2("password".toCharArray, utf8("salt"), iterations = 1, bits = 256)
    assertEquals(hex(dk),
      "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b")
  }

  test("randomBytes yields the asked length and does not repeat") {
    assertEquals(c.randomBytes(16).length, 16)
    assert(!java.util.Arrays.equals(c.randomBytes(16), c.randomBytes(16)))
  }
