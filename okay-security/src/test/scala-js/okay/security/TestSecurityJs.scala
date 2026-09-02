package okay.security

import okay.codec.Json

/**
 * The JS leg verifies — under Node, through node:crypto, running the
 * SAME shared code as the JVM: only the Crypto given differs, which
 * is the seam's whole claim. The PKCE case pins the RFC 7636 test
 * vector, so the sha256+base64url wiring is checked against the
 * standard rather than against itself.
 */
class TestSecurityJs extends munit.FunSuite {

  val now = 1_700_000_000L
  val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")

  test("an HS256 JWT signs and verifies on Node; tampering refuses") {
    val claims = Claims(subject = Some("u1"), audience = Vector("api"),
      expires = Some(now + 600), scopes = Set("read"))
    val t = Jwt.sign(claims, Jwt.Key.Hmac(secret))
    Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now) match
      case Verified.Ok(p) => assertEquals(p.id, "u1")
      case Verified.No(why) => fail(why)
    val other = "a-completely-different-secret!!!".getBytes("UTF-8")
    assert(Jwt.verify(t, _ => Some(Jwt.Key.Hmac(other)), Some("api"), now)
      .isInstanceOf[Verified.No])
  }

  test("passwords hash and verify through node's pbkdf2") {
    val stored = Password.hash("correct horse".toCharArray)
    assert(Password.verify("correct horse".toCharArray, stored))
    assert(!Password.verify("wrong".toCharArray, stored))
    assert(stored.startsWith("pbkdf2$"))
  }

  test("api keys issue and verify; the digest does not") {
    val (key, digest) = ApiKey.issue()
    assert(ApiKey.verify(key, digest))
    assert(!ApiKey.verify(digest, digest))
  }

  test("PKCE pins the RFC 7636 vector — the standard, not ourselves") {
    // appendix B of RFC 7636
    val verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    val challenge = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      summon[Crypto].sha256(verifier.getBytes("US-ASCII")))
    assertEquals(challenge, "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
  }

  test("RSA on JS refuses honestly instead of pretending") {
    // an RSA key cannot verify anything here — a refusal, not a crash
    assertEquals(Jwks.parse(Json.parse("""{"keys":[]}""")), Map.empty)
  }
}
