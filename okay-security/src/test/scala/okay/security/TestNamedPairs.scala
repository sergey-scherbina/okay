package okay.security

import scala.compiletime.testing.typeChecks

/**
 * The two security pairs are named, and a wrong name does not compile
 * (named-pairs-security, 2026-09-12).
 *
 * Why this suite exists: `OAuth2.pkce` and `ApiKey.issue` each return
 * two `String`s whose meaning used to live only in a doc comment.
 * Swap PKCE's pair and the verifier travels while the challenge
 * stays, which defeats the exchange; swap the key pair and the secret
 * is stored in the clear while its digest is handed out. Both
 * compiled, and every existing test still passed, because the types
 * were identical. The names are the fix, and they cost nothing: a
 * named tuple erases to the plain one.
 */
class TestNamedPairs extends munit.FunSuite {

  test("pkce answers a named pair, and the challenge is the S256 of the verifier") {
    val p = OAuth2.pkce()
    val expected = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      summon[Crypto].sha256(p.verifier.getBytes("US-ASCII")))
    assertEquals(p.challenge, expected)
    assertNotEquals(p.verifier, p.challenge)
  }

  test("issue answers a named pair, and the swapped pair does not verify") {
    val issued = ApiKey.issue()
    assert(ApiKey.verify(issued.key, issued.digest))
    assert(!ApiKey.verify(issued.digest, issued.key),
      "the swapped pair must not verify — this is the defect the names prevent")
  }

  test("a WRONG name does not compile, which is the guard that did not exist") {
    // paired with the right name, so the refusal is the NAME and not
    // something else about the snippet
    assert(typeChecks("""import okay.security.*
                         val x: String = OAuth2.pkce().verifier"""))
    assert(!typeChecks("""import okay.security.*
                          val x: String = OAuth2.pkce().verifer"""))
    assert(typeChecks("""import okay.security.*
                         val x: String = ApiKey.issue().digest"""))
    assert(!typeChecks("""import okay.security.*
                          val x: String = ApiKey.issue().secret"""))
  }

  test("a named destructure binds by NAME, whatever order it is written in") {
    val (challenge = c, verifier = v) = OAuth2.pkce()
    val expected = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      summon[Crypto].sha256(v.getBytes("US-ASCII")))
    assertEquals(c, expected, "written challenge-first, still bound by name")
  }
}
