package okay.security

/** specs/security.md, security-sessions — SessionIssuer. */
class TestSessionIssuer extends munit.FunSuite {

  test("a token it issues is a token it verifies, carrying subject and scopes") {
    val issuer = SessionIssuer()
    val t = issuer.issue("alice@x", scopes = Set("admin"))
    issuer.verify(t) match
      case Verified.Ok(p) =>
        assertEquals(p.id, "alice@x")
        assert(p.claims.scopes("admin"))
      case Verified.No(reason) => fail(s"expected Ok, got No($reason)")
  }

  test("no scopes by default — a plain login session") {
    val issuer = SessionIssuer()
    val t = issuer.issue("bob@x")
    issuer.verify(t) match
      case Verified.Ok(p) => assertEquals(p.claims.scopes, Set.empty[String])
      case Verified.No(reason) => fail(s"expected Ok, got No($reason)")
  }

  test("an expired token refuses") {
    // Jwt.verify's default clock skew is 60s — advance well past
    // both the TTL and the skew tolerance
    val issuer = SessionIssuer(ttlSec = 60)
    val now = System.currentTimeMillis()
    val t = issuer.issue("alice@x", now = now)
    assert(issuer.verify(t, now = now + 200_000).isInstanceOf[Verified.No])
  }

  test("independent instances hold independent keys — one instance's token refuses on another") {
    val a = SessionIssuer()
    val b = SessionIssuer()
    val t = a.issue("alice@x")
    assert(b.verify(t).isInstanceOf[Verified.No])
  }
}
