package okay.demo

import okay.security.given

/**
 * Confirm-and-sign (demo-sessions): a code, spent once, becomes a
 * signed session; the session verifies to the confirmed email and
 * nothing else, on the clock it was issued for.
 */
class TestLogin extends munit.FunSuite:

  test("start + confirm mints a session that verifies to the email") {
    val code = Login.start("ann@example.com")
    assert(code.matches("\\d{6}"), code)
    assert(Login.confirm("ann@example.com", code))
    val token = Login.issue("ann@example.com")
    assertEquals(Login.verify(token), Some("ann@example.com"))
  }

  test("a code is spent ONCE: the second confirm with the same code fails") {
    val code = Login.start("bob@example.com")
    assert(Login.confirm("bob@example.com", code))
    assert(!Login.confirm("bob@example.com", code))
  }

  test("the wrong code, the wrong email, and an expired code are all refused") {
    val code = Login.start("cara@example.com")
    assert(!Login.confirm("cara@example.com", "000000"))
    assert(!Login.confirm("other@example.com", code))
    assert(Login.confirm("cara@example.com", code))   // the right one still works after wrong tries
    val now = System.currentTimeMillis()
    val late = Login.start("dara@example.com", now)
    assert(!Login.confirm("dara@example.com", late, now + 11L * 60 * 1000))
  }

  test("a stranger's token, a tampered token, and an expired session are refused — never thrown") {
    val now = System.currentTimeMillis()
    val token = Login.issue("eve@example.com", now)
    assertEquals(Login.verify(token, now + 23L * 3600 * 1000), Some("eve@example.com"))
    assertEquals(Login.verify(token, now + 25L * 3600 * 1000), None)
    // flip a MIDDLE character, not the last one (BACKLOG
    // test-login-tamper-flake): the original `+ "xx"` could
    // reconstruct the SAME token when it already ended in "xx"
    // (~1 in 4096) — but flipping only the LAST char has its own,
    // much bigger trap: a 64-byte ES256 signature base64url-encodes
    // to 86 chars carrying 516 bits for 512 bits of real signature,
    // so the FINAL char holds just 2 significant bits (4 are
    // decoder-ignored padding) — many single-char flips there decode
    // to the IDENTICAL signature bytes, verifying anyway (~40%
    // observed empirically, not the rare edge case the first fix
    // assumed). A middle character sits inside a fully-significant
    // 6-bit block on any reasonable token length, so every flip
    // there changes real bytes.
    val i = token.length / 2
    val flipped = token.updated(i, if token(i) == 'x' then 'y' else 'x')
    assertEquals(Login.verify(flipped), None)
    assertEquals(Login.verify("not.a.jwt"), None)
    assertEquals(Login.verify(""), None)
  }
