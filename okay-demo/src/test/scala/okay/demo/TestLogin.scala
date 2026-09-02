package okay.demo

import okay.crypto.given

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
    assertEquals(Login.verify(token.dropRight(2) + "xx"), None)
    assertEquals(Login.verify("not.a.jwt"), None)
    assertEquals(Login.verify(""), None)
  }
