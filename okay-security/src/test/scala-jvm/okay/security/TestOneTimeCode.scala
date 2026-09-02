package okay.security

/** specs/security.md, security-sessions — OneTimeCode. */
class TestOneTimeCode extends munit.FunSuite {

  test("start-then-confirm with the right code succeeds ONCE") {
    val otc = OneTimeCode()
    val code = otc.start("alice@x")
    assert(otc.confirm("alice@x", code))
    assert(!otc.confirm("alice@x", code), "a spent code must not confirm again")
  }

  test("a wrong code fails without spending the real one") {
    val otc = OneTimeCode()
    val code = otc.start("alice@x")
    assert(!otc.confirm("alice@x", "000000"))
    assert(otc.confirm("alice@x", code), "the real code still confirms")
  }

  test("a confirm past the deadline fails") {
    val otc = OneTimeCode(ttlMs = 1000)
    val now = System.currentTimeMillis()
    val code = otc.start("alice@x", now = now)
    assert(!otc.confirm("alice@x", code, now = now + 1001))
  }

  test("starting again replaces the earlier code") {
    val otc = OneTimeCode()
    val first = otc.start("alice@x")
    val second = otc.start("alice@x")
    assert(!otc.confirm("alice@x", first))
    assert(otc.confirm("alice@x", second))
  }
}
