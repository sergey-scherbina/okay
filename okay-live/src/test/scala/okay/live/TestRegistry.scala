package okay.live

import okay.*
import okay.given

/** specs/live.md — a per-key channel, created lazily, reused after. */
class TestRegistry extends munit.FunSuite {

  test("the same key returns the SAME channel on repeated calls") {
    val reg = Registry[String, String]()
    val a1 = reg("alice@x")
    val a2 = reg("alice@x")
    assert(a1 eq a2)
  }

  test("different keys get independent channels") {
    val reg = Registry[String, String]()
    val a = reg("alice@x")
    val b = reg("bob@x")
    assert(!(a eq b))
    a.offer("for alice"): Unit
    assertEquals(a.receiveBlocking(), Some("for alice"))
    b.offer("for bob"): Unit
    assertEquals(b.receiveBlocking(), Some("for bob"))
  }
}
