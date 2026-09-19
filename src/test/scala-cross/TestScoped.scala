package okay

/** okay.Scoped (script-scoped-state, scoped-to-core): a ThreadLocal
 * with no public `set`. `current` reads the default when nothing is
 * bound; `where` binds for its own extent, restores on every exit
 * including an exception, and nests to the nearest `where`.
 */
class TestScoped extends munit.FunSuite:

  test("current answers the default when nothing has been bound") {
    val s = Scoped(1)
    assertEquals(s.current, 1)
  }

  test("where binds for its own extent, then restores the default") {
    val s = Scoped("default")
    var seenInside = ""
    s.where("bound") { seenInside = s.current }
    assertEquals(seenInside, "bound")
    assertEquals(s.current, "default")
  }

  test("where restores even when body throws") {
    val s = Scoped(0)
    val _ = intercept[RuntimeException] {
      s.where(1) { throw new RuntimeException("boom") }
    }
    assertEquals(s.current, 0)
  }

  test("nesting resolves to the nearest where, then unwinds to the outer one") {
    val s = Scoped("default")
    s.where("outer") {
      assertEquals(s.current, "outer")
      s.where("inner") { assertEquals(s.current, "inner") }
      assertEquals(s.current, "outer")
    }
    assertEquals(s.current, "default")
  }
