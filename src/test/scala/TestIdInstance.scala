package okay

/**
 * The identity instance is found by TYPE, not lexically
 * (comonad-id-map-capture, 2026-09-23). As a package-level given,
 * `Comonad[Id]` put `Functor`'s `map` extension on every type in
 * package `okay` and under `import okay.given`, where it beat facade
 * companions (`Static`, `Cont`, `Prog`) and foreign `.map`s (kyo, the
 * throws union). In `Comonad`'s companion it is still in the IMPLICIT
 * scope of `Comonad[Id]` — so summoning it and the handler it derives
 * work unchanged — but no longer in the lexical scope of a bare value.
 */
class TestIdInstance extends munit.FunSuite:

  test("a bare value has no universal .map in package okay") {
    val e = compileErrors("""
      val n: Int = 5
      n.map(_ + 1)
    """)
    assert(e.contains("map"), e)
  }

  test("... nor under import okay.given") {
    val e = compileErrors("""
      import okay.given
      val s: String = "abc"
      val t: String = s.map(_.toUpper)
      val n: Int = 5
      n.map(_ + 1)
    """)
    assert(e.contains("value map is not a member of Int"), e)
  }

  test("Comonad[Id] is still summoned by type") {
    val C = summon[Comonad[Id]]
    assertEquals(C.fmap(5, _ + 1), 6)
    assertEquals(C.extract(7), 7)
  }

  test("... and so is the Handler it derives") {
    assert(summon[Handler[Id]].isInstanceOf[ComonadHandler[?]])
  }
