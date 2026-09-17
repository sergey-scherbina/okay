package okay

/**
 * `.?` must mean something (specs/unwrap-glyph.md, stage 1).
 *
 * `throws` is an `into opaque` type with a `Conversion[A, A throws
 * E]`, so every value in the language is an `A throws Nothing` — and
 * before this guard `x.?` type-checked on ANY value and did nothing
 * at all. It cost an hour in the applicative-do lane: a `direct`
 * block written with `.?` compiled, ran, answered correctly through
 * auto-coloring, and the glyph was a no-op.
 */
class TestUnwrapGlyph extends munit.FunSuite {

  test("a value that cannot throw REFUSES the glyph") {
    assert(compileErrors("42.?").nonEmpty, "`42.?` still compiles: the no-op is back")
    assert(compileErrors("List(1).?").nonEmpty)
  }

  test("a program does NOT take the Throws glyph — this is the incident") {
    // with no `import Direct.*` in this file the only `?` a program
    // could find is the Throws one, and it must not find it. (With
    // Direct imported the glyph IS the mark — TestUnwrapMark.)
    assert(compileErrors("okay.pure[Nothing, Int](1).?").nonEmpty,
      "a program took the Throws glyph silently again")
  }

  test("a genuine throws value is unaffected") {
    val ok: Int throws Fault = 63
    assertEquals(ok.?, 63)
    val bad: Int throws Fault = Fault("no")
    intercept[Fault](bad.?)
    // the mending form takes an argument and never competed
    assertEquals(bad.?(_ => -1), -1)
  }
}
