package okay.deploy

/**
 * The guide's `Fact` samples, seen from OUTSIDE package `okay`
 * (monoid-scope).
 *
 * THIS FILE IMPORTS NOTHING FROM `okay` ON PURPOSE. `compileErrors`
 * compiles its snippet in the enclosing file's context, so the same
 * assertions in `TestDiDocs` — which imports `okay.given` at the top —
 * passed while proving nothing. That is the vacuity this repository
 * has a name for, met again.
 *
 * What is pinned here: the base monoids (Vector, String, List, numbers)
 * live in `okay.Monoid`'s companion since the algebras moved to the root,
 * so a `Fact` over any of them needs no import at all.
 */
class TestMonoidScope extends munit.FunSuite:

  test("a Fact over Vector needs no import: its Monoid is in the companion") {
    assertEquals(compileErrors("object V extends okay.std.Fact[Vector[Int]]"), "")
  }

  test("a Fact over String needs no import either: the base monoids are in Monoid's companion") {
    assertEquals(compileErrors("object N extends okay.std.Fact[String]"), "")
    assertEquals(compileErrors("import okay.*; object N2 extends okay.std.Fact[List[Int]]"), "")
  }

  test("a number's monoid too: a Group's, found where a Monoid is asked") {
    assertEquals(compileErrors("object N3 extends okay.std.Fact[Int]"), "")
  }
