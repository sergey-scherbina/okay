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
 * What is pinned here: `import okay.*` does NOT bring givens, the base
 * monoids are not all reachable the same way, and the import the guide
 * shows is the one that works.
 */
class TestMonoidScope extends munit.FunSuite:

  test("a Fact over Vector needs no import: its Monoid is in the companion") {
    assertEquals(compileErrors("object V extends okay.Fact[Vector[Int]]"), "")
  }

  test("a Fact over String needs `import okay.given`, and `import okay.*` is not enough") {
    val starOnly = compileErrors("import okay.*; object N extends okay.Fact[String]")
    assert(starOnly.contains("No given instance of type okay.Monoid[String]"), starOnly)
    assertEquals(compileErrors("import okay.given; object N2 extends okay.Fact[String]"), "")
  }

  test("nothing at all: the message names the given to import") {
    val bare = compileErrors("object N3 extends okay.Fact[String]")
    assert(bare.contains("okay.Monoid[String]"), bare)
  }
