package okay

import scala.compiletime.testing.typeChecks

/**
 * Named tuples work under `import okay.*`, and the loop DSL that once
 * blocked them still works (named-tuple-unblock).
 *
 * The defect this pins: `extension [A](a: A) inline def apply[R](f: A
 * Loop R)` offers an apply on EVERY type, a named tuple's field access
 * desugars to an apply BY INDEX, and so the wildcard import turned
 * `t.route` into "Found: (0 : Int)". A stable language feature was
 * disabled for anyone importing this package, and nothing in the
 * repository noticed for as long as nobody wrote a named tuple.
 */
class TestNamedTuples extends munit.FunSuite {

  type Trip = (route: String, service: String)

  test("a named tuple's fields are reachable with okay.* in scope") {
    val t: Trip = (route = "31", service = "weekday")
    assertEquals(t.route, "31")
    assertEquals(t.service, "weekday")
  }

  test("the names are free: a named tuple IS the plain tuple at runtime") {
    val t: Trip = (route = "31", service = "weekday")
    val plain: (String, String) = ("31", "weekday")
    assertEquals(t.getClass.getName, plain.getClass.getName)
    assert(t == plain, "a named tuple must equal the plain one it erases to")
  }

  test("the loop DSL the guard protects still RESOLVES, which is what the guard could have broken") {
    // `seed(body)` is what this extension exists for, and the two
    // fixes priced in BUGS.md would have removed the spelling. The
    // check is RESOLUTION, not semantics: a bare `take` body is an
    // infinite loop by construction (the continuation is the
    // recursive call), so it is type-checked and not run.
    assert(typeChecks("""import okay.*
                         val body: Int Loop Int = take[Int, Int]
                         val answer: Int = 0(body)"""),
      "the guarded extension must still apply to an ordinary seed")
  }

  test("the guard is exact: it declines named tuples and nothing else") {
    // paired, so the refusal is the named tuple rather than the snippet
    assert(typeChecks("""import okay.*
                         val t: (a: Int, b: Int) = (a = 1, b = 2)
                         val x: Int = t.a"""),
      "a named tuple's field must be reachable under the wildcard import")
    assert(typeChecks("""import okay.*
                         val p: (Int, Int) = (1, 2)
                         val x: Int = p(0)"""),
      "and a plain tuple still indexes, which is the compiler's own apply")
  }
}
