package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
import okay.Direct.*
import scala.language.implicitConversions

/**
 * SHIFT INSIDE A DIRECT BLOCK, over Cps itself (cont-in-direct,
 * 2026-09-17). `Shift.shift` captures under a handler; this is the
 * same word for the bare paramonad, on its DIAGONAL — the blocks whose
 * answer type does not move, which is the only shape `direct` has.
 */
class TestContDirect extends munit.FunSuite {

  /** the diagonal at String: one monad, so one `direct` block */
  type Str = [X] =>> Cps[X, String, String]

  test("shift in a direct block: the continuation is the rest of the block") {
    import okay.freer.Cps.direct.*
    val c: String />> String = direct[Str]:
      val x: String = !shift[String](k => k("one") + " " + k("two"))
      "<" + x + ">"
    // the block after the capture ran TWICE, once per call of k
    assertEquals(Cps.reset(c), "<one> <two>")
  }

  test("an import is a statement a direct block may contain") {
    val c: String />> String = direct[Str]:
      import okay.freer.Cps.direct.*         // binds nothing, runs nothing
      val x: String = !shift[String](k => k("a") + k("b"))
      x + "!"
    assertEquals(Cps.reset(c), "a!b!")
  }

  test("no capture: the block is an ordinary program") {
    val c: String />> String = direct[Str]:
      val a = "plain"
      a.toUpperCase
    assertEquals(Cps.reset(c), "PLAIN")
  }

  test("the answer type is the block's, so it need not be written") {
    import okay.freer.Cps.direct.*
    val c: Int />> Int = direct[[X] =>> Cps[X, Int, Int]]:
      val n: Int = !shift[Int](k => k(1) + k(2) + k(3))
      n * 10
    assertEquals(Cps.reset(c), 60)
  }

  test("an import in a direct block over a program row") {
    // the same macro rule, where blocks actually live: a row of effects
    val p: Int ! State % Int + okay.freer.Pure = direct:
      import okay.std.State.modify
      val a = !modify[Int](_ + 1)
      val b = !modify[Int](_ * 2)
      a + b
    assertEquals(State.run(3)(p)._2, 4 + 8)
  }

  test("outside a direct block the one-argument shift does not compile") {
    val e = compileErrors("okay.freer.Cps.direct.shift[Int](k => k(1))")
    assert(e.nonEmpty, "the direct-only capture compiled with no block around it")
    assert(e.contains("DirectCtx"), s"refused for the wrong reason: $e")
  }

  test("the package-level shift still takes its three type arguments") {
    // the reason `direct.shift` is an import and not an overload:
    // this call shape — no type arguments — must keep resolving here
    val c: Int />> Int = okay.freer.Cps.shift[Int, Int, Int](k => k(1) + k(2))
    assertEquals(Cps.reset(c), 3)
  }
}
