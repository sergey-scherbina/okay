package okay

import okay.Row.plus

/**
 * THE KEYED `reset` ON EVERY PLATFORM, AND NESTED WITHOUT A STACK
 * (shift-stacked-key, specs/shift-merge.md): a `reset` run outermost is
 * a machine run that a RUNNING machine absorbs, so a hundred thousand
 * nested resets are one machine's loop — on Scala.js, which has no
 * stack to switch to, and on a small JVM stack (TestResetSmallStack).
 */
class TestResetDepth extends munit.FunSuite:

  type P = Pure

  test("a keyed reset and its capture run on this platform") {
    val r = reset[Int, P](shift0[Int, Int, P](k => k(1).flatMap(a => k(10).map(_ + a))).map(_ * 2))
    assertEquals(!.run(r), 22)
  }

  /** each level's reset is built inside the outer one's continuation */
  def nest(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else reset[Int, Pure](!.tailcall(nest(n - 1)).plus[Shift % Int].flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))

  test("a hundred thousand nested resets of one answer type") {
    assertEquals(!.run(nest(100000)), 100000)
  }

  test("nested resets of two answer types, interleaved") {
    def two(n: Int): Int ! Pure =
      if n == 0 then pure(0)
      else reset[String, Pure](
        !.tailcall(reset[Int, Pure](!.tailcall(two(n - 1)).plus[Shift % Int].flatMap(x => shift0[Int, Int, Pure](k => k(x + 1)))))
          .plus[Shift % String].map(_.toString)).map(_.toInt)
    assertEquals(!.run(two(50000)), 50000)
  }
