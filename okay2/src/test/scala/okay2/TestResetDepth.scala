package okay2

/**
 * THE KEYED `reset` NESTED WITHOUT A STACK (okay2-shift-stacked-key, the Scala 3 core's shift-stacked-key):
 * a `reset` run outermost is a machine run that a RUNNING machine steps into, so a hundred thousand nested
 * resets are one machine's loop — on Scala.js, which has no stack to switch to, and on a small JVM stack
 * (TestResetSmallStack).
 */
class TestResetDepth extends munit.FunSuite {

  type P = Pure

  test("a keyed reset and its capture run on this platform") {
    val r = reset[Int, P](shift0[Int, Int, P](k => k(1).flatMap(a => k(10).map(_ + a))).map(_ * 2))
    assertEquals(!.run(r), 22)
  }

  /** each level's reset is built inside the outer one's continuation */
  def nest(n: Int): Int ! P =
    if (n == 0) pure(0)
    else reset[Int, P](!.tailcall(nest(n - 1)).plus[Shift[Int]].flatMap(x => shift0[Int, Int, P](k => k(x + 1))))

  test("a hundred thousand nested resets of one answer type") {
    assertEquals(!.run(nest(100000)), 100000)
  }

  test("nested resets of two answer types, interleaved") {
    def two(n: Int): Int ! P =
      if (n == 0) pure(0)
      else reset[String, P](
        !.tailcall(reset[Int, P](!.tailcall(two(n - 1)).plus[Shift[Int]].flatMap(x => shift0[Int, Int, P](k => k(x + 1)))))
          .plus[Shift[String]].map(_.toString)).map(_.toInt)
    assertEquals(!.run(two(50000)), 50000)
  }

  test("a reset is a value: built, it runs nothing until forced, and runs again each time") {
    var ran = 0
    val r = reset[Int, P](pure[Shift[Int] + P, Unit](()).map(_ => { ran += 1; ran }))
    assertEquals(ran, 0)
    assertEquals(!.run(r), 1)
    assertEquals(!.run(r), 2)
  }
}
