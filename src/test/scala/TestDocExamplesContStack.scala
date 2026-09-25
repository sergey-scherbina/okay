package okay

/**
 * docs/cont-stack.md's examples, VERBATIM (doc-snippet-debt): each
 * line as the page prints it, answer comment included, then asserted.
 * The deep one runs on a 128 KB thread, as the page claims.
 */
class TestDocExamplesContStack extends munit.FunSuite:

  test("a million tail shifts on a 128 KB stack") {
    val deep = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1))))
    val before = StackSwitch.switches.get()
    val answer = SmallStack.run(128):
      reset(deep) // 1000000 — no frame per level: the body is the value it passes
    assertEquals(answer, 1000000)
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }

  test("twenty thousand answer-using shifts: each level a frame, the rest on a fresh stack") {
    val used = (1 to 20_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1) + 1)))
    val answer = SmallStack.run(2048):
      reset(used) // 40000 — each level a frame; past the room the rest runs on a fresh stack
    assertEquals(answer, 40000)
  }
