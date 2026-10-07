package okay.freer

import okay.*

/**
 * docs/cont-stack.md's examples, VERBATIM (doc-snippet-debt): each
 * line as the page prints it, answer comment included, then asserted.
 * The deep one runs on a 128 KB thread, as the page claims.
 */
class TestDocExamplesContStack extends munit.FunSuite:

  test("a million tail shifts on a 128 KB stack") {
    val deep = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1))))
    val before = StackSwitch.switches.get()
    val answer = SmallStack.run(128):
      Cont.reset(deep) // 1000000 — no frame per level: the body is the value it passes
    assertEquals(answer, 1000000)
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }

  test("a million answer-using shifts on a 128 KB stack: the pending parts on the runner's stack") {
    val used = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1) + 1)))
    val before = StackSwitch.switches.get()
    val answer = SmallStack.run(128):
      Cont.reset(used) // 2000000 — no frame per level either: the pending `+ 1`s live on the runner's own stack
    assertEquals(answer, 2000000)
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }

  test("twenty thousand opaque shifts: each level a frame, the rest on a fresh stack") {
    val opaque = (1 to 20_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => try k(x + 1) catch { case _: ArithmeticException => 0 })))
    val answer = SmallStack.run(2048):
      Cont.reset(opaque) // 20000 — `k` under `try`: each level a frame; past the room the rest runs on a fresh stack
    assertEquals(answer, 20000)
  }
