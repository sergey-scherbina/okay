package okay

/**
 * specs/stack-safety.md stage 1c: direct-style Cont past the stack.
 * A shift whose body calls its continuation runs the REST of the
 * program inside that call, so shifts in a row nest; past a depth the
 * rest continues on a fresh stack. Each test ran RED on a 128 KB stack
 * before the switch existed.
 */
class TestContStack extends munit.FunSuite:

  val n = 20000

  test("shifts in a row whose bodies return k(v): the answer, on a small stack") {
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1))))
    assertEquals(SmallStack.run(512)(reset(m)), n)
  }

  test("shifts in a row whose bodies USE the answer: k(v) + 1") {
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1) + 1)))
    assertEquals(SmallStack.run(512)(reset(m)), 2 * n)
  }

  test("an absorbed leaf in a row: shift(...).flatMap(...) each time") {
    def step(x: Int): Int /> Int = shift[Int, Int, Int](k => k(x + 1)).flatMap(y => Cont.Pure[Int, Int](y))
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(step))
    assertEquals(SmallStack.run(512)(reset(m)), n)
  }

  test("multi-shot across the switch: k called twice at every level") {
    val d = 14
    val m = (1 to d).foldLeft(Cont.Pure[Long, Long](0L): Long /> Long)((m, _) => m.flatMap(x => shift[Long, Long, Long](k => k(x + 1) + k(x + 1))))
    assertEquals(SmallStack.run(512)(reset(m)), (1L << d) * d)
  }

  test("an exception thrown deep crosses every switch") {
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) =>
      m.flatMap(x => shift[Int, Int, Int](k => if x == n / 2 then throw IllegalStateException("deep") else k(x + 1))))
    val e = intercept[IllegalStateException](SmallStack.run(512)(reset(m)))
    assertEquals(e.getMessage, "deep")
  }
