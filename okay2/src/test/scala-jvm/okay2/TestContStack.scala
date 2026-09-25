package okay2

/**
 * specs/cont-stack.md in the Scala 2.13 core (cont-stack-okay2): a
 * shift whose body calls its continuation runs the rest of the
 * program inside that call, so shifts in a row nest; past the room the
 * rest continues on a fresh stack. Each test ran RED on a 2 MB thread
 * before the switch existed (20 000 levels overflow it).
 */
class TestContStack extends munit.FunSuite {

  val n = 20000

  private def row(n: Int)(step: Int => Int /> Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(step))

  test("shifts in a row whose bodies return k(v): the answer, on a 2 MB thread") {
    assertEquals(SmallStack.run(2048)(reset(row(n)(x => shift[Int, Int, Int](k => k(x + 1))))), n)
  }

  test("shifts in a row whose bodies USE the answer: k(v) + 1") {
    assertEquals(SmallStack.run(2048)(reset(row(n)(x => shift[Int, Int, Int](k => k(x + 1) + 1)))), 2 * n)
  }

  test("an absorbed leaf in a row: shift(...).flatMap(...) each time") {
    def step(x: Int): Int /> Int = shift[Int, Int, Int](k => k(x + 1)).flatMap(y => Cont.Pure[Int, Int](y))
    assertEquals(SmallStack.run(2048)(reset(row(n)(step))), n)
  }

  test("multi-shot across the switch: k called twice at every level") {
    val d = 14
    val m = (1 to d).foldLeft(Cont.Pure[Long, Long](0L): Long /> Long)((m, _) => m.flatMap(x => shift[Long, Long, Long](k => k(x + 1) + k(x + 1))))
    assertEquals(SmallStack.run(2048)(reset(m)), (1L << d) * d)
  }

  test("an exception thrown deep crosses every switch") {
    val m = row(n)(x => shift[Int, Int, Int](k => if (x == n / 2) throw new IllegalStateException("deep") else k(x + 1)))
    val e = intercept[IllegalStateException](SmallStack.run(2048)(reset(m)))
    assertEquals(e.getMessage, "deep")
  }

  test("a 256 KB thread still switches, and answers") {
    val before = StackSwitch.switches.get()
    assertEquals(SmallStack.run(256)(reset(row(2000)(x => shift[Int, Int, Int](k => k(x + 1))))), 2000)
    assert(StackSwitch.switches.get() - before >= 1, "a 256 KB stack cannot hold 2000 levels, and it never switched")
  }
}
