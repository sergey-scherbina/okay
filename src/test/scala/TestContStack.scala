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
    assertEquals(SmallStack.run(2048)(reset(m)), n)
  }

  test("shifts in a row whose bodies USE the answer: k(v) + 1") {
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1) + 1)))
    assertEquals(SmallStack.run(2048)(reset(m)), 2 * n)
  }

  test("an absorbed leaf in a row: shift(...).flatMap(...) each time") {
    def step(x: Int): Int /> Int = shift[Int, Int, Int](k => k(x + 1)).flatMap(y => Cont.Pure[Int, Int](y))
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(step))
    assertEquals(SmallStack.run(2048)(reset(m)), n)
  }

  test("multi-shot across the switch: k called twice at every level") {
    val d = 14
    val m = (1 to d).foldLeft(Cont.Pure[Long, Long](0L): Long /> Long)((m, _) => m.flatMap(x => shift[Long, Long, Long](k => k(x + 1) + k(x + 1))))
    assertEquals(SmallStack.run(2048)(reset(m)), (1L << d) * d)
  }

  test("an exception thrown deep crosses every switch") {
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) =>
      m.flatMap(x => shift[Int, Int, Int](k => if x == n / 2 then throw IllegalStateException("deep") else k(x + 1))))
    val e = intercept[IllegalStateException](SmallStack.run(2048)(reset(m)))
    assertEquals(e.getMessage, "deep")
  }

  /** tail shifts in a row, `n` of them */
  private def tail(n: Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1))))

  private def switchesDuring[A](body: => A): (A, Long) =
    val before = StackSwitch.switches.get()
    val a = body
    (a, StackSwitch.switches.get() - before)

  test("a program the stack can hold switches ZERO times where the stack is read, once at most where it is counted") {
    // 1000 levels on a default-size thread (2 MB here): far past the
    // suite's first room of 64, well inside the stack
    val (answer, switches) = switchesDuring(SmallStack.run(2048)(reset(tail(1000))))
    assertEquals(answer, 1000)
    if StackRoom.sp() >= 0 then assertEquals(switches, 0L, "exact road: the stack had room, and it switched")
    else assert(switches <= 1000 / 64 + 1, s"count road: $switches switches")
  }

  test("a 256 KB thread still switches, and answers") {
    val (answer, switches) = switchesDuring(SmallStack.run(256)(reset(tail(2000))))
    assertEquals(answer, 2000)
    assert(switches >= 1, "a 256 KB stack cannot hold 2000 levels, and it never switched")
  }

  test("an explicit 8 MB thread is granted more than a 2 MB one (exact road)") {
    assume(StackRoom.sp() >= 0, "the stack is not readable on this JVM")
    val (_, on2) = switchesDuring(SmallStack.run(2048)(reset(tail(n))))
    val (_, on8) = switchesDuring(SmallStack.run(8192)(reset(tail(n))))
    assert(on8 < on2, s"8 MB switched $on8 times, 2 MB $on2")
    assert(on2 >= 1, s"20 000 levels on 2 MB never switched ($on2)")
  }

  test("the first room is derived from the VM's default stack when no property is set") {
    // the suite itself runs with -Dokay.cont.room=64; the derivation is
    // what the benchmarks get
    assert(StackSwitch.defaultStackBytes >= (1L << 20), s"default stack ${StackSwitch.defaultStackBytes}")
    assertEquals(StackSwitch.firstRoom, 64)
  }
