package okay

/**
 * specs/cont-stack.md, the Native row: the room is exact, from the
 * runtime's own `ThreadInfo`, and the switch is a 1 GB platform thread.
 */
class TestContStackNative extends munit.FunSuite:

  private def onThread[A](kb: Int)(body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", kb.toLong * 1024)
    t.start()
    t.join()
    out.fold(e => throw e, identity)

  private def tail(n: Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => k(x + 1))))

  test("a stackalloc address lies inside the bounds the runtime reports (the ThreadInfo layout guard)") {
    val (top, floor, sp) = StackSwitch.probe()
    assert(top > 0 && floor > 0 && sp > 0, s"top=$top floor=$floor sp=$sp")
    assert(floor < sp && sp < top, s"$floor < $sp < $top")
    assert(top - floor < (1L << 31), s"a stack of ${top - floor} bytes")
  }

  test("20 000 tail shifts on a 2 MB thread: the answer, switching only when the stack is really out") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(2048)(reset(tail(20000))), 20000)
    val switches = StackSwitch.switches.get() - before
    assert(switches >= 1, "20 000 levels in 2 MB never switched")
    assert(switches < 100, s"$switches switches: the exact room was not read")
  }

  test("a 128 KB thread switches and answers") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(128)(reset(tail(2000))), 2000)
    assert(StackSwitch.switches.get() - before >= 1)
  }

  test("multi-shot across the switch") {
    val d = 14
    val m = (1 to d).foldLeft(Cont.Pure[Long, Long](0L): Long /> Long)((m, _) => m.flatMap(x => shift[Long, Long, Long](k => k(x + 1) + k(x + 1))))
    assertEquals(onThread(2048)(reset(m)), (1L << d) * d)
  }
