package okay.freer

import okay.{guard}

/**
 * specs/cont-stack.md, the Native row: the room is READ from the
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

  test("the end of a room reads more of a stack that has it, and none of one that does not") {
    assert(onThread(8192)(StackSwitch.more()) > 1000, "an 8 MB thread granted almost nothing")
    assertEquals(onThread(128)(lowThenMore()), 0)
  }

  /** 60 KB of this frame's own, then ask: a 128 KB thread so low has less than a 16-level grant over the margin */
  private def lowThenMore(): Int =
    val pad = scala.scalanative.unsafe.stackalloc[Byte](60000)
    pad(0) = 1.toByte
    StackSwitch.more()

  test("20 000 tail shifts on a 2 MB thread: the answer, a switch at the first room and none after") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(2048)(Cont.reset(tail(20000))), 20000)
    val switches = StackSwitch.switches.get() - before
    assert(switches >= 1, "20 000 levels in 2 MB never switched")
    assert(switches < 100, s"$switches switches: the fresh stack's room was not used")
  }

  test("a 128 KB thread switches and answers") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(128)(Cont.reset(tail(2000))), 2000)
    assert(StackSwitch.switches.get() - before >= 1)
  }

  test("multi-shot across the switch") {
    val d = 14
    val m = (1 to d).foldLeft(Cont.Pure[Long, Long](0L): Long /> Long)((m, _) => m.flatMap(x => Cont.shift[Long, Long, Long](k => k(x + 1) + k(x + 1))))
    assertEquals(onThread(2048)(Cont.reset(m)), (1L << d) * d)
  }
