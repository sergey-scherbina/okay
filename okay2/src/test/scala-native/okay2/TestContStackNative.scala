package okay2

/** specs/cont-stack.md, the Native row, in the Scala 2 core: the room is
 * exact from the runtime's own `ThreadInfo`, the switch a pooled 1 GB
 * thread */
class TestContStackNative extends munit.FunSuite {

  private def onThread[A](kb: Int)(body: => A): A = {
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch { case e: Throwable => Left(e) }, "small-stack", kb.toLong * 1024)
    t.start()
    t.join()
    out.fold(e => throw e, identity)
  }

  private def tail(n: Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1))))

  test("a stackalloc address lies inside the bounds the runtime reports (the ThreadInfo layout guard)") {
    val (top, floor, sp) = StackSwitch.probe()
    assert(top > 0 && floor > 0 && sp > 0, s"top=$top floor=$floor sp=$sp")
    assert(floor < sp && sp < top, s"$floor < $sp < $top")
  }

  test("20 000 tail shifts on a 2 MB thread: the answer, switching only when the stack is really out") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(2048)(reset(tail(20000))), 20000)
    val switches = StackSwitch.switches.get() - before
    assert(switches >= 1 && switches < 100, s"$switches switches")
  }

  test("a 128 KB thread switches and answers") {
    val before = StackSwitch.switches.get()
    assertEquals(onThread(128)(reset(tail(2000))), 2000)
    assert(StackSwitch.switches.get() - before >= 1)
  }
}
