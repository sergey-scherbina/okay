package okay2

/**
 * Layer 1 A in Scala 2 (cont-stack-okay2-macro, the Scala 3 core's TestContMacro stage A): a shift whose body
 * only calls its continuation in TAIL position, with an argument free of it, is that argument evaluated when the
 * runner reaches the shift — `Cont.shift` rewrites it to exactly that, so it nests no frame, counts no room and
 * never switches. The zero-switch assertion tells the rewrite apart from the runtime layer, which would also
 * ANSWER correctly (by switching). The JVM suites run forked and one after another, so the process-wide counter
 * is this suite's.
 */
class TestContMacro extends munit.FunSuite {

  val n = 1000000

  private def switchesDuring[A](body: => A): (A, Long) = {
    val before = StackSwitch.switches.get()
    val a = body
    (a, StackSwitch.switches.get() - before)
  }

  private def row(n: Int)(step: Int => Int /> Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(step))

  test("1M tail shifts k => k(x + 1) on a 128 KB stack: the answer and ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x => Cont.shift[Int, Int, Int](k => k(x + 1))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("1M tail shifts under if, match and a block, and a literal, on 128 KB: ZERO switches") {
    val bodies = List[(String, Int => Int /> Int)](
      "if" -> (x => Cont.shift[Int, Int, Int](k => if (x >= 0) k(x + 1) else k(0))),
      "match" -> (x => Cont.shift[Int, Int, Int](k => x % 2 match { case 0 => k(x + 1); case _ => k(x + 1) })),
      "block" -> (x => Cont.shift[Int, Int, Int](k => { val y = x + 1; k(y) })),
      "throw branch" -> (x => Cont.shift[Int, Int, Int](k => if (x < 0) throw new IllegalStateException("never") else k(x + 1))))
    bodies.foreach { case (label, body) =>
      val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(body))))
      assertEquals(a, n, label)
      assertEquals(s, 0L, label)
    }
    val (lit, s2) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(_ => Cont.shift[Int, Int, Int](k => k(7))))))
    assertEquals(lit, 7)
    assertEquals(s2, 0L)
  }
}
