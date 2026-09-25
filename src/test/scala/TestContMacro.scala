package okay

/**
 * specs/cont-stack.md plan stage B, Layer 1 A: a shift whose body only
 * ever calls its continuation in TAIL position, with an argument that
 * does not mention it, is `pure(v)` evaluated at run time — and `shift`
 * rewrites it to exactly that, so it nests no frame, counts no room and
 * never switches. The zero-switch assertion is what tells the rewrite
 * apart from the runtime layer, which would also ANSWER correctly (by
 * switching): each test here was red on that count first.
 */
class TestContMacro extends munit.FunSuite:

  val n = 1_000_000

  private def switchesDuring[A](body: => A): (A, Long) =
    val before = StackSwitch.switches.get()
    val a = body
    (a, StackSwitch.switches.get() - before)

  private def row(n: Int)(step: Int => Int /> Int): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(step))

  test("1M tail shifts k => k(x + 1) on a 128 KB stack: the answer and ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(reset(row(n)(x => shift[Int, Int, Int](k => k(x + 1))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("tail calls under if and match branches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(reset(row(n)(x =>
      shift[Int, Int, Int](k =>
        if x % 2 == 0 then k(x + 1)
        else x % 3 match
          case 0 => k(x + 1)
          case _ => k(x + 1))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("statements before the tail call run once, in order, when the runner reaches the shift") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    val m = shift[Int, Int, Int](k => { log += "body"; k(1) }).flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(log.toList, Nil, "the body ran at construction")
    assertEquals(reset(m), 1)
    assertEquals(log.toList, List("body", "then 1"))
    assertEquals(reset(m), 1)
    assertEquals(log.toList, List("body", "then 1", "body", "then 1"))
  }

  test("an exception from the body surfaces at run time, not at construction") {
    val m = shift[Int, Int, Int](k => if true then throw IllegalStateException("boom") else k(1))
    val e = intercept[IllegalStateException](reset(m))
    assertEquals(e.getMessage, "boom")
  }

  test("bodies that are NOT tail-shaped keep their meaning") {
    assertEquals(reset(shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(reset(shift[Int, Int, Int](k => k(k(1)))), 1)
    assertEquals(reset(shift[Int, Int, Int](k => List(1, 2).map(k).sum)), 3)
    assertEquals(reset(shift[Int, Int, Int](_ => 42)), 42)
    assertEquals(reset(shift[Int, Int, Int](k => if false then k(1) else 7)), 7)
  }

  test("the Control[Cont] instance's shift keeps its meaning") {
    val c = summon[Control[Cont]]
    assertEquals(c.shift[Int, Int, Int](k => k(5)) / identity, 5)
  }
