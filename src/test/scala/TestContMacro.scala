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

  // ---- Layer 1 B (specs/cont-stack.md plan stage E, cont-stack-layer1-b):
  // a body that USES the answer of `k` is CPS-transformed onto an
  // explicit stack of pending parts, so it nests no frame either. The
  // zero-switch count on a 128 KB stack is again what tells the
  // rewrite apart from the runtime layer. A function answer is not
  // transformed (measured slower than the direct road) and keeps its
  // meaning through Layer 2.

  test("1M answer-using shifts k => k(x + 1) + 1 on a 128 KB stack: the answer and ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(reset(row(n)(x => shift[Int, Int, Int](k => k(x + 1) + 1)))))
    assertEquals(a, 2 * n)
    assertEquals(s, 0L)
  }

  test("a function answer (PState's k(a)(s2)) stays on the direct road and keeps its meaning") {
    // measured 2.8x slower walked than direct on statePara (specs/cont-stack.md stage E)
    assertEquals(
      PState.run(0L)((1 to 1000).foldLeft(PState.get[Long, (Long, Long)])((m, _) =>
        m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1))))),
      (1000L, 999L))
    assertEquals((shift[Int, Int => Int, Int => Int](k => (s: Int) => k(s + 1)(s * 2)) / (a => (s: Int) => a + s))(3), 10)
  }

  test("answer-using shapes keep their meaning, multi-shot included") {
    assertEquals(reset(shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(reset(shift[Int, Int, Int](k => k(k(1)))), 1)
    assertEquals(reset(shift[String, String, String](k => s"${k("a")}-${k("b")}")), "a-b")
    assertEquals(reset(shift[List[Int], List[Int], List[Int]](k => 0 :: k(List(1)))), List(0, 1))
    assertEquals(reset(shift[Int, Int, Int](k => { val a = k(1); val b = k(2); a * 10 + b })), 12)
    assertEquals(reset(shift[Int, Int, Int](k => if k(1) > 0 then k(2) + 1 else k(3))), 3)
    var x = 0
    assertEquals(reset(shift[Int, Int, Int](k => { x = k(5); x + 1 })), 6)
    assertEquals(reset(shift[Int, Int, Int](k => k(1) match { case 1 => k(7) case _ => 0 })), 7)
    // the answer used by a continuation after the shift
    assertEquals(reset(shift[Int, Int, Int](k => k(1) + k(10)).flatMap(x => Cont.Pure[Int, Int](x * 2))), 22)
  }

  test("what ran before a call of k still runs before it, and what came after still after") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    def pre(): Int = { log += "pre"; 1 }
    def post(): Int = { log += "post"; 1 }
    def add(a: Int, b: Int, c: Int): Int = a + b + c
    val m = shift[Int, Int, Int](k => { log += "body"; add(pre(), k(10), post()) })
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(reset(m), 12)
    assertEquals(log.toList, List("body", "pre", "then 10", "post"))
  }

  test("an exception after a call of k surfaces once the rest has run") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    def boom(): Int = throw IllegalStateException("boom")
    val m = shift[Int, Int, Int](k => k(1) + boom()).flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    val e = intercept[IllegalStateException](reset(m))
    assertEquals(e.getMessage, "boom")
    assertEquals(log.toList, List("then 1"))
  }

  test("bodies the transform cannot read stay opaque and keep their meaning") {
    assertEquals(reset(shift[Int, Int, Int](k => 1 + (if true then k(1) else 2))), 2)
    assertEquals(reset(shift[Int, Int, Int](k => try k(1) catch { case _: Exception => 0 })), 1)
    assertEquals(reset(shift[Int, Int, Int](k => Option.empty[Int].getOrElse(k(4)))), 4)
    assertEquals(reset(shift[Int, Int, Int](k => { var i = 0; while i < 3 do i += k(1); i })), 3)
    assertEquals(reset(shift[Int, Int, Int](k => List(1, 2).map(x => k(x)).sum)), 3)
    assertEquals(reset(shift[Int, Int, Int](k => (() => k(1))())), 1)
  }

  test("the Control[Cont] instance's shift keeps its meaning") {
    val c = summon[Control[Cont]]
    assertEquals(c.shift[Int, Int, Int](k => k(5)) / identity, 5)
  }
