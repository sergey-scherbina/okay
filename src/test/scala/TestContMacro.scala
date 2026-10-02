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
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x => Cont.shift[Int, Int, Int](k => k(x + 1))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("tail calls under if and match branches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k =>
        if x % 2 == 0 then k(x + 1)
        else x % 3 match
          case 0 => k(x + 1)
          case _ => k(x + 1))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("statements before the tail call run once, in order, when the runner reaches the shift") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    val m = Cont.shift[Int, Int, Int](k => { log += "body"; k(1) }).flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(log.toList, Nil, "the body ran at construction")
    assertEquals(Cont.reset(m), 1)
    assertEquals(log.toList, List("body", "then 1"))
    assertEquals(Cont.reset(m), 1)
    assertEquals(log.toList, List("body", "then 1", "body", "then 1"))
  }

  test("an exception from the body surfaces at run time, not at construction") {
    val m = Cont.shift[Int, Int, Int](k => if true then throw IllegalStateException("boom") else k(1))
    val e = intercept[IllegalStateException](Cont.reset(m))
    assertEquals(e.getMessage, "boom")
  }

  test("bodies that are NOT tail-shaped keep their meaning") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(k(1)))), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2).map(k).sum)), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](_ => 42)), 42)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if false then k(1) else 7)), 7)
  }

  // ---- Layer 1 B (specs/cont-stack.md plan stage E, cont-stack-layer1-b):
  // a body that USES the answer of `k` is CPS-transformed onto an
  // explicit stack of pending parts, so it nests no frame either. The
  // zero-switch count on a 128 KB stack is again what tells the
  // rewrite apart from the runtime layer. A function answer is not
  // transformed (measured slower than the direct road) and keeps its
  // meaning through Layer 2.

  test("1M answer-using shifts k => k(x + 1) + 1 on a 128 KB stack: the answer and ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x => Cont.shift[Int, Int, Int](k => k(x + 1) + 1)))))
    assertEquals(a, 2 * n)
    assertEquals(s, 0L)
  }

  test("a function answer (PState's k(a)(s2)) stays on the direct road and keeps its meaning") {
    // measured 2.8x slower walked than direct on statePara (specs/cont-stack.md stage E)
    assertEquals(
      PState.run(0L)((1 to 1000).foldLeft(PState.get[Long, (Long, Long)])((m, _) =>
        m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1))))),
      (1000L, 999L))
    assertEquals((Cont.shift[Int, Int => Int, Int => Int](k => (s: Int) => k(s + 1)(s * 2)) / (a => (s: Int) => a + s))(3), 10)
  }

  test("answer-using shapes keep their meaning, multi-shot included") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10))), 11)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(k(1)))), 1)
    assertEquals(Cont.reset(Cont.shift[String, String, String](k => s"${k("a")}-${k("b")}")), "a-b")
    assertEquals(Cont.reset(Cont.shift[List[Int], List[Int], List[Int]](k => 0 :: k(List(1)))), List(0, 1))
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { val a = k(1); val b = k(2); a * 10 + b })), 12)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if k(1) > 0 then k(2) + 1 else k(3))), 3)
    var x = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { x = k(5); x + 1 })), 6)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) match { case 1 => k(7) case _ => 0 })), 7)
    // the answer used by a continuation after the shift
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => k(1) + k(10)).flatMap(x => Cont.Pure[Int, Int](x * 2))), 22)
  }

  test("what ran before a call of k still runs before it, and what came after still after") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    def pre(): Int = { log += "pre"; 1 }
    def post(): Int = { log += "post"; 1 }
    def add(a: Int, b: Int, c: Int): Int = a + b + c
    val m = Cont.shift[Int, Int, Int](k => { log += "body"; add(pre(), k(10), post()) })
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(Cont.reset(m), 12)
    assertEquals(log.toList, List("body", "pre", "then 10", "post"))
  }

  test("an exception after a call of k surfaces once the rest has run") {
    val log = collection.mutable.ArrayBuffer.empty[String]
    def boom(): Int = throw IllegalStateException("boom")
    val m = Cont.shift[Int, Int, Int](k => k(1) + boom()).flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    val e = intercept[IllegalStateException](Cont.reset(m))
    assertEquals(e.getMessage, "boom")
    assertEquals(log.toList, List("then 1"))
  }

  test("1M non-tail conditionals 1 + (if c then k(x + 1) else k(x + 1)) on a 128 KB stack: the answer and ZERO switches") {
    // a JOIN POINT (cont-stack-layer1-c (1)): the rest after the conditional is one local function, each branch
    // ends in it; until then the body was opaque and its strict k switched stacks
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => 1 + (if x % 2 == 0 then k(x + 1) else k(x + 1)))))))
    assertEquals(a, 2 * n)
    assertEquals(s, 0L)
  }

  test("non-tail conditionals keep their meaning: if and match, k in some branches, the rest after them, order") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => 1 + (if true then k(1) else 2))), 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => 1 + (if false then k(1) else 2))), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (if true then k(1) else k(2)) * 10 + k(3))), 13)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => 100 + (3 match { case 1 => k(1) case 3 => k(3) + 1 case _ => 0 }))), 104)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => 100 + (9 match { case 1 => k(1) case _ => 0 }))), 100)
    // multi-shot across the join: both calls of k, the rest once per branch taken
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (if k(1) > 0 then k(2) else 0) + k(10))), 12)
    // order: the condition, then the branch's call of k (the rest after the shift), then the rest of the body
    val log = collection.mutable.ArrayBuffer.empty[String]
    def cond(): Boolean = { log += "cond"; true }
    val m = Cont.shift[Int, Int, Int](k => { val r = (if cond() then k(5) else 0); log += "after"; r + 1 })
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(Cont.reset(m), 6)
    assertEquals(log.toList, List("cond", "then 5", "after"))
  }

  test("1M bodies that call k inside List.map on a 128 KB stack: the answer and ZERO switches") {
    // the known traversals (cont-stack-layer1-c (2)): the lambda's body a program over the lazy k, the
    // traversal a chain of binds the machine runs; until then the body was opaque
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => List(x).map(y => k(y + 1)).sum)))))
    assertEquals(a, n)
    assertEquals(s, 0L)
  }

  test("k inside map, foreach and foldLeft over List, Vector and Seq keeps its meaning: order, multi-shot, types") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2).map(x => k(x)).sum)), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Vector(1, 2, 3).map(x => k(x) * 10).sum)), 60)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Seq(4, 5).map(x => k(x)).last)), 5)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2, 3).foldLeft(0)((acc, x) => acc * 10 + k(x)))), 123)
    var seen = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { List(1, 2).foreach(x => seen += k(x)); seen })), 3)
    // the result keeps its collection type
    val v: Vector[Int] = Cont.reset(Cont.shift[Int, Vector[Int], Vector[Int]](k => Vector(1, 2).map(x => k(x).sum)).map(x => Vector(x, x)))
    assertEquals(v, Vector(2, 4))
    // order: elements in turn, each call of k running the rest after the shift before the next element
    val log = collection.mutable.ArrayBuffer.empty[String]
    val m = Cont.shift[Int, Int, Int](k => List(1, 2).map(x => { log += s"el $x"; k(x) }).sum)
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(Cont.reset(m), 3)
    assertEquals(log.toList, List("el 1", "then 1", "el 2", "then 2"))
    // multi-shot: the outer k resumed twice, each time traversing again
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => List(z, z).map(x => k(x)).sum))), 6)
  }

  test("bodies the transform cannot read stay opaque and keep their meaning") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => try k(1) catch { case _: Exception => 0 })), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option.empty[Int].getOrElse(k(4)))), 4)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { var i = 0; while i < 3 do i += k(1); i })), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (() => k(1))())), 1)
  }

  test("the Control[Cont] instance's shift keeps its meaning") {
    val c = summon[Control[Cont]]
    assertEquals(c.shift[Int, Int, Int](k => k(5)) / identity, 5)
  }
