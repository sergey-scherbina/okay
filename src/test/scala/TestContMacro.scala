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
/** inline helpers a shift body calls with `k` on their path (cont-stack-layer1-c (3)) */
object ContInlineHelpers:
  inline def applyTo(f: Int => Int, x: Int): Int = f(x)
  inline def plusOne(v: Int): Int = v + 1
  inline def twice(f: Int => Int, x: Int): Int = f(x) + f(x)

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

  test("1M bodies passing k itself to List.map, and assigning from k, on a 128 KB stack: ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => List(x + 1).map(k).sum)))))
    assertEquals(a, n)
    assertEquals(s, 0L)
    val (a2, s2) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => { var v = 0; v = k(x + 1); v })))))
    assertEquals(a2, n)
    assertEquals(s2, 0L)
  }

  test("k as a value to map/foreach, and assignment from k, keep their meaning: order and multi-shot") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2).map(k).sum)), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Vector(1, 2, 3).map(k).sum)), 6)
    var acc = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { List(1, 2).foreach(x => acc += k(x)); acc })), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { var v = 0; v = k(5); v + 1 })), 6)
    // the variable is read BEFORE the call of k, as in the strict road
    var w = 10
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { w += k(1); w }).flatMap(x => { w = 100; Cont.Pure[Int, Int](x) })), 11)
    // multi-shot: each resumption assigns again
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => { var v = 0; v = k(z); v * 10 }))), 30)
  }

  test("1M bodies with a while loop calling k, on a 128 KB stack: ZERO switches; a long loop without k holds no frame") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => { var v = 0; var i = 0; while i < 1 do { v = k(x + 1); i += 1 }; v })))))
    assertEquals(a, n)
    assertEquals(s, 0L)
    // a million iterations, k called in the first only: each iteration a trampolined step, not a host frame
    val (b, s2) = switchesDuring(SmallStack.run(128)(Cont.reset(
      Cont.shift[Int, Int, Int](k => { var v = 0; var i = 0; while i < 1_000_000 do { if i == 0 then v = k(7); i += 1 }; v + i }))))
    assertEquals(b, 1_000_007)
    assertEquals(s2, 0L)
  }

  test("a while loop with k keeps its meaning: order, the condition calling k, multi-shot") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { var s = 0; var i = 0; while i < 3 do { s += k(i); i += 1 }; s })), 3)
    // the condition itself calls k
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { var i = 0; while k(i) < 3 do i += 1; i * 10 })), 30)
    val log = collection.mutable.ArrayBuffer.empty[String]
    val m = Cont.shift[Int, Int, Int](k => { var i = 0; while i < 2 do { log += s"it $i"; log += s"k ${k(i)}"; i += 1 }; i })
      .flatMap(x => { log += s"then $x"; Cont.Pure[Int, Int](x) })
    assertEquals(Cont.reset(m), 2)
    assertEquals(log.toList, List("it 0", "then 0", "k 0", "it 1", "then 1", "k 1"))
    // multi-shot: the outer k resumed twice, the loop runs once per resumption
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => { var s = 0; var i = 0; while i < z do { s += k(1); i += 1 }; s }))), 3)
  }

  test("1M bodies calling k through Option.getOrElse and &&, on a 128 KB stack: ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => Option.empty[Int].getOrElse(k(x + 1)))))))
    assertEquals(a, n)
    assertEquals(s, 0L)
    val (a2, s2) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => if x >= 0 && k(x + 1) > 0 then x + 1 else 0)))))
    // the body answers x + 1, not k's answer: the run answers the outermost body's, as the strict road does
    assertEquals(a2, 1)
    assertEquals(s2, 0L)
  }

  test("Option, Either, && and || with k keep their meaning: by-name stays lazy, the receiver once, multi-shot") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option.empty[Int].getOrElse(k(4)))), 4)
    // a by-name default is NOT evaluated when the option is defined: k never called
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option(9).getOrElse(k(4)))), 9)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option(2).map(x => k(x) * 10).getOrElse(0))), 20)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option(2).flatMap(x => Option(k(x))).getOrElse(0))), 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option.empty[Int].fold(k(1))(x => k(x) + 100))), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option(5).fold(k(1))(x => k(x) + 100))), 105)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Option.empty[Int].orElse(Option(k(3))).getOrElse(0))), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Left("e"): Either[String, Int]).fold(_ => k(7), r => r))), 7)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Right(6): Either[String, Int]).fold(_ => 0, r => k(r)))), 6)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Left("e"): Either[String, Int]).getOrElse(k(8)))), 8)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if false && k(1) > 0 then 1 else 2)), 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if true || k(1) > 0 then 1 else 2)), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if true && k(1) > 0 then 1 else 2)), 1)
    // the receiver is evaluated once
    var built = 0
    def opt(): Option[Int] = { built += 1; Some(1) }
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => opt().map(x => k(x)).getOrElse(0))), 1)
    assertEquals(built, 1)
    // multi-shot across the rewritten match
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => Option(z).map(x => k(x)).getOrElse(0)))), 3)
  }

  test("1M bodies calling k inside Seq.flatMap and exists, on a 128 KB stack: ZERO switches") {
    val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => List(x).flatMap(y => List(k(y + 1))).sum)))))
    assertEquals(a, n)
    assertEquals(s, 0L)
    val (a2, s2) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(x =>
      Cont.shift[Int, Int, Int](k => if List(x).exists(y => k(y + 1) > 0) then x + 1 else 0)))))
    assertEquals(a2, 1)
    assertEquals(s2, 0L)
  }

  test("flatMap, exists, forall, find, foldRight with k keep their meaning: early stop, order, types, multi-shot") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2).flatMap(x => List(k(x), k(x) * 10)).sum)), 33)
    val v: Vector[Int] = Cont.reset(Cont.shift[Int, Vector[Int], Vector[Int]](k => Vector(1, 2).flatMap(x => k(x))).map(x => Vector(x, x)))
    assertEquals(v, Vector(1, 1, 2, 2))
    // exists stops at the first true: k is not called for the elements after it
    var calls = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if List(1, 2, 3).exists(x => { calls += 1; k(x) > 1 }) then 1 else 0)), 1)
    assertEquals(calls, 2)
    calls = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if List(1, 2, 3).forall(x => { calls += 1; k(x) < 2 }) then 1 else 0)), 0)
    assertEquals(calls, 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2, 3).find(x => k(x) == 2).getOrElse(0))), 2)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2, 3).find(x => k(x) == 9).getOrElse(0))), 0)
    // foldRight from the right: the last element first
    val log = collection.mutable.ArrayBuffer.empty[Int]
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => List(1, 2, 3).foldRight(0)((x, acc) => { log += x; acc * 10 + k(x) }))), 321)
    assertEquals(log.toList, List(3, 2, 1))
    // multi-shot across the traversal
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => List(z).flatMap(x => List(k(x))).sum))), 3)
  }

  test("1M bodies calling k through inline helpers, on a 128 KB stack: ZERO switches") {
    import ContInlineHelpers.*
    for (label, body) <- List[(String, Int => Int /> Int)](
        "k passed to an inline helper" -> (x => Cont.shift[Int, Int, Int](k => applyTo(k, x + 1))),
        "k's answer passed to an inline helper" -> (x => Cont.shift[Int, Int, Int](k => plusOne(k(x + 1)) - 1))) do
      val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(body))))
      assertEquals(a, n, label)
      assertEquals(s, 0L, label)
  }

  test("inline helpers keep their meaning: order and multi-shot") {
    import ContInlineHelpers.*
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => twice(k, 3))), 6)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => plusOne(k(4)))), 5)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => applyTo(k, z)))), 3)
  }

  test("1M bodies calling k through Either.map/flatMap, Try.getOrElse, Set and Map, on a 128 KB stack: ZERO switches") {
    import scala.util.{Failure, Try}
    val boom = new RuntimeException("boom")
    for (label, body) <- List[(String, Int => Int /> Int)](
        "Either.map" -> (x => Cont.shift[Int, Int, Int](k => (Right(x): Either[String, Int]).map(y => k(y + 1)).getOrElse(0))),
        "Either.flatMap" -> (x => Cont.shift[Int, Int, Int](k =>
          (Right(x): Either[String, Int]).flatMap(y => Right(k(y + 1))).getOrElse(0))),
        "Try.getOrElse" -> (x => Cont.shift[Int, Int, Int](k => (Failure(boom): Try[Int]).getOrElse(k(x + 1)))),
        "Set.map" -> (x => Cont.shift[Int, Int, Int](k => Set(x).map(y => k(y + 1)).sum)),
        "Set.foldLeft" -> (x => Cont.shift[Int, Int, Int](k => Set(x).foldLeft(0)((acc, y) => acc + k(y + 1)))),
        "Map.foldLeft" -> (x => Cont.shift[Int, Int, Int](k => Map(x -> 1).foldLeft(0) { case (acc, (key, _)) => acc + k(key + 1) }))) do
      val (a, s) = switchesDuring(SmallStack.run(128)(Cont.reset(row(n)(body))))
      assertEquals(a, n, label)
      assertEquals(s, 0L, label)
  }

  test("Either.map/flatMap, Try.getOrElse, Set, Map and LazyList with k keep their meaning") {
    import scala.util.{Failure, Success, Try}
    // a Left passes through map/flatMap: k never called
    var calls = 0
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Left("e"): Either[String, Int]).map(y => { calls += 1; k(y) }).getOrElse(5))), 5)
    assertEquals(calls, 0)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k =>
      (Right(2): Either[String, Int]).flatMap(y => if k(y) > 1 then Left("big") else Right(0)).fold(_.length, identity))), 3)
    // flatMap widens the left side: Either[Nothing, Int] into Either[String, Int]
    val e: Either[String, Int] = Cont.reset(Cont.shift[Int, Either[String, Int], Either[String, Int]](k =>
      (Right(1): Either[Nothing, Int]).flatMap(y => k(y))).map(x => Right(x + 1)))
    assertEquals(e, Right(2))
    // Try.getOrElse: a by-name default, evaluated only on a Failure
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Success(3): Try[Int]).getOrElse(k(1)))), 3)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (Failure(new Exception): Try[Int]).getOrElse(k(1)))), 1)
    // Set.map answers a Set: duplicates collapse
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Set(1, 3).map(x => k(x) % 2).size)), 1)
    val st: Set[Int] = Cont.reset(Cont.shift[Int, Set[Int], Set[Int]](k => Set(1, 2).flatMap(x => k(x))).map(x => Set(x, -x)))
    assertEquals(st, Set(1, -1, 2, -2))
    // Map: the iteration order the map itself has
    val log = collection.mutable.ArrayBuffer.empty[Int]
    val m = Map(3 -> "c", 1 -> "a", 2 -> "b")
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => { m.foreach((key, _) => log += k(key)); log.sum })), 6)
    assertEquals(log.toList, m.keys.toList)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if m.exists((key, _) => k(key) == 2) then 1 else 0)), 1)
    // exists / find over an INFINITE LazyList stop at the first that decides
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => if LazyList.from(1).exists(x => k(x) > 2) then 1 else 0)), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => LazyList.from(1).find(x => k(x) == 4).getOrElse(0))), 4)
    // multi-shot across a Set traversal
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k1 => k1(1) + k1(2)).flatMap(z =>
      Cont.shift[Int, Int, Int](k => Set(z).map(x => k(x)).sum))), 3)
  }

  test("bodies the transform cannot read stay opaque and keep their meaning") {
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => try k(1) catch { case _: Exception => 0 })), 1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => (() => k(1))())), 1)
    // OPAQUE ON PURPOSE (cont-stack-layer1-c): Try(...) and Try's map catch what the rest throws; with a lazy k
    // the rest would run outside them
    import scala.util.Try
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Try(k(1)).getOrElse(-1)).map(x => if x == 1 then sys.error("rest") else x)), -1)
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => Try(1).map(x => k(x)).getOrElse(-1)).map(x => if x == 1 then sys.error("rest") else x)), -1)
    // a mutable collection: the traversal would read it after the body could have changed it
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => collection.mutable.ArrayBuffer(1, 2).map(x => k(x)).sum)), 3)
    // LazyList.map is lazy: the elements are not forced by the transform
    assertEquals(Cont.reset(Cont.shift[Int, Int, Int](k => LazyList.from(1).map(x => k(x)).take(2).sum)), 3)
  }

  test("the Control[Cont] instance's shift keeps its meaning") {
    val c = summon[Control[Cont]]
    assertEquals(c.shift[Int, Int, Int](k => k(5)) / identity, 5)
  }
