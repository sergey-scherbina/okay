package okay2

class TestCont extends munit.FunSuite {

  test("shift and reset: answer-type modification") {
    val c: Cont[Int, String, String] =
      shift[Int, String, String](k => k(20) + "!").flatMap(x => Cont.Pure[Int, String](x * 2))
    assertEquals(c / (x => s"got $x"), "got 40!")
  }

  test("stack safety: a 1M flatMap chain, left-nested") {
    val n = 1000000
    val m = (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int) { (m, _) =>
      m.flatMap(x => Cont.Pure[Int, Int](x + 1))
    }
    assertEquals(reset(m), n)
  }

  test("tagless Control: Cont and Func agree") {
    def prog[M[_, _, _]](implicit C: Control[M]): M[Int, Int, Int] =
      C.flatMap(C.pure[Int, Int](1))(x => C.shift[Int, Int, Int](k => k(x + 1) * 10))
    def check[M[_, _, _]](implicit C: Control[M]): Int = C.reset(prog[M])
    assertEquals(check[Cont.Rep], 20)
    assertEquals(check[Func], 20)
  }

  test("absorption: the FIRST bind enters the leaf, the second is a node") {
    // `Cont` is an opaque facade over `Free[Shift, A]`, so from here
    // the nodes are reached by a class test on `Any`
    val s = shift[Int, Int, Int](k => k(0))
    def succ(x: Int): Int /> Int = Cont.Pure(x + 1)
    def isOp(c: Any) = c match { case Free.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Free.Bind(_, _) => true; case _ => false }

    assert(isOp(s), "a bare shift is a leaf")
    assert(isOp(s.flatMap(succ)), "the first bind is absorbed into the leaf")
    assert(isBind(s.flatMap(succ).flatMap(succ)), "the second bind is a node")
    assert(isBind(Cont.Pure[Int, Int](0).flatMap(succ)), "a Pure receiver never absorbs")
    assertEquals(reset(s.flatMap(succ).flatMap(succ)), 2)
  }

  test("absorption is bounded: a leading shift, then 1M binds, stack-safe") {
    val n = 1000000
    val m = (1 to n).foldLeft(shift[Int, Int, Int](k => k(0))) { (m, _) =>
      m.flatMap(x => Cont.Pure[Int, Int](x + 1))
    }
    assertEquals(reset(m), n)
  }

  test("absorption is per leaf, not per program") {
    def leaf(i: Int) = shift[Int, Int, Int](k => k(i)).flatMap(x => Cont.Pure[Int, Int](x * 2))
    def isOp(c: Any) = c match { case Free.Inject(_) => true; case _ => false }

    assert(isOp(leaf(1)) && isOp(leaf(2)), "each leaf absorbed its own bind")
    val joined = leaf(1).flatMap(x => leaf(2).map(_ + x))
    assert(joined match { case Free.Bind(a, _) => isOp(a); case _ => false },
           "joining is a node over the still-absorbed left leaf")
    assertEquals(reset(joined), 6)
  }

  test("defer: mutual tail recursion across two functions, stack-safe") {
    def isEven(n: Int): Boolean /> Boolean =
      if (n == 0) Cont.Pure(true) else Cont.defer[Boolean, Boolean, Boolean, Boolean, Boolean](() => isOdd(n - 1))(Cont.Pure[Boolean, Boolean])
    def isOdd(n: Int): Boolean /> Boolean =
      if (n == 0) Cont.Pure(false) else Cont.defer[Boolean, Boolean, Boolean, Boolean, Boolean](() => isEven(n - 1))(Cont.Pure[Boolean, Boolean])
    assert(reset(isEven(1000000)))
    assert(!reset(isOdd(1000000)))
  }

  test("a shift that does not resume aborts; one that resumes twice is multi-shot") {
    val abort: Int /> Int = shift[Int, Int, Int](_ => -1)
    assertEquals(reset(abort.flatMap(x => Cont.Pure[Int, Int](x + 1))), -1)
    val twice: Int /> Int = shift[Int, Int, Int](k => k(1) + k(10))
    assertEquals(reset(twice.flatMap(x => Cont.Pure[Int, Int](x * 2))), 22)
  }
}
