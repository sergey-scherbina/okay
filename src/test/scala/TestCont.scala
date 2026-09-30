package okay

class TestCont extends munit.FunSuite {

  test("shift and reset") {
    extension [A1, A2](t: (() => A1, () => A2))
      inline def ? : (A1, A2) = (t._1(), t._2())

    inline def delay[A, B](a: => A) =
      shift((k: A => B) => () => k(a))

    val example1 = reset(for {
      _ <- delay(println("Hello,"))
      _ <- delay(println("World!"))
      _ <- delay(println("Goodbye!"))
    } yield ())

    val example2 = reset(for {
      _ <- delay(println("1"))
      _ <- delay(println("2"))
      _ <- delay(println("3"))
      _ <- delay(println("4"))
    } yield ())

    (example1, example2).?.?.?
  }

  test("stack safety: a 1M flatMap chain, left-nested") {
    val n = 1000000
    val m = (1 to n).foldLeft(Cont.Pure(0): Int /> Int): (m, _) =>
      m.flatMap(x => Cont.Pure(x + 1))
    assertEquals(reset(m), n)
  }

  test("tagless Control: Cont and Func agree") {
    def prog[M[_, _, _]](using C: Control[M]): M[Int, Int, Int] =
      C.pure(1).flatMap(x => C.shift((k: Int => Int) => k(x + 1) * 10))
    def check[M[_, _, _]](using C: Control[M]): Int = C.reset(prog[M])
    assertEquals(check[Cont], 20)
    assertEquals(check[Func], 20)
  }

  test("no absorption: a bind on a leaf is a node — the frame machine pushes it (cont-step-on-frames)") {
    // until cont-step-on-frames a leaf ABSORBED its first bind (the old
    // runner's `Leaf.Absorbed`, a Fib-lane optimisation for `step`); on
    // the frame machine a bind is always a node and the machine pushes
    // it as a frame, so there is nothing to absorb. The leaf is built
    // with `shiftLeaf`: `shift` would rewrite this tail-shaped body to a
    // Return at compile time (ContMacro). `Cont` is an opaque facade, so
    // the nodes are reached by a class test on `Any`.
    val s = Cont.shiftLeaf[Int, Int, Int](k => k(0))
    def succ(x: Int): Int /> Int = Cont.Pure(x + 1)
    def isOp(c: Any) = c match { case Freer.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Freer.Bind(_, _) => true; case _ => false }

    assert(isOp(s), "a bare shift is a leaf")
    assert(isBind(s.flatMap(succ)), "a bind on a leaf is a node")
    assert(isBind(s.flatMap(succ).flatMap(succ)), "and so is the next")
    assert(isBind(Cont.Pure(0).flatMap(succ)), "a Pure receiver is a node too")
    assertEquals(reset(s.flatMap(succ).flatMap(succ)), 2)
  }

  test("a leading shift, then 1M binds, stack-safe") {
    // 1M binds after one leaf: the machine pushes each as a frame and
    // pops it in its loop; no bind nests a closure call.
    val n = 1000000
    val m = (1 to n).foldLeft(Cont.shiftLeaf[Int, Int, Int](k => k(0))): (m, _) =>
      m.flatMap(x => Cont.Pure(x + 1))
    assertEquals(reset(m), n)
  }

  test("leaves joined by binds: every bind a node, the answer unchanged") {
    def leaf(i: Int) = Cont.shiftLeaf[Int, Int, Int](k => k(i)).flatMap(x => Cont.Pure(x * 2))
    def isOp(c: Any) = c match { case Freer.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Freer.Bind(_, _) => true; case _ => false }

    assert(isBind(leaf(1)) && isBind(leaf(2)), "a leaf's own bind is a node")
    val joined = leaf(1).flatMap(x => leaf(2).map(_ + x))
    assert(joined match { case Freer.Bind(a, _) => isBind(a) && (a match { case Freer.Bind(l, _) => isOp(l); case _ => false }); case _ => false },
           "joining is a node over the left leaf's node")
    assertEquals(reset(joined), 6)
  }

  test("staged: one inline program, both carriers, no dispatch") {
    inline def prog[M[_, _, _]]: M[Int, Int, Int] =
      val C = Control[M]
      C.flatMap(C.pure(1))(x => C.shift((k: Int => Int) => k(x + 1) * 10))
    assertEquals(reset(prog[Cont]), 20)
    assertEquals(prog[Func](identity), 20)
  }

  test("defer: mutual tail recursion across two functions, stack-safe") {
    def isEven(n: Int): Boolean /> Boolean =
      if n == 0 then Cont.Pure(true) else Cont.defer(() => isOdd(n - 1))(Cont.Pure)
    def isOdd(n: Int): Boolean /> Boolean =
      if n == 0 then Cont.Pure(false) else Cont.defer(() => isEven(n - 1))(Cont.Pure)
    assert(reset(isEven(1000000)))
    assert(!reset(isOdd(1000000)))
  }

  test("the diagonal of a ParaMonad is an ordinary Monad") {
    def sum[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(x + _))
    assertEquals(reset(sum[[A] =>> A /> Int](Cont.Pure(1), Cont.Pure(2))), 3)
  }

}
