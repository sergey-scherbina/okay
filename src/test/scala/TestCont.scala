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

  test("absorption: the FIRST bind enters the leaf, the second is a node") {
    // the structural half of the one-step rule (specs/freer-base.md).
    // The leaf is built with `shiftLeaf`: `shift` would rewrite this
    // tail-shaped body to a Return at compile time (ContMacro).
    // `Cont` is an opaque facade over `Free[Shift, A]`, so from here
    // the nodes are reached by a class test on `Any` — which is all a
    // structural probe needs, and all the facade allows.
    val s = Cont.shiftLeaf[Int, Int, Int](k => k(0))
    def succ(x: Int): Int /> Int = Cont.Pure(x + 1)
    def isOp(c: Any) = c match { case Free.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Free.Bind(_, _) => true; case _ => false }

    assert(isOp(s), "a bare shift is a leaf")
    assert(isOp(s.flatMap(succ)), "the first bind is absorbed into the leaf")
    assert(isBind(s.flatMap(succ).flatMap(succ)), "the second bind is a node")
    assert(isBind(Cont.Pure(0).flatMap(succ)), "a Pure receiver never absorbs")
    assertEquals(reset(s.flatMap(succ).flatMap(succ)), 2)
  }

  test("absorption is bounded: a leading shift, then 1M binds, stack-safe") {
    // the behavioural half: if absorption were unbounded the run would
    // nest 1M closure calls. It stops after one, and the rest are Bind
    // nodes the tail-recursive `resume` rotates.
    val n = 1000000
    val m = (1 to n).foldLeft(Cont.shiftLeaf[Int, Int, Int](k => k(0))): (m, _) =>
      m.flatMap(x => Cont.Pure(x + 1))
    assertEquals(reset(m), n)
  }

  test("absorption is per leaf, not per program") {
    // each leaf absorbs its OWN first bind; joining two absorbed
    // leaves makes a node and leaves both absorptions intact
    def leaf(i: Int) = Cont.shiftLeaf[Int, Int, Int](k => k(i)).flatMap(x => Cont.Pure(x * 2))
    // a structural probe on Any: the facade is opaque from here
    def isOp(c: Any) = c match { case Free.Inject(_) => true; case _ => false }

    assert(isOp(leaf(1)) && isOp(leaf(2)), "each leaf absorbed its own bind")
    val joined = leaf(1).flatMap(x => leaf(2).map(_ + x))
    assert(joined match { case Free.Bind(a, _) => isOp(a); case _ => false },
           "joining is a node over the still-absorbed left leaf")
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
