package okay.freer


import okay.std.*
import okay.std.given
import okay.{Control, Func, Monad}
import okay.given

class TestCont extends munit.FunSuite {

  test("shift and reset") {
    extension [A1, A2](t: (() => A1, () => A2))
      inline def ? : (A1, A2) = (t._1(), t._2())

    inline def delay[A, B](a: => A) =
      Cps.shift((k: A => B) => () => k(a))

    val example1 = Cps.reset(for {
      _ <- delay(println("Hello,"))
      _ <- delay(println("World!"))
      _ <- delay(println("Goodbye!"))
    } yield ())

    val example2 = Cps.reset(for {
      _ <- delay(println("1"))
      _ <- delay(println("2"))
      _ <- delay(println("3"))
      _ <- delay(println("4"))
    } yield ())

    (example1, example2).?.?.?
  }

  test("stack safety: a 1M flatMap chain, left-nested") {
    val n = 1000000
    val m = (1 to n).foldLeft(Cps.Pure(0): Int />> Int): (m, _) =>
      m.flatMap(x => Cps.Pure(x + 1))
    assertEquals(Cps.reset(m), n)
  }

  test("tagless Control: Cps and Func agree") {
    def prog[M[_, _, _]](using C: Control[M]): M[Int, Int, Int] =
      C.pure(1).flatMap(x => C.shift((k: Int => Int) => k(x + 1) * 10))
    def check[M[_, _, _]](using C: Control[M]): Int = C.reset(prog[M])
    assertEquals(check[Cps], 20)
    assertEquals(check[Func], 20)
  }

  test("no absorption: a bind on a leaf is a node — the frame machine pushes it (cont-step-on-frames)") {
    // until cont-step-on-frames a leaf ABSORBED its first bind (the old
    // runner's `Leaf.Absorbed`, a Fib-lane optimisation for `step`); on
    // the frame machine a bind is always a node and the machine pushes
    // it as a frame, so there is nothing to absorb. The leaf is built
    // with `shiftLeaf`: `shift` would rewrite this tail-shaped body to a
    // Return at compile time (ContMacro). `Cps` is an opaque facade, so
    // the nodes are reached by a class test on `Any`.
    val s = Cps.shiftLeaf[Int, Int, Int](k => k(0))
    def succ(x: Int): Int />> Int = Cps.Pure(x + 1)
    def isOp(c: Any) = c match { case Freer.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Freer.Bind(_, _) => true; case _ => false }

    assert(isOp(s), "a bare shift is a leaf")
    assert(isBind(s.flatMap(succ)), "a bind on a leaf is a node")
    assert(isBind(s.flatMap(succ).flatMap(succ)), "and so is the next")
    assert(isBind(Cps.Pure(0).flatMap(succ)), "a Pure receiver is a node too")
    assertEquals(Cps.reset(s.flatMap(succ).flatMap(succ)), 2)
  }

  test("a leading shift, then 1M binds, stack-safe") {
    // 1M binds after one leaf: the machine pushes each as a frame and
    // pops it in its loop; no bind nests a closure call.
    val n = 1000000
    val m = (1 to n).foldLeft(Cps.shiftLeaf[Int, Int, Int](k => k(0))): (m, _) =>
      m.flatMap(x => Cps.Pure(x + 1))
    assertEquals(Cps.reset(m), n)
  }

  test("leaves joined by binds: every bind a node, the answer unchanged") {
    def leaf(i: Int) = Cps.shiftLeaf[Int, Int, Int](k => k(i)).flatMap(x => Cps.Pure(x * 2))
    def isOp(c: Any) = c match { case Freer.Inject(_) => true; case _ => false }
    def isBind(c: Any) = c match { case Freer.Bind(_, _) => true; case _ => false }

    assert(isBind(leaf(1)) && isBind(leaf(2)), "a leaf's own bind is a node")
    val joined = leaf(1).flatMap(x => leaf(2).map(_ + x))
    assert(joined match { case Freer.Bind(a, _) => isBind(a) && (a match { case Freer.Bind(l, _) => isOp(l); case _ => false }); case _ => false },
           "joining is a node over the left leaf's node")
    assertEquals(Cps.reset(joined), 6)
  }

  test("staged: one inline program, both carriers, no dispatch") {
    inline def prog[M[_, _, _]]: M[Int, Int, Int] =
      val C = Control[M]
      C.flatMap(C.pure(1))(x => C.shift((k: Int => Int) => k(x + 1) * 10))
    assertEquals(Cps.reset(prog[Cps]), 20)
    assertEquals(prog[Func](identity), 20)
  }

  test("defer: mutual tail recursion across two functions, stack-safe") {
    def isEven(n: Int): Boolean />> Boolean =
      if n == 0 then Cps.Pure(true) else Cps.defer(() => isOdd(n - 1))(Cps.Pure)
    def isOdd(n: Int): Boolean />> Boolean =
      if n == 0 then Cps.Pure(false) else Cps.defer(() => isEven(n - 1))(Cps.Pure)
    assert(Cps.reset(isEven(1000000)))
    assert(!Cps.reset(isOdd(1000000)))
  }

  test("the diagonal of a ParaMonad is an ordinary Monad") {
    def sum[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(x + _))
    assertEquals(Cps.reset(sum[[A] =>> A />> Int](Cps.Pure(1), Cps.Pure(2))), 3)
  }

  // cont-run-prompt: every run is a frame of its own, and a leaf goes to the nearest one
  test("a shift answers its own reset, not an outer one: lazy and strict leaves") {
    val viaMacro = Cps.reset(for
      a <- Cps.shift((k: Int => Int) => k(1) + 100)
      b = Cps.reset(for x <- Cps.shift((k: Int => Int) => k(10) * 2) yield x + 1)
    yield a + b)
    val viaLeaf = Cps.reset(for
      a <- Cps.shiftLeaf((k: Int => Int) => k(1) + 100)
      b = Cps.reset(for x <- Cps.shiftLeaf((k: Int => Int) => k(10) * 2) yield x + 1)
    yield a + b)
    // inner: (10 + 1) * 2 = 22; outer: (1 + 22) + 100
    assertEquals((viaMacro, viaLeaf), (123, 123))
  }

  test("a k that left its run answers to that run's root, called from inside another run") {
    var saved: Int => Int = identity
    val first = Cps.reset(for x <- Cps.shiftLeaf((k: Int => Int) => { saved = k; k(1) }) yield x * 10)
    val second = Cps.reset(for y <- Cps.shiftLeaf((k: Int => Int) => k(saved(2))) yield y + 1)
    // saved(2) is the first run's rest, 2 * 10, under the first run's root; the second adds its own 1
    assertEquals((first, second), (10, 21))
  }

  // cont-shift-op's refuting case (specs/cont-shift-op.md, 2026-10-01): a lazy k that calls k twice, nested.
  // A frame per run keeps D-F's boundary in the machine: k's own copy of the frame delimits every call of k
  test("k(x + 1) + k(x + 1) chained d deep, lazy and strict, agree with the closure instance") {
    def lazily(d: Int): Int />> Int =
      (1 to d).foldLeft(Cps.Pure[Int, Int](0): Int />> Int)((m, _) => m.flatMap(x => Cps.shift((k: Int => Int) => k(x + 1) + k(x + 1))))
    def strictly(d: Int): Int />> Int =
      (1 to d).foldLeft(Cps.Pure[Int, Int](0): Int />> Int)((m, _) => m.flatMap(x => Cps.shiftLeaf((k: Int => Int) => k(x + 1) + k(x + 1))))
    def closures(d: Int): Func[Int, Int, Int] =
      (1 to d).foldLeft[Func[Int, Int, Int]](k => k(0))((m, _) => k => m(x => k(x + 1) + k(x + 1)))
    for d <- 0 to 6 do
      val want = closures(d)(identity)
      assertEquals((Cps.reset(lazily(d)), Cps.reset(strictly(d))), (want, want), s"d = $d")
  }

  // cont-atm: A ! F are plain values to Cps — an answer may be a program, which the effect's own handler runs
  test("A ! F as answers with answer-type modification: Int ! Reader → String ! Reader, run by Reader.run") {
    type Rd = Reader % Int
    val c: Cps[Int, Int ! Rd, String ! Rd] =
      Cps.shift[Int, Int ! Rd, String ! Rd](k => Reader.ask[Int].flatMap(e => k(e)).map(n => s"n=$n"))
    val prog: String ! Rd = Cps.run(c.map(_ + 1))(x => Freer.Return(x))
    // the body asks 5; k(5) is 5 + 1 as a program; the body answers it as text
    assertEquals(!.run(Reader.run[Int, String, Pure](5)(prog)), "n=6")
  }

}
