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

  test("staged: one inline program, both carriers, no dispatch") {
    inline def prog[M[_, _, _]]: M[Int, Int, Int] =
      val C = staged[M]
      C.flatMap(C.pure(1))(x => C.shift((k: Int => Int) => k(x + 1) * 10))
    assertEquals(reset(prog[Cont]), 20)
    assertEquals(prog[Func](identity), 20)
  }

  test("the diagonal of a ParaMonad is an ordinary Monad") {
    def sum[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(x + _))
    assertEquals(reset(sum[[A] =>> A /> Int](Cont.Pure(1), Cont.Pure(2))), 3)
  }

}
