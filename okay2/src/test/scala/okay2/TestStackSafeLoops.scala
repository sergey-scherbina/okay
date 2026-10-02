package okay2

/** specs/eager-carrier-depth.md in okay2: `TailRecM` from the carrier, and `foldMap` through it — a million
 * iterations with no overflow, on every platform (the Scala 3 core's TestStackSafeLoops) */
class TestStackSafeLoops extends munit.FunSuite {
  import Eager._

  val n = 1000000

  def countTo[F[_]](k: Int)(wrap: Either[Int, Int] => F[Either[Int, Int]]): Int => F[Either[Int, Int]] =
    i => wrap(if (i < k) Left(i + 1) else Right(i))

  test("the carriers' own loops: Option, Either, LazyList, a program — a million iterations") {
    assertEquals(TailRecM[Option].tailRecM(0)(countTo[Option](n)(Some(_))), Some(n))
    type E[X] = Either[String, X]
    assertEquals(TailRecM[E].tailRecM(0)(countTo[E](n)(Right(_))), Right(n))
    assertEquals(TailRecM[LazyList].tailRecM(0)(countTo[LazyList](n)(LazyList(_))).toList, List(n))
    type P[X] = Free[Pure, X]
    assertEquals(!.run(TailRecM[P].tailRecM(0)(countTo[P](n)(pure[Pure, Either[Int, Int]](_)))), n)
  }

  test("a stop answers at once: None, Left") {
    assertEquals(TailRecM[Option].tailRecM(0)(i => if (i < 5) Some(Left(i + 1)) else None), None)
    type E[X] = Either[String, X]
    assertEquals(TailRecM[E].tailRecM[Int, Int](0)(i => if (i < 5) Right(Left(i + 1)) else Left("at 5")), Left("at 5"))
  }

  test("LazyList: every branch, depth first, lazily") {
    val tree = TailRecM[LazyList].tailRecM[Int, Int](1)(i => if (i < 4) LazyList(Left(i * 2), Left(i * 2 + 1)) else LazyList(Right(i)))
    assertEquals(tree.toList, List(4, 5, 6, 7))
  }

  test("a monad with no TailRecM has no tailRecM: a compile error naming it") {
    val errs = compileErrors("TailRecM[List]")
    assert(errs.contains("no TailRecM"), errs)
  }

  val asOption: Static.To[Produce, Option] = new Static.To[Produce, Option] {
    def apply[X](op: Produce.Emit[X]): Option[X] = Some(op.a)
  }

  test("foldMap into Option: a million operations, left-nested and non-tail") {
    val left: Int ! Produce = (1 to n).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))
    assertEquals(Effects.free.foldMap(left)(asOption), Some(n))
    def nonTail(k: Int): Int ! Produce =
      if (k == 0) pure[Produce, Int](0) else produce(1).flatMap(x => nonTail(k - 1).map(_ + x))
    assertEquals(Effects.free.foldMap(nonTail(n))(asOption), Some(n))
  }

  test("foldMap: a None stops the fold; the eager encoding folds the same") {
    val stop: Static.To[Produce, Option] = new Static.To[Produce, Option] {
      def apply[X](op: Produce.Emit[X]): Option[X] = if (op.a == 3) None else Some(op.a)
    }
    var after = false
    val p: Int ! Produce = produce(3).flatMap(_ => { after = true; produce(4) })
    assertEquals(Effects.free.foldMap(p)(stop), None)
    assert(!after, "the fold went on past a None")
    val E = Effects[Eager.Rep]
    val eager = E.flatMap(E.perform[Produce, Int](Produce.Emit(1)))(x => E.map(E.perform[Produce, Int](Produce.Emit(x + 1)))(_ + x))
    assertEquals(E.foldMap(eager)(asOption), Some(3))
  }
}
