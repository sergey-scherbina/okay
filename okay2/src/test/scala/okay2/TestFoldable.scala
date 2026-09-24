package okay2

import Generate._

/** Foldable (the push side) and the rest of Stream's combinators — the
 * Scala 3 core's TestStd / TestFoldUntil lines for them */
class TestFoldable extends munit.FunSuite {

  test("Foldable: run one Fold over any container") {
    assertEquals(List(1, 2, 3).foldTo[Seq[Int]], Seq(1, 2, 3))
    assertEquals(Iterator(1, 2, 3).foldTo[Int], 6)
    assertEquals(Vector(1L, 5L, 3L).foldTo(Fold.maxLong), 5L)
    assertEquals(List(2.0, -1.0).foldTo(Fold.minDouble), -1.0)
  }

  test("the four unboxed shapes agree with the generic fold on every walk, and stop") {
    val xs = (1 to 100).toList
    val sumL = FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ > 50)(identity)
    val sumI = FoldUntil.int[Int, Int](0)((s, a) => s + a)(_ > 50)(identity)
    val sumD = FoldUntil.double[Int, Double](0.0)((s, a) => s + a)(_ > 50)(identity)
    val anyB = FoldUntil.boolean[Int, Boolean](false)((s, a) => s || a == 7)(identity)(identity)
    val generic = FoldUntil[Int, Long, Long](0L)((s, a) => s + a)(_ > 50)(identity)
    for ((name, fo) <- List(("long", sumL), ("generic", generic)))
      assertEquals(xs.foldUntilTo(fo), 55L, s"List $name")
    assertEquals(xs.foldUntilTo(sumI), 55)
    assertEquals(xs.foldUntilTo(sumD), 55.0)
    assertEquals(xs.foldUntilTo(anyB), true)
    assertEquals(List(1, 2, 3).foldUntilTo(anyB), false)
    // the stop holds on the specialised arm: an infinite iterator ends
    assertEquals(Iterator.from(1).foldUntilTo(sumL), 55L)
    assertEquals(xs.foldUntilTo(FoldUntil.exists[Int](_ == 42)), true)
    assertEquals(xs.foldUntilTo(FoldUntil.forall[Int](_ < 42)), false)
  }

  test("an Iterator folds on in pieces: the walk stops right after the satisfying element") {
    val it = Iterator.from(1)
    assertEquals(it.foldUntilTo(FoldUntil.take[Int](3)), Vector(1, 2, 3))
    assertEquals(it.foldUntilTo(FoldUntil.take[Int](2)), Vector(4, 5))
  }

  test("zip and ++ across carriers; Stream.map and Stream.flatMap by name") {
    import Stream._
    // a producer on the left: a collection's own zip/++ would win there
    def ints(rest: List[Int]): Producer[Int] = rest match {
      case Nil => pure(0)
      case x :: t => produce(x).flatMap(_ => ints(t))
    }
    assertEquals(ints(List(1, 2, 3)).zip(nats[Int, Producer]).toList, List((1, 0), (2, 1), (3, 2)))
    assertEquals((ints(List(1, 2)) ++ Vector(3, 4)).toList, List(1, 2, 3, 4))
    assertEquals(Stream.map(fibs[Long, Producer])(_ * 2).take(5).toList, List(0L, 2L, 2L, 4L, 6L))
    assertEquals(Stream.flatMap(List(1, 2, 3))((n: Int) => List.fill(n)(n)).toList, List(1, 2, 2, 3, 3, 3))
    // zip is lazy on both sides: an infinite producer against a finite list ends
    assertEquals(nats[Int, Producer].zip(List("a", "b")).toList, List((0, "a"), (1, "b")))
  }

  test("LazyList is a MonadPlus: empty is failure, append concatenates, lazily") {
    val M = MonadPlus[LazyList]
    assertEquals(M.flatMap(LazyList(1, 2))(n => M.append(M.pure(n), M.pure(n * 10))).toList, List(1, 10, 2, 20))
    assertEquals(M.mplus(M.mzero[Int], LazyList(7)).toList, List(7))
    val infinite = M.append(LazyList(1), LazyList.from(2))
    assertEquals(infinite.take(3).toList, List(1, 2, 3))
    assertEquals(Monad[LazyList].fmap(LazyList(1, 2), (n: Int) => n + 1).toList, List(2, 3))
  }
}
