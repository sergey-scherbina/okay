package okay2

import okay2.Produce.produce
import okay2.Stream.{StreamOps, FeedOps, FeedInOps}

/** the Stream typeclass and its carriers: List, LazyList, a writer
 * program, a writer program in G; the fold and the stopping fold */
class TestStream extends munit.FunSuite {

  def told(xs: Int*): Unit ! Writer[Int] =
    xs.foldLeft(pure[Writer[Int], Unit](()))((p, x) => p.flatMap(_ => Writer.tell(x)))

  test("List and LazyList are streams: uncons, toLazyList, the combinators") {
    assertEquals(List(1, 2, 3).uncons, Some((1, List(2, 3))))
    assertEquals(LazyList.from(1).map(_ * 2).take(3).toList, List(2, 4, 6))
    assertEquals(List(1, 2, 3, 4).filter(_ % 2 == 0).toList, List(2, 4))
    assertEquals(List(1, 2, 3).foldLeft(0)(_ + _), 6)
    assertEquals(List(1, 2, 3).exists(_ == 2), true)
    assertEquals(List.empty[Int].headOption, None)
  }

  test("a writer program is a stream of its told values, the answer forgotten") {
    val p = told(1, 2, 3)
    assertEquals(p.uncons.map(_._1), Some(1))
    assertEquals(p.toLazyList.toList, List(1, 2, 3))
    assertEquals(p.iterator.toList, List(1, 2, 3))
    assertEquals(told().toLazyList.toList, Nil)
    // the specialized iterator agrees with the uncons road on an infinite teller
    def nats(n: Int): Unit ! Writer[Int] = Writer.tell(n).flatMap(_ => nats(n + 1))
    assertEquals(nats(0).iterator.take(5).toList, nats(0).toLazyList.take(5).toList)
  }

  test("Stream.fold dispatches on the accumulator and agrees with a plain fold") {
    val xs = (1 to 100).toList
    assertEquals(Stream.fold(xs)(Fold.count), 100L)
    assertEquals(Stream.fold(xs)(Fold.sumInt), 5050)
    assertEquals(Stream.fold(xs)(Fold.sum[Int]), 5050)
    assertEquals(Stream.fold(xs)(Fold.exists[Int](_ == 7)), true)
    assertEquals(Stream.fold(xs)(Fold.forall[Int](_ < 50)), false)
    assertEquals(Stream.fold(xs)(Fold.last[Int]), Some(100))
    assertEquals(Stream.fold(xs)(Fold.first[Int]), Some(1))
  }

  test("Stream.foldUntil stops: an infinite LazyList is asked for exactly enough") {
    var pulled = 0
    val counted = LazyList.from(1).map(i => { pulled += 1; i })
    assertEquals(Stream.foldUntil(counted)(FoldUntil.take[Int](3)), Vector(1, 2, 3))
    assertEquals(pulled, 3)
    assertEquals(Stream.foldUntil(LazyList.from(1))(FoldUntil.find[Int](_ > 7)), Some(8))
    assertEquals(Stream.foldUntil(LazyList.from(1))(FoldUntil.exists[Int](_ == 11)), true)
    assertEquals(Stream.foldUntil(List(1, 2, 3))(FoldUntil.forall[Int](_ < 5)), true)
    assertEquals(Stream.foldUntil(List(1, 2, 3))(FoldUntil.until[Int, Int, Int](0)((s, a) => if (s + a > 2) Right(s) else Left(s + a))(identity)), 1)
    assertEquals(Stream.foldUntil(LazyList.from(1))(FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ > 50)(identity)), 55L)
  }

  test("a writer program's foldUntil never resumes the producer past the satisfying tell") {
    var told = 0
    def nums(n: Int): Unit ! Writer[Int] =
      (1 to n).foldRight(pure[Writer[Int], Unit](()))((i, r) => Writer.tell(i).flatMap(_ => { told += 1; r }))
    assertEquals(nums(50).foldUntil(FoldUntil.take[Int](3)), Vector(1, 2, 3))
    assertEquals(told, 3)
  }

  test("a writer program in G is a stream in G: the Handler runs the forwarded operations") {
    type Row = Writer[Int] + Produce
    var performed = 0
    def prog: Unit ! Row = for {
      _ <- Writer.tell(1).at[Row]
      x <- produce({ performed += 1; 5 }).at[Row]
      _ <- Writer.tell(x).at[Row]
      _ <- Writer.tell(99).at[Row]
    } yield ()
    assertEquals(prog.toLazyList.toList, List(1, 5, 99))
    assertEquals(prog.iterator.toVector, Vector(1, 5, 99))
    val (ws, _) = Writer.run[Int, Unit, Row](prog).runWith
    assertEquals(ws, Seq(1, 5, 99))
    assertEquals(prog.foldUntil(FoldUntil.take[Int](1)), Vector(1))
  }

  test("Writer.of: any stream as a writer program, and Pull reads one step at a time") {
    val p: Unit ! (Writer[Int] + Pure) = Writer.of[List, Pure, Int](List(1, 2, 3))
    assertEquals(Effects.run(Writer.run[Int, Unit, Writer[Int] + Pure](p))._1, Seq(1, 2, 3))
    val seen = List.newBuilder[Int]
    Effects.run(Pull.told(told(4, 5, 6)).loop(seen += _))
    assertEquals(seen.result(), List(4, 5, 6))
    val evens = Effects.run(Pull.of[List, Int, Pure](List(1, 2, 3, 4)).withFilter(_ % 2 == 0).step)
    assertEquals(evens.map(_._1), Some(2))
  }

  test("Writer.fold folds the told values by any Fold, forwarding the rest") {
    val p: Unit ! (Writer[Int] + Pure) = told(1, 2, 3).plus[Pure]
    assertEquals(Effects.run(Writer.fold[Int, Long, Unit, Writer[Int] + Pure](p)(Fold.count)), (3L, ()))
    assertEquals(Effects.run(Writer.fold[Int, Int, Unit, Writer[Int] + Pure](p)(Fold.sumInt)), (6, ()))
    assertEquals(Effects.run(Writer.fold[Int, Seq[Int], Unit, Writer[Int] + Pure](p)(Fold.collect[Int])), (Seq(1, 2, 3), ()))
  }
}
