package okay2

import Generate._

/** generators from delimited control, and the producer effect — the
 * Scala 3 core's TestStd producer tests and TestFoldUntil's Producer ones */
class TestGenerate extends munit.FunSuite {

  test("one generator, two semantics: fibs as a LazyList and as a Producer agree") {
    assertEquals(fibs[Long, LazyList].take(10).toList, List(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L, 34L))
    assertEquals(Produce.stream.iterator(fibs[Long, Producer]).take(10).toList, fibs[Long, LazyList].take(10).toList)
    assertEquals(nats[Int, LazyList].take(4).toList, List(0, 1, 2, 3))
    assertEquals(generateLazy(1)(_ * 10)(_ + 1).take(3).toList, List(10, 20, 30))
  }

  test("a producer is a stream: uncons steps it, toLazyList unfolds it") {
    val Some((h, t)) = Effects.run(Produce.stream.uncons(fibs[Long, Producer])): @unchecked
    assertEquals(h, 0L)
    assertEquals(Effects.run(Produce.stream.uncons(t)).map(_._1), Some(1L))
    val Some((x, end)) = Effects.run(Produce.stream.uncons(produce(42))): @unchecked
    assertEquals(x, 42)
    assertEquals(Effects.run(Produce.stream.uncons(end)), None)
  }

  test("an infinite LazyList generator is lazy: nothing past the read is built") {
    var built = 0
    val xs = generate[Int, Int, LazyList](0)(i => { built += 1; i })(_ + 1)
    assertEquals(xs.take(3).toList, List(0, 1, 2))
    assert(built <= 4, s"built $built elements for 3 read")
  }

  /** 1, tell "after 1", 2, tell "after 2", ... — a G operation after every production */
  type P = Produce + Writer[String]
  private def counted(n: Int): Int ! P = {
    def go(i: Int): Int ! P =
      if (i > n) pure[P, Int](-1)
      else produce(i).flatMap(_ => Writer.tell(s"after $i")).flatMap(_ => go(i + 1))
    go(1)
  }

  private def runP[R](p: R ! Writer[String]): (Seq[String], R) = Effects.run(Writer.run(p))

  test("Producer.foldUntil agrees with Stream.foldUntil over the prefix, on every instance") {
    val xs = (1 to 20).toList
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit = {
      val expected = Stream.foldUntil[List, Pure, Int, S, X](xs)(fo)
      assertEquals(runP(Producer.foldUntil[Int, S, X, Int, Writer[String]](counted(20))(fo))._2, expected, name)
    }
    check(FoldUntil.take[Int](3), "take(3)")
    check(FoldUntil.take[Int](0), "take(0)")
    check(FoldUntil.take[Int](100), "take(100)")
    check(FoldUntil.find[Int](_ > 7), "find")
    check(FoldUntil.find[Int](_ > 70), "find-none")
    check(FoldUntil.exists[Int](_ == 11), "exists")
    check(FoldUntil.forall[Int](_ < 5), "forall")
    check(FoldUntil.headOption[Int], "headOption")
  }

  test("Producer.foldUntil performs the G op before the stop and not the one after it") {
    val (told, got) = runP(Producer.foldUntil[Int, Vector[Int], Vector[Int], Int, Writer[String]](counted(1000))(FoldUntil.take[Int](3)))
    assertEquals(got, Vector(1, 2, 3))
    assertEquals(told, Seq("after 1", "after 2"))
    val (told0, got0) = runP(Producer.foldUntil[Int, Vector[Int], Vector[Int], Int, Writer[String]](counted(1000))(FoldUntil.take[Int](0)))
    assertEquals(got0, Vector.empty[Int])
    assertEquals(told0, Seq.empty[String])
  }

  test("Producer.foldUntil and fold are tail-recursive across productions: 100 000") {
    val n = 100000
    val (_, got) = runP(Producer.foldUntil[Int, Long, Long, Int, Writer[String]](counted(n))(
      FoldUntil[Int, Long, Long](0L)(_ + _)(_ => false)(identity)))
    assertEquals(got, n.toLong * (n + 1) / 2)
    val ((sum, a)) = Effects.run(Producer.fold[Int, Long, Unit, Pure]((1 to n).foldLeft(pure[Produce, Unit](()))((m, i) => m.flatMap(_ => produce(i).map(_ => ()))))(0L)(_ + _))
    assertEquals(sum, n.toLong * (n + 1) / 2)
    assertEquals(a, ())
  }

  test("each, concat, and the in-row stream") {
    val seen = scala.collection.mutable.ListBuffer.empty[Int]
    val p: Int ! P = counted(3)
    val (told, end) = runP(Producer.each[Int, Int, Writer[String]](p)(seen += _))
    assertEquals(seen.toList, List(1, 2, 3))
    assertEquals(told, Seq("after 1", "after 2", "after 3"))
    assertEquals(end, -1)
    val chunks: IndexedSeq[Int] ! Produce = produce(IndexedSeq(1, 2)).flatMap(_ => produce(IndexedSeq(3)))
    assertEquals(Effects.run(Producer.concat[Int, Pure](chunks)), Vector(1, 2, 3))
    val first = runP(Produce.streamIn[Writer[String]].uncons(counted(3)))._2
    assertEquals(first.map(_._1), Some(1))
  }

  test("each is stack-safe over a long producer: a production is the loop's own tail call") {
    val n = 200000
    var sum = 0L
    val p = (1 to n).foldLeft(pure[Produce, Unit](()))((m, i) => m.flatMap(_ => produce(i).map(_ => ())))
    Effects.run(Producer.each[Int, Unit, Pure](p)(sum += _))
    assertEquals(sum, n.toLong * (n + 1) / 2)
  }
}
