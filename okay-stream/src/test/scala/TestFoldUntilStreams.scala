package okay

import scala.collection.immutable.ArraySeq

/**
 * specs/fold-until.md on the stream carriers: `Chunks.foldUntil`,
 * `Writer.foldUntil`, `Source.runFoldUntil` agree with the pure road,
 * and each STOPS — no chunk pulled after the satisfying one, no Async
 * operation performed after the stop, and the producer not resumed
 * past the satisfying tell.
 */
class TestFoldUntilStreams extends munit.FunSuite:

  type R = Writer % Int + Async

  /** 1, perform, 2, perform, 3, perform, ... — an Async op after every tell */
  private def counted(n: Int, performed: () => Unit): Source[Int] =
    def go(i: Int): Source[Int] =
      if i > n then okay.pure(())
      else okay.effect[R, Unit](Writer(i))
        .flatMap(_ => okay.effect[R, Unit](Async.Run(() => performed())))
        .flatMap(_ => go(i + 1))
    okay.pure[R, Unit](()).flatMap(_ => go(1))

  private def chunked(xs: List[Int], size: Int): Chunks[Int] =
    def go(rest: List[Int]): Chunks[Int] =
      if rest.isEmpty then okay.pure(())
      else Writer.tell(ArraySeq.from(rest.take(size))).flatMap(_ => go(rest.drop(size)))
    go(xs)

  test("the four carriers agree with the pure road on every instance") {
    val xs = (1 to 20).toList
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit =
      val expected = Stream.foldUntil(xs)(using fo)
      assertEquals(Chunks.foldUntil(chunked(xs, 4))(using fo), expected, s"Chunks $name")
      assertEquals(Writer.foldUntil[Int, S, Unit, X, Async](counted(20, () => ()))(using summon, fo).runWith, expected, s"Writer $name")
      assertEquals(Source.of(xs).runFoldUntil(using fo).runWith, expected, s"Source $name")
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.take(100), "take(100)")
    check(FoldUntil.find[Int](_ > 7), "find")
    check(FoldUntil.find[Int](_ > 70), "find-none")
    check(FoldUntil.exists[Int](_ == 11), "exists")
    check(FoldUntil.forall[Int](_ < 5), "forall")
    check(FoldUntil.headOption, "headOption")
    check(FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 30 then Right(s) else Left(s + a))(identity), "until")
  }

  test("Chunks.foldUntil pulls no chunk after the one that satisfied it") {
    var pulls = 0
    val chunked = Chunks.generateWith(0) { i => pulls += 1; (ArraySeq.range(i, i + 4), i + 4) }
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.take[Int](3)), Vector(0, 1, 2))
    assertEquals(pulls, 1)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.find[Int](_ == 5)), Some(5))
    assertEquals(pulls, 2)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.take[Int](0)), Vector.empty[Int])
    assertEquals(pulls, 0)
  }

  test("Source.runFoldUntil performs the Async op before the stop and not the one after it") {
    var performed = 0
    // take(3): tells 1, 2, 3; the op after 3 is never reached, so 2 performed
    assertEquals(counted(1000, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](3)).runWith, Vector(1, 2, 3))
    assertEquals(performed, 2)
    performed = 0
    assertEquals(counted(1000, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](0)).runWith, Vector.empty[Int])
    assertEquals(performed, 0)
    performed = 0
    assertEquals(counted(5, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](100)).runWith, Vector(1, 2, 3, 4, 5))
    assertEquals(performed, 5)
  }

  test("Writer.foldUntil is tail-recursive across tells: 100 000 elements, the stop never firing") {
    val n = 100_000
    assertEquals(Source.range(0, n).runFoldUntil(using FoldUntil.exists[Long](_ < 0)).runWith, false)
    assertEquals(Chunks.foldUntil(Chunks.range(0, n))(using FoldUntil.exists[Long](_ < 0)), false)
  }
