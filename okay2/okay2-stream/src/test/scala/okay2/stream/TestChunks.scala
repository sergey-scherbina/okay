package okay2.stream

import okay2._
import okay2.stream.Chunks.ChunksOps

/** Chunked streams: batch amortization over the ordinary stream layer. */
class TestChunks extends munit.FunSuite {

  test("lazy: an infinite chunked generator computes only the pulled chunks") {
    var built = 0
    val s = Chunks.generate(0)(x => { built += 1; x })(_ + 1)(8)
    assertEquals(built, 0)
    assertEquals(s.elements.take(20).toList, (0 until 20).toList)
    assertEquals(built, 24) // ceil(20/8) = 3 chunks of 8
  }

  test("elements agrees with the LazyList generators") {
    assertEquals(Chunks.fibs[Long]().elements.take(10).toList,
      LazyList.unfold((0L, 1L)) { case (a, b) => Some((a, (b, a + b))) }.take(10).toList)
    assertEquals(Chunks.nats[Int](7).elements.take(20).toList, LazyList.from(0).take(20).toList)
  }

  test("range emits a short tail chunk when size does not divide, and it is a long[]") {
    assertEquals(Chunks.range(0, 10, 4).elements.toList, (0L until 10L).toList)
    assertEquals(Chunks.range(0, 10, 4).toLazyList.map(_.length).toList, List(4, 4, 2))
    assert(Chunks.range(0, 10, 4).toLazyList.head.isInstanceOf[scala.collection.immutable.ArraySeq.ofLong])
  }

  test("chunked transformers agree with the LazyList reference, boundaries included") {
    val ref = LazyList.from(0).map(_ * 2).filter(_ % 3 == 0).take(100).toList
    val c = Chunks.take(Chunks.filter(Chunks.map(Chunks.nats[Int](7))(_ * 2))(_ % 3 == 0))(100)
    assertEquals(c.elements.toList, ref)
    assertEquals(Chunks.drop(Chunks.range(0, 20, 6))(7).elements.toList, (7L until 20L).toList)
    assertEquals(Chunks.takeWhile(Chunks.nats[Int](4))(_ < 10).elements.toList, (0 until 10).toList)
    assertEquals(Chunks.dropWhile(Chunks.range(0, 12, 5))(_ < 7).elements.toList, (7L until 12L).toList)
    assertEquals(Chunks.filter(Chunks.range(0, 10, 3))(_ => false).elements.toList, Nil)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.sum[Long]), 45L)
  }

  test("a transformer chain over an infinite source stays lazy") {
    var built = 0
    val s = Chunks.generate(0)(x => { built += 1; x })(_ + 1)(8)
    val t = Chunks.take(Chunks.map(s)(_ + 1))(10)
    assertEquals(built, 0)
    assertEquals(Chunks.fold(t)(Fold.count), 10L)
    assertEquals(built, 16) // two chunks of 8
  }

  test("zip realigns chunk boundaries and stops at the shorter stream") {
    assertEquals(
      Chunks.zip(Chunks.range(0, 10, 3), Chunks.range(100, 110, 4)).elements.toList,
      (0L until 10L).map(i => (i, 100L + i)).toList)
    assertEquals(Chunks.zip(Chunks.range(0, 5), Chunks.range(0, 100)).elements.size, 5)
    assertEquals(
      Chunks.zip(Chunks.nats[Int](7), Chunks.fibs[Long](5)).elements.take(6).toList,
      List((0, 0L), (1, 1L), (2, 1L), (3, 2L), (4, 3L), (5, 5L)))
  }

  test("rechunk preserves content and normalizes sizes, tail shorter") {
    val r = Chunks.rechunk(Chunks.range(0, 20, 3))(8)
    assertEquals(r.toLazyList.map(_.length).toList, List(8, 8, 4))
    assertEquals(Chunks.rechunk(Chunks.range(0, 20, 3))(8).elements.toList, (0L until 20L).toList)
    val f = Chunks.filter(Chunks.range(0, 100, 10))(_ % 7 == 0)
    assertEquals(Chunks.rechunk(f)(4).elements.toList, (0L until 100L).filter(_ % 7 == 0).toList)
  }

  test("chunked pipe: an elementwise consumer over chunked transport") {
    def sums(n: Int, acc: Long): Long ! Take[Long] =
      if (n == 0) pure(acc)
      else Take.await[Long].flatMap {
        case Some(x) => sums(n - 1, acc + x)
        case None => pure(acc)
      }
    var built = 0
    val s = Chunks.generate(0L)(x => { built += 1; x })(_ + 1)(16)
    assertEquals(Chunks.pipe(s)(sums(10, 0)), 45L)
    assertEquals(built, 16) // one chunk pulled
    assertEquals(Chunks.pipe(Chunks.range(0, 5))(sums(100, 0)), 10L) // early end: None
  }

  test("foldLeft agrees with fold, and the specialized accumulators give the same answers") {
    assertEquals(Chunks.foldLeft(Chunks.range(0, 10))(0L)(_ + _), 45L)
    assertEquals(Chunks.count(Chunks.range(0, 10)), 10L)
    assertEquals(Chunks.foldLeft(Chunks.range(0, 10))(0L)(_ + _), Chunks.fold(Chunks.range(0, 10))(Fold.sum[Long]))
    assertEquals(Chunks.count(Chunks.end[Long]), 0L)
    assertEquals(Chunks.foldLeft(Chunks.fromIterator((1 to 5).iterator, 2))("")(_ + _.toString), "12345")
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.count), 10L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.sumLong), 45L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.maxLong), 9L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.minLong), 0L)
    assertEquals(Chunks.fold(Chunks.map(Chunks.range(0, 5))(_.toInt))(Fold.sumInt), 10)
    assertEquals(Chunks.fold(Chunks.mapTagged(Chunks.range(0, 5))(_.toDouble))(Fold.sumDouble), 10.0)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.exists[Long](_ == 7L)), true)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(Fold.forall[Long](_ < 5L)), false)
    val asFold: Fold[Long, Long] = Fold.count[Long]
    assertEquals(Chunks.fold(Chunks.range(0, 4))(asFold), 4L)
    assertEquals(Chunks.fold(Chunks.end[Long])(Fold.forall[Long](_ => false)), true)
  }

  test("mapTagged fills an unboxed chunk where map fills a boxed one") {
    val tagged = Chunks.mapTagged(Chunks.range(0, 4))(_ * 2).toLazyList.head
    assert(tagged.isInstanceOf[scala.collection.immutable.ArraySeq.ofLong], tagged.getClass.getName)
    val boxed = Chunks.map(Chunks.range(0, 4))(_ * 2).toLazyList.head
    assert(boxed.isInstanceOf[scala.collection.immutable.ArraySeq.ofRef[_]], boxed.getClass.getName)
    assertEquals(tagged.toList, boxed.toList)
  }

  test("ofChars: a string in char[] chunks") {
    assertEquals(Chunks.ofChars("hello, world", 5).toLazyList.map(_.length).toList, List(5, 5, 2))
    assertEquals(Chunks.ofChars("hello, world", 5).elements.mkString, "hello, world")
  }
}
