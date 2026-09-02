package okay

import !.*
import Chunks.*

/** Chunked streams: batch amortization over the ordinary stream layer. */
class TestChunks extends munit.FunSuite {

  test("lazy: an infinite chunked generator computes only the pulled chunks") {
    var built = 0
    val s = Chunks.generate(0)(x => { built += 1; x })(_ + 1)(8)
    assertEquals(built, 0)
    assertEquals(s.elements.take(20).toList, (0 until 20).toList)
    assertEquals(built, 24)   // ceil(20/8) = 3 chunks of 8
  }

  test("elements agrees with the unchunked generators") {
    assertEquals(Chunks.fibs[Long]().elements.take(10).toList,
      okay.fibs[Long, LazyList].take(10).toList)
    assertEquals(Chunks.nats[Int](7).elements.take(20).toList,
      okay.nats[Int, LazyList].take(20).toList)
  }

  test("range emits a short tail chunk when size does not divide") {
    assertEquals(Chunks.range(0, 10, 4).elements.toList, (0L until 10L).toList)
    assertEquals(Chunks.range(0, 10, 4).toLazyList.map(_.length).toList, List(4, 4, 2))
  }

  test("chunked transformers agree with the LazyList reference, boundaries included") {
    val ref = okay.nats[Int, LazyList].map(_ * 2).filter(_ % 3 == 0).take(100).toList
    val c = Chunks.take(Chunks.filter(Chunks.map(Chunks.nats[Int](7))(_ * 2))(_ % 3 == 0))(100)
    assertEquals(c.elements.toList, ref)
    assertEquals(Chunks.drop(Chunks.range(0, 20, 6))(7).elements.toList, (7L until 20L).toList)
    assertEquals(Chunks.takeWhile(Chunks.nats[Int](4))(_ < 10).elements.toList, (0 until 10).toList)
    assertEquals(Chunks.dropWhile(Chunks.range(0, 12, 5))(_ < 7).elements.toList, (7L until 12L).toList)
    assertEquals(Chunks.filter(Chunks.range(0, 10, 3))(_ => false).elements.toList, Nil)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.sum[Long]), 45L)
  }

  test("a transformer chain over an infinite source stays lazy") {
    var built = 0
    val s = Chunks.generate(0)(x => { built += 1; x })(_ + 1)(8)
    val t = Chunks.take(Chunks.map(s)(_ + 1))(10)
    assertEquals(built, 0)
    assertEquals(Chunks.fold(t)(using Fold.count), 10L)
    assertEquals(built, 16)   // two chunks of 8
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
    def sums(n: Int, acc: Long): Long ! Take % Long =
      if n == 0 then pure(acc)
      else Take.await[Long].flatMap:
        case Some(x) => sums(n - 1, acc + x)
        case None => pure(acc)
    var built = 0
    val s = Chunks.generate(0L)(x => { built += 1; x })(_ + 1)(16)
    assertEquals(Chunks.pipe(s)(sums(10, 0)), 45L)
    assertEquals(built, 16)                                        // one chunk pulled
    assertEquals(Chunks.pipe(Chunks.range(0, 5))(sums(100, 0)), 10L)  // early end: None
  }

  test("merge of chunked streams is the existing merge, one op per chunk") {
    val merged = Chunks.range(0, 500) merge Chunks.range(500, 1000)
    var sum = 0L
    var c = merged.receiveBlocking()
    while c.isDefined do { sum += c.get.sum; c = merged.receiveBlocking() }
    assertEquals(sum, (0L until 1000L).sum)
  }

  test("foldLeft agrees with fold, and specializes") {
    // the same answers as the Fold-as-data path — the point of the
    // inline entry is that it is 5.4x faster, not that it differs
    assertEquals(Chunks.foldLeft(Chunks.range(0, 10))(0L)(_ + _), 45L)
    assertEquals(Chunks.count(Chunks.range(0, 10)), 10L)
    assertEquals(
      Chunks.foldLeft(Chunks.range(0, 10))(0L)(_ + _),
      Chunks.fold(Chunks.range(0, 10))(using Fold.sum[Long]))
    assertEquals(Chunks.count(Chunks.range(0, 10)),
      Chunks.fold(Chunks.range(0, 10))(using Fold.count))
    // empty, and a step that is not commutative, so order is checked
    assertEquals(Chunks.count(Chunks.end[Long]), 0L)
    assertEquals(
      Chunks.foldLeft(Chunks.fromIterator((1 to 5).iterator, 2))("")(_ + _.toString),
      "12345")
  }

  test("specialized accumulators: same answers, unboxed step") {
    // the four shapes exist to keep the accumulator in a register;
    // what a test can check is that they still compute the right thing
    // and that Chunks.fold really routes through them
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.count), 10L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.sumLong), 45L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.maxLong), 9L)
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.minLong), 0L)
    assertEquals(
      Chunks.fold(Chunks.map(Chunks.range(0, 5))(_.toInt))(using Fold.sumInt), 10)
    assertEquals(
      Chunks.fold(Chunks.map(Chunks.range(0, 5))(_.toDouble))(using Fold.sumDouble), 10.0)
    assertEquals(
      Chunks.fold(Chunks.range(0, 10))(using Fold.exists[Long](_ == 7L)), true)
    assertEquals(
      Chunks.fold(Chunks.range(0, 10))(using Fold.forall[Long](_ < 10L)), true)
    assertEquals(
      Chunks.fold(Chunks.range(0, 10))(using Fold.forall[Long](_ < 5L)), false)

    // the specialized shapes ARE Folds: a generic consumer still works
    val asFold: Fold[Long, Long] = Fold.count[Long]
    assertEquals(Chunks.fold(Chunks.range(0, 4))(using asFold), 4L)

    // and an empty stream gives the init, through the same dispatch
    assertEquals(Chunks.fold(Chunks.end[Long])(using Fold.count), 0L)
    assertEquals(Chunks.fold(Chunks.end[Long])(using Fold.forall[Long](_ => false)), true)
  }

  test("Fold.sum picks the unboxed sum where the type is known") {
    // summonFrom selects on N, and the `=:=` transports the fold —
    // the point is that both the specialized and generic branches
    // compute the same thing
    assertEquals(Chunks.fold(Chunks.range(0, 10))(using Fold.sum[Long]), 45L)
    assertEquals(Fold.sum[Long].getClass, Fold.sumLong.getClass)
    assertEquals(Fold.sum[Int].getClass, Fold.sumInt.getClass)
    assertEquals(Fold.sum[Double].getClass, Fold.sumDouble.getClass)
    // BigInt has no specialization: the generic branch, still correct
    val big = Fold.sum[BigInt]
    assertEquals(List[BigInt](1, 2, 3).foldLeft(big.init)(big.add), BigInt(6))
  }
}
