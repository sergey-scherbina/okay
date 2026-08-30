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

  test("merge of chunked streams is the existing merge, one op per chunk") {
    val merged = Chunks.merge(Chunks.range(0, 500), Chunks.range(500, 1000))
    var sum = 0L
    var c = merged.receive()
    while c.isDefined do { sum += c.get.sum; c = merged.receive() }
    assertEquals(sum, (0L until 1000L).sum)
  }
}
