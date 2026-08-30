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

  test("merge of chunked streams is the existing merge, one op per chunk") {
    val merged = Chunks.merge(Chunks.range(0, 500), Chunks.range(500, 1000))
    var sum = 0L
    var c = merged.receive()
    while c.isDefined do { sum += c.get.sum; c = merged.receive() }
    assertEquals(sum, (0L until 1000L).sum)
  }
}
