package okay

/**
 * The chunked buffered producer: the same elements in the same order
 * as `buffer`, only carried in arrays.
 */
class TestBufferChunked extends munit.FunSuite {
  test("same elements, same order, as the per-element buffer") {
    val n = 500
    val xs = LazyList.range(0, n)
    val got = Channel.bufferChunked(16, size = 32)(xs).drained
      .toLazyList.flatMap(_.toList).toList
    assertEquals(got, (0 until n).toList)
  }

  test("unchunked at the far end is the plain buffer's stream") {
    val xs = LazyList.range(0, 300)
    val got = Channel.bufferChunked(8, size = 16)(xs).drained.unchunked.toLazyList.toList
    assertEquals(got, (0 until 300).toList)
  }

  test("a short final chunk still arrives") {
    // 10 elements at size 4 -> 4, 4, 2
    val sizes = Channel.bufferChunked(8, size = 4)(LazyList.range(0, 10)).drained
      .toLazyList.map(_.length).toList
    assertEquals(sizes.sum, 10)
    assertEquals(sizes.last, 2, s"the tail must not be dropped: $sizes")
  }

  test("an empty source ends with no chunk at all") {
    assertEquals(Channel.bufferChunked(4, size = 8)(LazyList.empty[Int]).drained
      .toLazyList.toList, Nil)
  }

  test("a failing source fails the stream after what it produced") {
    val boom = LazyList.range(0, 5) #::: LazyList.from(List(0)).map(_ => throw RuntimeException("boom"))
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    val thrown = intercept[RuntimeException] {
      Channel.bufferChunked(8, size = 4)(boom).drained.toLazyList.foreach(out ++= _.toList)
    }
    assertEquals(thrown.getMessage, "boom")
    assert(out.toList.startsWith(List(0, 1, 2, 3)), s"what was produced must arrive: ${out.toList}")
  }
}
