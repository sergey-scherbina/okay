package okay

/**
 * What the staged feed costs: CANCELLATION GRANULARITY.
 *
 * The feed used to run the interpreter between every two elements, so
 * a cancelled fiber stopped almost at once. Now it loops while the
 * channel has room and builds a program only where it must wait — so
 * cancellation is noticed when the buffer fills, or at the end of the
 * source. On a BOUNDED channel that bound is the capacity; on an
 * unbounded one it is the whole source.
 *
 * That is a real change and it is written down as a test rather than
 * a sentence, because a reader who cancels a feed needs to know when
 * it will actually stop.
 */
class TestFeedCancel extends munit.FunSuite {

  test("a bounded feed stops within a buffer of where it was cancelled") {
    // nobody consumes, so the producer fills the buffer and parks --
    // which is exactly where cancellation is noticed
    val produced = java.util.concurrent.atomic.AtomicInteger(0)
    val counted = LazyList.from(0).map { i => produced.incrementAndGet(); i }
    val c = Channel.buffer(64)(counted)
    // give it time to fill and park
    Thread.sleep(100)
    val seen = produced.get
    Thread.sleep(100)
    assert(produced.get - seen <= 64,
      s"a parked feed must not race ahead: $seen then ${produced.get}")
    c.close()
  }

  test("a closed channel stops the feed at the next wait") {
    val produced = java.util.concurrent.atomic.AtomicInteger(0)
    val counted = LazyList.from(0).map { i => produced.incrementAndGet(); i }
    val c = Channel.buffer(16)(counted)
    Thread.sleep(100)
    c.close()
    Thread.sleep(150)
    val after = produced.get
    Thread.sleep(150)
    assertEquals(produced.get, after,
      "once the channel is closed the feed must stop producing")
  }

  test("a finite source finishes and the stream ends") {
    val c = Channel.buffer(8)(LazyList.range(0, 100))
    val got = c.drained.toLazyList.toList
    assertEquals(got, (0 until 100).toList)
  }
}
