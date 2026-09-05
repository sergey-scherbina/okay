package okay

/**
 * WHEN A FEED STOPS — pinned down, because a reader who closes a
 * channel needs to know when the producer behind it actually gives up,
 * and nothing said so before.
 *
 * A buffered producer runs ahead until the buffer fills, then parks.
 * So it stops within one buffer of wherever it was when the channel
 * closed, and a finite source simply finishes.
 *
 * These began as the cost of `feed-staged-loop`, which would have
 * coarsened this to "one buffer, or the whole source". That lane was
 * measured and declined, so the granularity is unchanged — but the
 * questions it forced are worth keeping answered.
 */
class TestFeedCancel extends munit.FunSuite {

  test("a parked feed does not race ahead of its buffer") {
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
