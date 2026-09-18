package okay

/**
 * The merge paths the DEFAULT channel feeds, run to the end with a
 * deadline (adversarial-lanes, 2026-09-06). Under the adaptive
 * default `Source.merge(chunked = true)` spun at 100% CPU for twelve
 * minutes inside a JMH warmup: a livelock, not a hang. Every shape
 * `ChunkFlushBenchmark` and `MergeBenchmark` exercise is here as a
 * law, so the next default change meets it in a test, not in a
 * benchmark that never returns.
 */
class TestMergeEnds extends munit.FunSuite {
  private val N = 2000L
  private def l = Source.of(LazyList.range(0L, N))
  private def r = Source.of(LazyList.range(N, 2L * N))
  private val expect = (0L until 2L * N).sum

  private def within[A](ms: Long, what: String)(body: => A): A =
    var out: Option[A] = None
    val th = Thread.ofVirtual().start(() => { out = Some(body) })
    th.join(ms)
    assert(!th.isAlive, s"$what did not finish in ${ms} ms (livelock or hang)")
    out.get

  test("elementwise merge ends") {
    assertEquals(within(10000, "elementwise")((l merge r).toLazyList.foldLeft(0L)(_ + _)), expect)
  }
  test("chunked merge ends, at each chunk size the benchmark sweeps") {
    for k <- Seq(16, 256, 1024) do
      assertEquals(within(10000, s"chunked k=$k")(
        l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)), expect)
  }
  test("chunked + timed flush ends") {
    assertEquals(within(10000, "flush")(
      l.merge(r, capacity = 1024, chunked = true, flushAfter = Some(1000)).toLazyList.foldLeft(0L)(_ + _)), expect)
  }
  test("Channel.buffer and bufferChunked end") {
    val list = (0L until 4000L).toList
    assertEquals(within(10000, "buffer")(Channel.buffer(1024)(list).drained.toLazyList.foldLeft(0L)(_ + _)), list.sum)
    assertEquals(within(10000, "bufferChunked")(Channel.bufferChunked(64, size = 256)(list).drained.toLazyList.foldLeft(0L)((a, ch) => a + ch.sum)), list.sum)
  }
  test("Channel.merge ends") {
    assertEquals(within(10000, "Channel.merge")(
      Channel.merge(LazyList.range(0L, 500L), LazyList.range(500L, 1000L)).toLazyList.foldLeft(0L)(_ + _)), (0L until 1000L).sum)
  }
}
