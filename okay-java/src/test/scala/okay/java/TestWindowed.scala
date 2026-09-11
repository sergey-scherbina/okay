package okay.java

import okay.{Aggregator, Chunks, Pane}

/**
 * An event-time window as a `Collector` (Windowed), against a brute
 * force that groups by (window, key) and sums — the same oracle
 * `TestWindows` uses in the core, now on the other side of the JDK
 * seam.
 *
 * The test that matters is the PARALLEL one, and what it asserts is a
 * REFUSAL. This suite was written the other way round first — as an
 * equality check on a parallel stream — and it failed: a split evicts
 * panes against its own range's watermark, so two splits reported the
 * same window with partial values. That failure is the reason the
 * collector's combiner throws, and this test is what keeps it
 * throwing.
 */
class TestWindowed extends munit.FunSuite {

  final case class Ev(ts: Long, key: String, v: Long)

  val sum: Aggregator[Ev, Long, Long] = Aggregator.sum[Long].contramap[Ev](_.v)

  /** the closed panes, collected by (start, key) — order-insensitive,
   * as a `Windowed` downstream must be */
  val intoMap: Aggregator[Pane[String, Long], Map[(Long, String), Long], Map[(Long, String), Long]] =
    Aggregator[Pane[String, Long], Map[(Long, String), Long], Map[(Long, String), Long]](
      Map.empty)((m, p) => m.updated((p.start, p.key), p.value))((a, b) => a ++ b)(identity)

  def mix(x: Long): Long =
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  /** events in ARRIVAL order, out of order by at most `jitter` */
  def events(n: Int, span: Long, keys: Int, jitter: Long): Vector[Ev] =
    val evs = (0 until n).map { i =>
      val r = mix(i.toLong)
      Ev(Math.floorMod(r, span), "k" + Math.floorMod(r >>> 17, keys.toLong), Math.floorMod(r >>> 41, 100L))
    }
    evs.sortBy(e => e.ts + Math.floorMod(mix(e.ts * 31 + e.v), jitter + 1)).toVector

  def reference(evs: Seq[Ev], size: Long, slide: Long): Map[(Long, String), Long] =
    val m = scala.collection.mutable.Map.empty[(Long, String), Long]
    for e <- evs do
      var start = e.ts - Math.floorMod(e.ts, slide) - size + slide
      var i = 0L
      while i < size / slide do
        m.update((start, e.key), m.getOrElse((start, e.key), 0L) + e.v)
        start += slide
        i += 1
      end while
    m.toMap

  def collected(evs: Seq[Ev], size: Long, slide: Long, lateness: Long, parallel: Boolean, chunk: Int)
  : Map[(Long, String), Long] =
    val source = Chunks.fromIterator(evs.iterator, chunk)
    Streams.stream(source, parallel).collect(
      Windowed.collector[Ev, String, Long, Long, Map[(Long, String), Long], Map[(Long, String), Long]](
        size, slide, lateness)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)(intoMap))

  test("a windowing collector equals the recompute — tumbling, sequential") {
    val evs = events(2000, 10000L, 7, 40L)
    assertEquals(collected(evs, 100L, 100L, 50L, parallel = false, 64), reference(evs, 100L, 100L))
  }

  test("and sliding, sequential") {
    val evs = events(2000, 10000L, 5, 20L)
    assertEquals(collected(evs, 300L, 100L, 30L, parallel = false, 64), reference(evs, 300L, 100L))
  }

  /**
   * THE REFUSAL, ASSERTED. A parallel split evicts panes against its
   * own range's watermark, so each split would report the same window
   * with a partial value; the combiner throws instead of returning
   * that. The test that would otherwise have caught it silently is the
   * one that produced this design: written first as an equality check,
   * it failed with two partial reports of the same pane.
   */
  test("a parallel stream is refused, loudly") {
    val evs = events(4000, 20000L, 6, 25L)
    val thrown = intercept[Exception](collected(evs, 200L, 100L, 30L, parallel = true, 64))
    val message = Option(thrown.getMessage).getOrElse(Option(thrown.getCause).flatMap(c => Option(c.getMessage)).getOrElse(""))
    assert(message.contains("SEQUENTIAL"), s"expected the collector's own refusal, got: $thrown")
  }

  test("the collector is reusable: the same value collects twice") {
    val evs = events(500, 4000L, 4, 20L)
    val c = Windowed.tumbling[Ev, String, Long, Long, Map[(Long, String), Long], Map[(Long, String), Long]](
      100L, 30L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)(intoMap)
    val first = Streams.stream(Chunks.fromIterator(evs.iterator, 64), false).collect(c)
    val second = Streams.stream(Chunks.fromIterator(evs.iterator, 64), false).collect(c)
    assertEquals(second, first)
    assertEquals(first, reference(evs, 100L, 100L))
  }
}
