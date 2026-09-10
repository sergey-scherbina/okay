package okay.flink.wroclaw

import okay.Chunks
import scala.collection.mutable

/**
 * THE OKAY LANE: the same job, in one JVM, with no engine under it.
 *
 * What this file makes visible is as interesting as the number it
 * produces: Flink's `.window(TumblingEventTimeWindows.of(...))` is one
 * line, and here it is `Windows` — fifty lines of keyed panes, a
 * watermark and an eviction sweep that a user of okay writes for
 * themselves. The core has `Aggregator` (the arithmetic) and `Chunks`
 * (the pass); it has no event-time window operator, and this benchmark
 * is where that shows. What it does NOT have to write is the
 * statistic — `Job.stats` is one value, shared with the Flink lane.
 *
 * The pass is single-threaded and single-pass: the fan-out to four
 * consumers costs nothing, because a fan-out in one JVM is calling
 * four methods with the same reference. In Flink the same fan-out is
 * three shuffles. That asymmetry is not a flaw of the benchmark, it
 * IS the difference between the two things being compared, and the
 * report says so rather than hiding it.
 */
object OkayLane {

  /**
   * An event-time window operator: keyed panes, evicted by watermark.
   *
   * Panes live in a `LongMap` under a packed key — the window index in
   * the high bits, the (dense) route or stop index in the low twenty —
   * so the hot path allocates no tuple and hashes no object. Eviction
   * sweeps only when the watermark crosses a slide boundary, which is
   * once per five minutes of EVENT time, not once per element.
   */
  final class Windows(size: Long, slide: Long, emit: (Long, Int, Job.Stats) => Unit) {
    private val agg = Job.stats
    private val panes = mutable.LongMap.empty[Job.Acc]
    private val closing = mutable.ArrayBuffer.empty[Long]
    private val panesPer = (size / slide).toInt
    private var nextSweep = Long.MinValue

    /** the element enters every window it belongs to (three, for the
     * sliding stage — which is why a sliding window costs what it does) */
    def add(k: Int, ride: Ride): Unit =
      var start = ride.ts - Math.floorMod(ride.ts, slide) - size + slide
      var i = 0
      while i < panesPer do
        val id = ((start / slide) << 20) | k.toLong
        panes.update(id, agg.add(panes.getOrElse(id, agg.init), ride))
        start += slide
        i += 1

    /** the watermark: every pane that ended before it is complete */
    def advance(watermark: Long): Unit =
      if watermark >= nextSweep then
        sweep(watermark)
        nextSweep = watermark - Math.floorMod(watermark, slide) + slide

    /** the end of the input closes everything still open */
    def close(): Unit = sweep(Long.MaxValue)

    private def sweep(watermark: Long): Unit =
      panes.foreachKey(id => if (id >>> 20) * slide + size <= watermark then closing += id)
      var i = 0
      while i < closing.length do
        val id = closing(i)
        val acc = panes.remove(id)
        acc.foreach(a => emit((id >>> 20) * slide, (id & 0xfffffL).toInt, agg.present(a)))
        i += 1
      closing.clear()
  }

  /**
   * One pass over the events. `chunk` is the source's chunk size —
   * the pipeline is `Chunks`, so the element loop is inside a chunk
   * walk rather than a per-element program.
   */
  def run(feed: Feed, chunk: Int = 256): Job.Result = {
    val tram = feed.routes.map(_.tram)

    var routeWins = 0L; var routeEvents = 0L; var routeDelay = 0L; var routeHash = 0L
    var stopWins = 0L; var stopEvents = 0L; var stopHash = 0L
    var bunches = 0L; var bunchGap = 0L

    // stage 5: the per-window ranking, accumulated as the tumbling
    // windows of stage 2 close — the same `Aggregator.topK` value the
    // Flink lane hands to its own second window
    val ranked = mutable.LongMap.empty[List[(Int, Job.Stats)]]

    val routeWindows = new Windows(Job.WindowMs, Job.WindowMs, (start, route, s) => {
      routeWins += 1; routeEvents += s.n; routeDelay += s.sum
      routeHash ^= Job.hash(start, route.toLong, s.n, s.sum, s.max.toLong)
      if tram(route) then
        ranked.update(start, Job.top.add(ranked.getOrElse(start, Job.top.init), (route, s)))
    })

    val stopWindows = new Windows(Job.SlideWindowMs, Job.SlideMs, (start, stop, s) => {
      stopWins += 1; stopEvents += s.n
      stopHash ^= Job.hash(start, stop.toLong, s.n, s.sum, s.max.toLong)
    })

    // stage 4: the keyed state — when this route last left this stop
    val lastSeen = mutable.LongMap.empty[Long]
    var maxTs = Long.MinValue

    // stage 1: the enrichment, as the source's own map/filter
    val source = Chunks.fromIterator(feed.events.iterator, chunk)
    val rides = Chunks.map(Chunks.filter(source)(d => d.route >= 0 && d.route < tram.length))(d =>
      new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))

    Chunks.foldLeft(rides)(())((_, r) => {
      if r.ts > maxTs then maxTs = r.ts
      routeWindows.add(r.route, r)
      stopWindows.add(r.stop, r)

      val key = (r.route.toLong << 20) | r.stop.toLong
      val prev = lastSeen.getOrElse(key, Long.MinValue)
      if prev != Long.MinValue then
        val gap = Math.abs(r.ts - prev) // arrival order, not event order: |gap|
        if gap < Job.BunchMs then { bunches += 1; bunchGap += gap }
      lastSeen.update(key, r.ts)

      // the watermark of a bounded-out-of-orderness strategy, per element
      val watermark = maxTs - Job.Lateness
      routeWindows.advance(watermark)
      stopWindows.advance(watermark)
      ()
    })

    routeWindows.close()
    stopWindows.close()

    var topWins = 0L; var topHash = 0L
    ranked.foreachKey { start =>
      topWins += 1
      topHash ^= Job.topHash(start, Job.top.present(ranked(start)))
    }

    Job.Result(routeWins, routeEvents, routeDelay, routeHash,
      stopWins, stopEvents, stopHash, bunches, bunchGap, topWins, topHash)
  }
}
