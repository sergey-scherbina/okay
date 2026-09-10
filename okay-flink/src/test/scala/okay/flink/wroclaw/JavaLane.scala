package okay.flink.wroclaw

import okay.java.{Collect, Streams}
import okay.Chunks
import java.util.function.Function as JFunction
import java.util.stream.{Collectors, Stream as JStream}
import java.util.{List as JList, Map as JMap}

/**
 * THE JDK LANE: the same job over `java.util.stream`, through
 * okay-java's interop, sequential and parallel.
 *
 * WHY IT BELONGS BESIDE FLINK. `Collect.collector` says the same thing
 * `FlinkInterop.toFlink` says: an okay `Aggregator` IS a JDK
 * `Collector` — supplier/accumulator/combiner/finisher against
 * init/add/merge/present, and `merge` is precisely what a PARALLEL
 * stream needs to combine the halves it split. So `Job.stats`, the one
 * value the okay and Flink lanes share, runs here too, and this lane
 * measures a third engine over the same arithmetic:
 * `ForkJoinPool.commonPool` instead of a MiniCluster.
 *
 * WHAT THE JDK DOES NOT HAVE, and the lane shows it rather than
 * apologising for it:
 *
 *   - **No event time.** There is no watermark, so a "window" here is
 *     just a KEY — `(windowStart, route)` — and every pane of the
 *     whole run is live at once. The okay and Flink lanes evict as the
 *     watermark passes; this one holds all of them and finishes with a
 *     map of the entire history. On a bounded replay that is merely
 *     memory; on an unbounded stream it is the reason event-time
 *     engines exist.
 *   - **No fan-out.** A `java.util.stream` is single-use, so the three
 *     consumers of one pass become THREE PASSES over the source. That
 *     is charged to this lane honestly — it is what the model costs.
 *   - **Ordering.** The bunching stage needs per-key arrival order,
 *     which the JDK gives only because okay-java's spliterator claims
 *     `ORDERED` (it does) and `groupingBy(…, toList())` preserves
 *     encounter order for an ordered stream, parallel or not.
 */
object JavaLane {

  /**
   * THE SPLIT SIZE IS THE CHUNK SIZE, and on this lane that is not a
   * detail. `Streams.spliterator` hands over ONE CHUNK per `trySplit`,
   * so a chunked source of 256 turns 2.4M events into ~9 400 parallel
   * splits — and `Collectors.groupingBy` builds one map PER SPLIT and
   * merges them pairwise. Measured here: at 256 the parallel lane dies
   * with an OutOfMemoryError on a 4 GB heap before it finishes one
   * service day. At 8 192 it runs. Nothing is wrong with either the
   * interop or the JDK; the two just have to be told about each other,
   * and a benchmark is where that gets found.
   */
  private val Chunk = 8192

  /** stage 1, as this lane's source: the enrichment, then the interop */
  private def rides(feed: Feed, tram: Array[Boolean], parallel: Boolean): JStream[Ride] =
    val source = Chunks.fromIterator(feed.events.iterator, Chunk)
    val enriched = Chunks.map(Chunks.filter(source)(d => d.route >= 0 && d.route < tram.length))(d =>
      new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))
    Streams.stream(enriched, parallel)

  /** a pane's identity, packed the way the okay lane packs it */
  private def paneKey(start: Long, slide: Long, key: Int): java.lang.Long =
    java.lang.Long.valueOf(((start / slide) << 20) | key.toLong)

  def run(feed: Feed, parallel: Boolean): Job.Result = {
    val tram = feed.routes.iterator.map(_.tram).toArray

    // ---- stage 2: tumbling windows per route. No watermark: the
    // window is a key, and every one of them lives to the end
    val byRoute: JMap[java.lang.Long, Job.Stats] =
      rides(feed, tram, parallel).collect(Collectors.groupingBy(
        ((r: Ride) => paneKey(r.ts - Math.floorMod(r.ts, Job.WindowMs), Job.WindowMs, r.route)): JFunction[Ride, java.lang.Long],
        Collect.collector(Job.stats)))

    var routeWins = 0L; var routeEvents = 0L; var routeDelay = 0L; var routeHash = 0L
    val ranked = scala.collection.mutable.LongMap.empty[List[(Int, Job.Stats)]]
    byRoute.forEach { (id, s) =>
      val start = (id.longValue >>> 20) * Job.WindowMs
      val route = (id.longValue & 0xfffffL).toInt
      routeWins += 1; routeEvents += s.n; routeDelay += s.sum
      routeHash ^= Job.hash(start, route.toLong, s.n, s.sum, s.max.toLong)
      if tram(route) then
        ranked.update(start, Job.top.add(ranked.getOrElse(start, Job.top.init), (route, s)))
    }

    // ---- stage 3: sliding windows per stop. A JDK stream cannot put
    // one element in three groups, so the element is EXPANDED first —
    // three pairs per ride, which is the shape the model forces
    val byStop: JMap[java.lang.Long, Job.Stats] =
      rides(feed, tram, parallel)
        .flatMap(((r: Ride) => {
          val first = r.ts - Math.floorMod(r.ts, Job.SlideMs) - Job.SlideWindowMs + Job.SlideMs
          JStream.of(
            (paneKey(first, Job.SlideMs, r.stop), r),
            (paneKey(first + Job.SlideMs, Job.SlideMs, r.stop), r),
            (paneKey(first + 2 * Job.SlideMs, Job.SlideMs, r.stop), r))
        }): JFunction[Ride, JStream[(java.lang.Long, Ride)]])
        .collect(Collectors.groupingBy(
          ((p: (java.lang.Long, Ride)) => p._1): JFunction[(java.lang.Long, Ride), java.lang.Long],
          Collectors.mapping(
            ((p: (java.lang.Long, Ride)) => p._2): JFunction[(java.lang.Long, Ride), Ride],
            Collect.collector(Job.stats))))

    var stopWins = 0L; var stopEvents = 0L; var stopHash = 0L
    byStop.forEach { (id, s) =>
      val start = (id.longValue >>> 20) * Job.SlideMs
      val stop = (id.longValue & 0xfffffL).toInt
      stopWins += 1; stopEvents += s.n
      stopHash ^= Job.hash(start, stop.toLong, s.n, s.sum, s.max.toLong)
    }

    // ---- stage 4: the bunching detector. Keyed state has no place in
    // the model either, so the whole group is materialised and scanned
    // — in ENCOUNTER order, which is what makes the answer the same
    val byPair: JMap[java.lang.Long, JList[Ride]] =
      rides(feed, tram, parallel).collect(Collectors.groupingBy(
        ((r: Ride) => java.lang.Long.valueOf((r.route.toLong << 20) | r.stop.toLong)): JFunction[Ride, java.lang.Long],
        Collectors.toList[Ride]))

    var bunches = 0L; var bunchGap = 0L
    byPair.forEach { (_, rs) =>
      var prev = Long.MinValue
      rs.forEach { r =>
        if prev != Long.MinValue then
          val gap = Math.abs(r.ts - prev)
          if gap < Job.BunchMs then { bunches += 1; bunchGap += gap }
        prev = r.ts
      }
    }

    // ---- stage 5: the ranking, from stage 2's map (already in hand)
    var topWins = 0L; var topHash = 0L
    ranked.foreachKey { start =>
      topWins += 1
      topHash ^= Job.topHash(start, Job.top.present(ranked(start)))
    }

    Job.Result(routeWins, routeEvents, routeDelay, routeHash,
      stopWins, stopEvents, stopHash, bunches, bunchGap, topWins, topHash)
  }
}
