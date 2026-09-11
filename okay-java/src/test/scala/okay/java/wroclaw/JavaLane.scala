package okay.java.wroclaw

import okay.wroclaw.{Depart, Feed, Job, Native, Ride}

import okay.java.{Collect, Streams, Windowed}
import okay.{Aggregator, Chunks, Pane}
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

  // ------------------------------------------- the JDK's own two roads

  /**
   * THE JDK ALONE, no interop of ours anywhere in it: `Arrays.stream`
   * over the event array, the platform's own `filter` and `map`, and
   * `Native.Fold` — the fold a JDK user writes when the platform hands
   * them no event-time window, which it does not.
   *
   * `Arrays.stream` is the source rather than okay-java's
   * `Streams.stream`, on purpose: the interop's spliterator hands over
   * one CHUNK per `trySplit` and the array's hands over halves, and
   * the second is what the JDK gives its own users.
   */
  def stream(feed: Feed): Job.Result = {
    val tram = Native.tramTable(feed)
    val f = new Native.Fold(tram)
    java.util.Arrays.stream(feed.events)
      .filter(((d: Depart) => Native.known(tram, d)): java.util.function.Predicate[Depart])
      .map(((d: Depart) => Native.enrich(tram, d)): JFunction[Depart, Ride])
      .forEachOrdered(((r: Ride) => f.add(r)): java.util.function.Consumer[Ride])
    f.result
  }

  /**
   * THE SAME ROAD IN PARALLEL, as the JDK means it: a MUTABLE
   * REDUCTION, `collect(supplier, accumulator, combiner)`, over a
   * parallel stream — the platform splits the array, folds each range
   * into its own container and combines them.
   *
   * TWO THINGS MAKE IT CORRECT and neither is incidental. The stream
   * is ORDERED (an array's spliterator is), so the containers are
   * combined in ENCOUNTER order — which the bunching stage needs,
   * because `Fold.absorb` stitches the pair that straddles two ranges
   * and that stitch is not commutative. And the panes need no
   * coordination at all, because with no watermark a window is a key
   * and two ranges that both touch one simply add their cells.
   *
   * The core count is the pool's, not the machine's:
   * `ForkJoinPool.commonPool` would use every core no matter what the
   * row says, so the collect is submitted to a pool of exactly the
   * width being measured. That is the JDK's own documented way to
   * bound a parallel stream, and it is what makes a 1/2/4/8 column
   * mean the same thing here as in every other lane.
   */
  def parallel(feed: Feed, cores: Int): Job.Result =
    if cores <= 1 then stream(feed) else
      val tram = Native.tramTable(feed)
      val pool = new java.util.concurrent.ForkJoinPool(cores)
      try
        pool.submit(() =>
          java.util.Arrays.stream(feed.events).parallel()
            .filter(((d: Depart) => Native.known(tram, d)): java.util.function.Predicate[Depart])
            .map(((d: Depart) => Native.enrich(tram, d)): JFunction[Depart, Ride])
            .collect(
              (() => new Native.Fold(tram)): java.util.function.Supplier[Native.Fold],
              ((f: Native.Fold, r: Ride) => f.add(r)): java.util.function.BiConsumer[Native.Fold, Ride],
              ((a: Native.Fold, b: Native.Fold) => a.absorb(b)): java.util.function.BiConsumer[Native.Fold, Native.Fold])
            .result).get()
      finally pool.shutdown()


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

  // ------------------------------------------------- the windowed road

  /**
   * THE SAME LANE WITH EVENT TIME IN IT (jdk-event-time-collector).
   *
   * Everything above holds the whole history because
   * `Collectors.groupingBy` has no notion of a group being complete.
   * `okay.java.Windowed` gives the JDK that notion: an `okay.Windows`
   * inside a `Collector`, folding each pane into a downstream
   * aggregator the moment the watermark closes it and evicting it. The
   * bunching stage gets the same treatment from the other direction —
   * `bunchAgg` is keyed STATE as an aggregator, so `groupingBy` holds
   * one small accumulator per key instead of every element that ever
   * had that key.
   *
   * It is SEQUENTIAL, and the collector says why (its combiner
   * refuses): a parallel split evicts against its own range's
   * watermark. So this road is measured against the sequential
   * groupingBy road, which is the honest pairing — same threading,
   * different state model.
   */
  def windowed(feed: Feed): Job.Result = {
    val tram = feed.routes.iterator.map(_.tram).toArray

    val route = rides(feed, tram, parallel = false).collect(
      Windowed.tumbling[Ride, Int, Job.Acc, Job.Stats, RouteFold, RouteFold](
        Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)(routeInto(tram)))

    val stop = rides(feed, tram, parallel = false).collect(
      Windowed.collector[Ride, Int, Job.Acc, Job.Stats, StopFold, StopFold](
        Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats)(stopInto))

    val byPair: JMap[java.lang.Long, BunchFold] =
      rides(feed, tram, parallel = false).collect(Collectors.groupingBy(
        ((r: Ride) => java.lang.Long.valueOf((r.route.toLong << 20) | r.stop.toLong)): JFunction[Ride, java.lang.Long],
        Collect.collector(bunchAgg)))

    var bunches = 0L; var bunchGap = 0L
    byPair.forEach((_, b) => { bunches += b.bunches; bunchGap += b.gap })

    var topWins = 0L; var topHash = 0L
    for (start, acc) <- route.ranked do
      topWins += 1
      topHash ^= Job.topHash(start, Job.top.present(acc))

    Job.Result(route.wins, route.events, route.delay, route.hash,
      stop.wins, stop.events, stop.hash, bunches, bunchGap, topWins, topHash)
  }

  /** stage 2's downstream: the checksums, and the ranking accumulated
   * per window as the panes close */
  private final case class RouteFold(wins: Long, events: Long, delay: Long, hash: Long,
                                     ranked: Map[Long, List[(Int, Job.Stats)]])

  private def routeInto(tram: Array[Boolean])
  : Aggregator[Pane[Int, Job.Stats], RouteFold, RouteFold] =
    Aggregator[Pane[Int, Job.Stats], RouteFold, RouteFold](RouteFold(0, 0, 0, 0, Map.empty))(
      (f, p) =>
        val s = p.value
        val folded = RouteFold(f.wins + 1, f.events + s.n, f.delay + s.sum,
          f.hash ^ Job.hash(p.start, p.key.toLong, s.n, s.sum, s.max.toLong), f.ranked)
        if !tram(p.key) then folded
        else folded.copy(ranked = folded.ranked.updated(p.start,
          Job.top.add(folded.ranked.getOrElse(p.start, Job.top.init), (p.key, s)))))(
      (a, b) => RouteFold(a.wins + b.wins, a.events + b.events, a.delay + b.delay, a.hash ^ b.hash,
        b.ranked.foldLeft(a.ranked)((m, kv) =>
          m.updated(kv._1, m.get(kv._1).fold(kv._2)(Job.top.merge(_, kv._2))))))(identity)

  /** stage 3's downstream */
  private final case class StopFold(wins: Long, events: Long, hash: Long)

  private val stopInto: Aggregator[Pane[Int, Job.Stats], StopFold, StopFold] =
    Aggregator[Pane[Int, Job.Stats], StopFold, StopFold](StopFold(0, 0, 0))(
      (f, p) => StopFold(f.wins + 1, f.events + p.value.n,
        f.hash ^ Job.hash(p.start, p.key.toLong, p.value.n, p.value.sum, p.value.max.toLong)))(
      (a, b) => StopFold(a.wins + b.wins, a.events + b.events, a.hash ^ b.hash))(identity)

  /**
   * Stage 4 as an AGGREGATOR: the keyed state of the bunching detector
   * in the shape `groupingBy` can hold — first and last time seen,
   * plus what the pairs inside this group came to. Its `merge` is the
   * seam: the pair that falls between two halves of one key's stream
   * is exactly `|b.first - a.last|`, which is the same stitch the
   * merge-parallel okay lane makes across a slice boundary.
   */
  private final case class BunchFold(first: Long, last: Long, bunches: Long, gap: Long, seen: Boolean)

  private val bunchAgg: Aggregator[Ride, BunchFold, BunchFold] =
    Aggregator[Ride, BunchFold, BunchFold](BunchFold(0, 0, 0, 0, false))(
      (b, r) =>
        if !b.seen then BunchFold(r.ts, r.ts, 0, 0, true)
        else
          val gap = Math.abs(r.ts - b.last)
          if gap < Job.BunchMs then BunchFold(b.first, r.ts, b.bunches + 1, b.gap + gap, true)
          else BunchFold(b.first, r.ts, b.bunches, b.gap, true))(
      (a, b) =>
        if !a.seen then b else if !b.seen then a
        else
          val gap = Math.abs(b.first - a.last)
          val crossed = gap < Job.BunchMs
          BunchFold(a.first, b.last,
            a.bunches + b.bunches + (if crossed then 1L else 0L),
            a.gap + b.gap + (if crossed then gap else 0L), true))(identity)
}
