package okay.wroclaw

import scala.collection.mutable

/**
 * THE JOB WITH NO okay TYPE IN IT — what a user of fs2, ZIO, kyo,
 * `java.util.stream` or of nothing at all writes when the library
 * hands them no event-time window, which is the true state of all
 * five (operator's rule, 2026-09-10: a competitor's row must measure
 * THEIR api, not ours).
 *
 * The earlier §20 gave those five lanes `okay.Windows` and said so.
 * That comparison was honest about what it measured — the plumbing
 * around identical work — but it is not the comparison a reader wants
 * before choosing a library, because the operator they would be
 * choosing is the one thing it held constant.
 *
 * So this is the same five stages as `Job` defines them, in plain
 * Scala over plain maps:
 *
 *   - a window is A KEY, `(windowStart << 20) | key`, and nothing ever
 *     evicts it. That is not laziness, it is the shape the model
 *     forces: with no watermark there is no notion of a window being
 *     COMPLETE, so the state of the run is the whole history of it.
 *     On this bounded replay that costs memory and nothing else; on an
 *     unbounded stream it is the reason event-time engines exist, and
 *     §20 prices the memory rather than asserting the point.
 *   - the accumulator is a MUTABLE CELL, three fields bumped in place.
 *     An `Aggregator` is not available here and neither is anything
 *     like it, so the fold is the one every hand-written pipeline has:
 *     look the cell up, add to it, put nothing back.
 *   - the bunching detector is a last-seen map, exactly as in the okay
 *     lane, because there it is already plain keyed state.
 *
 * WHY IT IS SHARED between the five lanes rather than copied into
 * each. Every line below is stdlib — no okay import, nothing a fs2 or
 * ZIO user could not have written — and holding it identical is what
 * makes the rows comparable: what differs between the lanes is then
 * the CARRIER (how an element reaches `add`, and how the library
 * expresses running four of these at once) and not my typing. A lane
 * that also rewrote the arithmetic would be measuring two changes.
 *
 * `Job.Stats`, `Job.hash`, `Job.topHash` and `Job.ranking` are used
 * for the ANSWER, and they are the specification of the job rather
 * than a way of computing it: the checksums are how any two lanes are
 * compared at all, and the ranking rule is what the report asks for.
 */
object Native {

  /** the accumulator of stage 2 and 3, bumped in place */
  final class Cell(var n: Long, var sum: Long, var max: Int)

  /**
   * ONE SLICE'S STATE, and — because it also carries what a slice
   * boundary needs — one PARALLEL slice's state. `absorb` puts two
   * together in arrival order, which is what every parallel lane
   * below reduces with, whether its combiner is a `Thread` join, the
   * JDK's mutable reduction, `parEvalMap`, `foreachPar` or
   * `Async.parallel`.
   */
  final class Fold(val tram: Array[Boolean]) {

    /** stage 2: the tumbling panes, `(window << 20) | route` */
    private val route = mutable.LongMap.empty[Cell]

    /** stage 3: the sliding panes, `(pane << 20) | stop` */
    private val stop = mutable.LongMap.empty[Cell]

    /** stage 4's keyed state */
    private val lastSeen = mutable.LongMap.empty[Long]

    /** stage 4 across a slice boundary: the first and last time each
     * key was seen INSIDE this slice, so the pair that straddles two
     * slices is `|b.first - a.last|` and is counted exactly once */
    private val first = mutable.LongMap.empty[Long]
    private val last = mutable.LongMap.empty[Long]

    private var bunches = 0L
    private var gap = 0L

    /** stage 1, the map-side join, as a predicate and a function —
     * every lane applies these with its own `filter` and `map` */
    def known(d: Depart): Boolean = Native.known(tram, d)

    def enrich(d: Depart): Ride = Native.enrich(tram, d)

    /** how many pane cells this fold holds — the state of the run, and
     * with no watermark to close a pane it only ever grows. It is the
     * number to compare against `okay.Windows.live` (the panes open at
     * one moment), and the reason the parallel rows of the five
     * in-process libraries plateau: this is what their merge walks. */
    def cells: Int = route.size + stop.size

    /** stages 2, 3 and 4 of one event */
    def add(r: Ride): Unit =
      bump(route, ((Job.windowOf(r.ts) / Job.WindowMs) << 20) | r.route.toLong, r.delay)
      var pane = Job.slideFirst(r.ts) / Job.SlideMs
      var i = 0
      while i < Job.Panes do
        bump(stop, (pane << 20) | r.stop.toLong, r.delay)
        pane += 1
        i += 1
      val key = (r.route.toLong << 20) | r.stop.toLong
      val prev = lastSeen.getOrElse(key, Long.MinValue)
      if prev == Long.MinValue then first.update(key, r.ts)
      else
        val g = Math.abs(r.ts - prev)
        if g < Job.BunchMs then { bunches += 1; gap += g }
      lastSeen.update(key, r.ts)
      last.update(key, r.ts)

    private def bump(m: mutable.LongMap[Cell], id: Long, delay: Int): Unit =
      val c = m.getOrElse(id, null)
      if c == null then m.update(id, new Cell(1L, delay.toLong, delay))
      else
        c.n += 1L
        c.sum += delay.toLong
        if delay > c.max then c.max = delay

    /**
     * Take a LATER slice's fold into this one. Panes add; the bunching
     * seam is the one thing that is not a sum, and it is why this is
     * ordered: the pair that falls between the two slices is this
     * fold's last time for a key against the other's first.
     */
    def absorb(o: Fold): Unit =
      absorbCells(route, o.route)
      absorbCells(stop, o.stop)
      bunches += o.bunches
      gap += o.gap
      o.first.foreachKey { k =>
        val prev = last.getOrElse(k, Long.MinValue)
        if prev != Long.MinValue then
          val g = Math.abs(o.first(k) - prev)
          if g < Job.BunchMs then { bunches += 1; gap += g }
      }
      o.first.foreachKey(k => if !first.contains(k) then first.update(k, o.first(k)))
      o.last.foreachKey(k => last.update(k, o.last(k)))

    private def absorbCells(into: mutable.LongMap[Cell], from: mutable.LongMap[Cell]): Unit =
      from.foreachKey { id =>
        val b = from(id)
        val a = into.getOrElse(id, null)
        if a == null then into.update(id, new Cell(b.n, b.sum, b.max))
        else
          a.n += b.n
          a.sum += b.sum
          if b.max > a.max then a.max = b.max
      }

    /**
     * The run's answer. Stage 5 is here rather than in the fold for
     * the same reason it is cheap: the ranking needs the FINISHED
     * panes, and with no eviction they are all finished at the end —
     * so it is a sort per window over the tram routes in it, which is
     * what a user without a top-k combinator writes.
     */
    def result: Job.Result =
      var routeWins = 0L; var routeEvents = 0L; var routeDelay = 0L; var routeHash = 0L
      val ranked = mutable.LongMap.empty[mutable.ArrayBuffer[(Int, Job.Stats)]]
      route.foreachKey { id =>
        val c = route(id)
        val start = (id >>> 20) * Job.WindowMs
        val key = (id & 0xfffffL).toInt
        routeWins += 1; routeEvents += c.n; routeDelay += c.sum
        routeHash ^= Job.hash(start, key.toLong, c.n, c.sum, c.max.toLong)
        if tram(key) then
          ranked.getOrElseUpdate(start, mutable.ArrayBuffer.empty)
            .addOne((key, Job.Stats(c.n, c.sum, c.max))): Unit
      }

      var stopWins = 0L; var stopEvents = 0L; var stopHash = 0L
      stop.foreachKey { id =>
        val c = stop(id)
        val start = (id >>> 20) * Job.SlideMs
        val key = (id & 0xfffffL).toInt
        stopWins += 1; stopEvents += c.n
        stopHash ^= Job.hash(start, key.toLong, c.n, c.sum, c.max.toLong)
      }

      var topWins = 0L; var topHash = 0L
      ranked.foreachKey { start =>
        topWins += 1
        topHash ^= Job.topHash(start,
          ranked(start).sortWith((a, b) => Job.ranking.gt(a, b)).take(Job.K).toSeq)
      }

      Job.Result(routeWins, routeEvents, routeDelay, routeHash,
        stopWins, stopEvents, stopHash, bunches, gap, topWins, topHash)
  }

  /** stage 1 as free functions too: a `java.util.stream` applies the
   * filter and the map OUTSIDE the container it collects into, so they
   * cannot be methods of the fold there */
  def known(tram: Array[Boolean], d: Depart): Boolean = d.route >= 0 && d.route < tram.length

  def enrich(tram: Array[Boolean], d: Depart): Ride =
    new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route))

  /** the routes table as the flag stage 1 reads (`.iterator` because
   * `okay.given` puts a `map` in scope that an Array would pick up) */
  def tramTable(feed: Feed): Array[Boolean] = feed.routes.iterator.map(_.tram).toArray

  /** where slice `i` of `lanes` starts — contiguous ranges of the
   * ARRIVAL order, so every lane cuts the stream the same way */
  def bound(events: Int, lanes: Int, i: Int): Int = (events.toLong * i / lanes).toInt

  /** the ordered reduction every parallel lane finishes with */
  def combine(parts: Seq[Fold]): Job.Result =
    val head = parts.head
    for p <- parts.tail do head.absorb(p)
    head.result
}
