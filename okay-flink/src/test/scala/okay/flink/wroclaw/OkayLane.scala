package okay.flink.wroclaw

import okay.*
import okay.given
import scala.collection.mutable

/**
 * THE OKAY LANE: the same job, in one JVM, with no engine under it.
 *
 * WHAT CHANGED SINCE THE FIRST §20 (stream-event-time-window,
 * 2026-09-10). The first version of this file carried fifty lines of
 * keyed panes, a watermark and an eviction sweep, and the section said
 * so: Flink's stage 2 is one line and okay's was fifty, because the
 * core had nowhere to put them. It has now — `okay.Windows`
 * (specs/event-time-windows.md) — so the lane below is the job and
 * nothing else, and the fifty lines survive only as `Packed`, the
 * baseline the general operator is measured against.
 *
 * The pass is single-threaded and single-pass: the fan-out to three
 * consumers costs nothing, because a fan-out in one JVM is calling
 * three methods with the same reference. In Flink the same fan-out is
 * three shuffles. That asymmetry is not a flaw of the benchmark, it
 * IS the difference between the two things being compared, and the
 * report says so rather than hiding it.
 */
object OkayLane {

  /**
   * The four result streams, folded into the run's checksums. It is a
   * class rather than four `var`s in a method because three drivers
   * below share it and the numbers must be produced identically by
   * each — a lane that computes a different answer measures nothing.
   */
  private final class Sink(tram: Array[Boolean]) {
    private var routeWins = 0L; private var routeEvents = 0L
    private var routeDelay = 0L; private var routeHash = 0L
    private var stopWins = 0L; private var stopEvents = 0L; private var stopHash = 0L
    private var bunches = 0L; private var bunchGap = 0L
    private val ranked = mutable.LongMap.empty[List[(Int, Job.Stats)]]

    /** stage 2's pane, and stage 5 fed from it */
    val route: Pane[Int, Job.Stats] => Unit = p =>
      val s = p.value
      routeWins += 1; routeEvents += s.n; routeDelay += s.sum
      routeHash ^= Job.hash(p.start, p.key.toLong, s.n, s.sum, s.max.toLong)
      if tram(p.key) then
        ranked.update(p.start, Job.top.add(ranked.getOrElse(p.start, Job.top.init), (p.key, s)))

    /** stage 3's pane */
    val stop: Pane[Int, Job.Stats] => Unit = p =>
      val s = p.value
      stopWins += 1; stopEvents += s.n
      stopHash ^= Job.hash(p.start, p.key.toLong, s.n, s.sum, s.max.toLong)

    /** stage 4's detection */
    def bunch(gap: Long): Unit = { bunches += 1; bunchGap += gap }

    /** take another slice's sink into this one: counters add, hashes
     * XOR (they are order-independent by construction), and the
     * ranking accumulators merge — which is `Aggregator.merge` doing
     * across threads what it does across panes */
    def absorb(o: Sink): Unit =
      routeWins += o.routeWins; routeEvents += o.routeEvents
      routeDelay += o.routeDelay; routeHash ^= o.routeHash
      stopWins += o.stopWins; stopEvents += o.stopEvents; stopHash ^= o.stopHash
      bunches += o.bunches; bunchGap += o.bunchGap
      o.ranked.foreachKey { start =>
        ranked.update(start, ranked.get(start).fold(o.ranked(start))(Job.top.merge(_, o.ranked(start))))
      }

    def result: Job.Result =
      var topWins = 0L; var topHash = 0L
      ranked.foreachKey { start =>
        topWins += 1
        topHash ^= Job.topHash(start, Job.top.present(ranked(start)))
      }
      Job.Result(routeWins, routeEvents, routeDelay, routeHash,
        stopWins, stopEvents, stopHash, bunches, bunchGap, topWins, topHash)
  }

  /** the routes table as the flag the job actually reads. `.iterator`
   * on purpose: `import okay.given` puts a `map` in scope that an
   * Array picks up, and the lambda's parameter then types as the array */
  private def tramTable(feed: Feed): Array[Boolean] = feed.routes.iterator.map(_.tram).toArray

  /** stage 1, shared by every driver: the map-side join, as the source's own map */
  private def rides(feed: Feed, tram: Array[Boolean], chunk: Int): Chunks[Ride] =
    val source = Chunks.fromIterator(feed.events.iterator, chunk)
    Chunks.map(Chunks.filter(source)(d => d.route >= 0 && d.route < tram.length))(d =>
      new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))

  /** stage 4, shared: when this route last left this stop */
  private inline def bunching(last: mutable.LongMap[Long], r: Ride, sink: Sink): Unit =
    val key = (r.route.toLong << 20) | r.stop.toLong
    val prev = last.getOrElse(key, Long.MinValue)
    if prev != Long.MinValue then
      val gap = Math.abs(r.ts - prev) // arrival order, not event order: |gap|
      if gap < Job.BunchMs then sink.bunch(gap)
    last.update(key, r.ts)

  /**
   * The lane as a user would now write it: two `okay.Windows`, a map
   * for the keyed state, one pass.
   */
  def run(feed: Feed, chunk: Int = 256): Job.Result = {
    val tram = tramTable(feed)
    val sink = new Sink(tram)
    val routeWindows = Windows.tumbling[Int, Ride, Job.Acc, Job.Stats](
      Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)
    val stopWindows = Windows.sliding[Int, Ride, Job.Acc, Job.Stats](
      Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats)
    val lastSeen = mutable.LongMap.empty[Long]

    Chunks.foldLeft(rides(feed, tram, chunk))(())((_, r) => {
      routeWindows.add(r)(sink.route)
      stopWindows.add(r)(sink.stop)
      bunching(lastSeen, r, sink)
      ()
    })
    routeWindows.close()(sink.route)
    stopWindows.close()(sink.stop)
    sink.result
  }

  /**
   * The same job over the operator this benchmark carried BEFORE the
   * core had one: panes under a key packed as `(window << 20) | key`,
   * which is one hash lookup instead of two and is only possible
   * because a route and a (dense) stop index both fit in twenty bits.
   *
   * It is kept for one reason — specs/event-time-windows.md says the
   * general shape is measured against it rather than assumed equal —
   * and the driver is duplicated rather than abstracted, because an
   * interface over the hot path would measure the interface.
   */
  def packed(feed: Feed, chunk: Int = 256): Job.Result = {
    val tram = tramTable(feed)
    val sink = new Sink(tram)
    val routeWindows = new Packed(Job.WindowMs, Job.WindowMs, sink.route)
    val stopWindows = new Packed(Job.SlideWindowMs, Job.SlideMs, sink.stop)
    val lastSeen = mutable.LongMap.empty[Long]
    var maxTs = Long.MinValue

    Chunks.foldLeft(rides(feed, tram, chunk))(())((_, r) => {
      if r.ts > maxTs then maxTs = r.ts
      routeWindows.add(r.route, r)
      stopWindows.add(r.stop, r)
      bunching(lastSeen, r, sink)
      val watermark = maxTs - Job.Lateness
      routeWindows.advance(watermark)
      stopWindows.advance(watermark)
      ()
    })
    routeWindows.close()
    stopWindows.close()
    sink.result
  }

  /** the pre-core operator, kept only as the measured baseline */
  private final class Packed(size: Long, slide: Long, emit: Pane[Int, Job.Stats] => Unit) {
    private val agg = Job.stats
    private val panes = mutable.LongMap.empty[Job.Acc]
    private val closing = mutable.ArrayBuffer.empty[Long]
    private val panesPer = (size / slide).toInt
    private var nextSweep = Long.MinValue

    def add(k: Int, ride: Ride): Unit =
      var start = ride.ts - Math.floorMod(ride.ts, slide) - size + slide
      var i = 0
      while i < panesPer do
        val id = ((start / slide) << 20) | k.toLong
        panes.update(id, agg.add(panes.getOrElse(id, agg.init), ride))
        start += slide
        i += 1

    def advance(watermark: Long): Unit =
      if watermark >= nextSweep then
        sweep(watermark)
        nextSweep = watermark - Math.floorMod(watermark, slide) + slide

    def close(): Unit = sweep(Long.MaxValue)

    private def sweep(watermark: Long): Unit =
      panes.foreachKey(id => if (id >>> 20) * slide + size <= watermark then closing += id)
      var i = 0
      while i < closing.length do
        val id = closing(i)
        val start = (id >>> 20) * slide
        panes.remove(id).foreach(a =>
          emit(Pane(start, start + size, (id & 0xfffffL).toInt, agg.present(a))))
        i += 1
      closing.clear()
  }

  /**
   * ONE STAGE, THREE ROADS — stage 2 alone (the tumbling windows per
   * route), because a `Stage` is LINEAR and the whole job is a
   * fan-out: three consumers of one pass, which a coroutine pairing
   * cannot express and a method call can.
   *
   * The three roads are chosen so that each difference means one
   * thing. `chunks` is the road a user takes: the class, driven by a
   * `Chunks` fold. `elementwise` is the same class driven by a
   * per-element `Writer` producer — so the delta to `chunks` is what
   * the producer costs, not what the operator costs. `stage` is
   * `Windows.stage` under `through`, driven by the SAME producer — so
   * the delta to `elementwise` is exactly the Take/Writer coroutine,
   * which is the number the composable form is actually judged on.
   */
  // Writer.fold's inline body checks the told value's type at an
  // abstract type — the trusted kernel's warning (Effects.scala), the
  // same one TestPhased silences at the same call
  @scala.annotation.nowarn("msg=cannot be checked at runtime")
  def routeWindowsOnly(feed: Feed, road: String, chunk: Int = 256): Long = {
    val tram = tramTable(feed)
    var wins = 0L
    val count: Pane[Int, Job.Stats] => Unit = _ => wins += 1

    road match
      case "chunks" =>
        val w = Windows.tumbling[Int, Ride, Job.Acc, Job.Stats](
          Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)
        Chunks.foldLeft(rides(feed, tram, chunk))(())((_, r) => { w.add(r)(count); () })
        w.close()(count)

      case "elementwise" =>
        val w = Windows.tumbling[Int, Ride, Job.Acc, Job.Stats](
          Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)
        given Fold[Ride, Long] = Fold(0L)((n: Long, r: Ride) => { w.add(r)(count); n + 1L })
        !.run(Writer.fold(produce(feed, tram))): Unit
        w.close()(count)

      case _ =>
        val st = Windows.stage[Int, Ride, Job.Acc, Job.Stats](
          Job.WindowMs, Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)
        given Fold[Pane[Int, Job.Stats], Long] =
          Fold(0L)((n: Long, _: Pane[Int, Job.Stats]) => n + 1L)
        wins = !.run(Writer.fold(through(produce(feed, tram))(st)))._1
    wins
  }

  /** stage 1 as a per-element producer: what the composable roads are
   * driven by, so that both of them pay for it and the difference
   * between them is the coroutine alone */
  private def produce(feed: Feed, tram: Array[Boolean]): Unit ! Writer % Ride =
    def go(i: Int): Unit ! Writer % Ride =
      if i >= feed.events.length then pure(())
      else
        val d = feed.events(i)
        if d.route >= 0 && d.route < tram.length then
          Writer.tell(new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))
            .flatMap(_ => go(i + 1))
        else go(i + 1)
    go(0)

  /**
   * THE PARALLEL LANE — parallelism by MERGE, not by shuffle.
   *
   * §20's okay lane is one thread, so its lead over Flink-on-four-cores
   * is per EVENT, not per box. This is the other number, and the way it
   * is reached is the point: an `Aggregator` carries `merge` precisely
   * so partial results combine, and a window's panes are aggregates. So
   * the events are cut into P contiguous slices of the ARRIVAL order,
   * each slice folds with its own `Windows`, and the pieces are put
   * together at the end. No shuffle, no serialization, no network — the
   * only thing crossing a thread boundary is one accumulator per pane
   * that spans a slice boundary.
   *
   * WHICH PANES SPAN A BOUNDARY is not guessed. One cheap pass first
   * records, per slice, the greatest event time in it (`hi`, monotone
   * because arrival order is nearly event order) and, globally, the
   * greatest BACKWARDNESS `b` — how far an event's time can fall below
   * the greatest seen before it. Then, in slice i:
   *
   *   - no EARLIER slice can have touched a window starting after
   *     `hi(i-1)`, because every earlier event's time is at most that;
   *   - no LATER slice can touch a window ending at or before
   *     `hi(i) - b`, because every later event's time is at least that.
   *
   * A pane satisfying both is COMPLETE in its slice and is folded
   * there. Everything else — a handful per boundary — is handed back as
   * an accumulator and merged by the coordinator. The suite asserts the
   * result EQUALS the single-threaded lane's, which is what keeps this
   * reasoning honest.
   */
  def parallel(feed: Feed, lanes: Int, chunk: Int = 256): Job.Result = {
    val tram = tramTable(feed)
    val events = feed.events
    val n = events.length
    val bounds = (0 to lanes).map(i => (n.toLong * i / lanes).toInt).toArray

    // pass 1: the slice maxima and the greatest backwardness
    val hi = new Array[Long](lanes)
    var runningMax = Long.MinValue
    var back = 0L
    var i = 0
    while i < lanes do
      var j = bounds(i)
      while j < bounds(i + 1) do
        val ts = events(j).ts
        if ts > runningMax then runningMax = ts else if runningMax - ts > back then back = runningMax - ts
        j += 1
      hi(i) = runningMax
      i += 1

    // pass 2: one fold per slice, in parallel
    val parts = (0 until lanes).map { s =>
      Async.spawn(async(slice(feed, tram, bounds(s), bounds(s + 1), s, hi, back, chunk)))
    }.map(_.join())

    // the coordinator: fold what was complete, merge what was not
    val sink = parts.head.sink
    for p <- parts.tail do sink.absorb(p.sink)
    merged(parts.map(_.route), Job.WindowMs)((start, key, acc) =>
      sink.route(Pane(start, start + Job.WindowMs, key, Job.stats.present(acc))))
    merged(parts.map(_.stop), Job.SlideMs)((start, key, acc) =>
      sink.stop(Pane(start, start + Job.SlideWindowMs, key, Job.stats.present(acc))))

    // the bunching pairs that fall between two slices: for each key,
    // the last time in an earlier slice against the first in the next
    val running = mutable.LongMap.empty[Long]
    for p <- parts do
      p.first.foreachKey { k =>
        running.get(k).foreach { prev =>
          val gap = Math.abs(p.first(k) - prev)
          if gap < Job.BunchMs then sink.bunch(gap)
        }
      }
      p.last.foreachKey(k => running.update(k, p.last(k)))
    sink.result
  }

  /** what one slice hands back */
  private final class Part(val sink: Sink,
                           val route: mutable.LongMap[Job.Acc],
                           val stop: mutable.LongMap[Job.Acc],
                           val first: mutable.LongMap[Long],
                           val last: mutable.LongMap[Long])

  /** the same aggregator, presenting its ACCUMULATOR — what a partial
   * pane must carry so that another slice's can be merged into it */
  private val partialStats: Aggregator[Ride, Job.Acc, Job.Acc] =
    Aggregator[Ride, Job.Acc, Job.Acc](Job.stats.init)(Job.stats.add)(Job.stats.merge)(identity)

  private def slice(feed: Feed, tram: Array[Boolean], from: Int, until: Int, index: Int,
                    hi: Array[Long], back: Long, chunk: Int): Part = {
    val sink = new Sink(tram)
    val partialRoute = mutable.LongMap.empty[Job.Acc]
    val partialStop = mutable.LongMap.empty[Job.Acc]
    val first = mutable.LongMap.empty[Long]
    val last = mutable.LongMap.empty[Long]
    val lower = if index == 0 then Long.MinValue else hi(index - 1)
    val upper = hi(index) - back

    def keep(partial: mutable.LongMap[Job.Acc], slide: Long)(p: Pane[Int, Job.Acc]): Unit =
      val id = ((p.start / slide) << 20) | p.key.toLong
      partial.update(id, partial.get(id).fold(p.value)(Job.stats.merge(_, p.value)))

    val routeWindows = Windows.tumbling[Int, Ride, Job.Acc, Job.Acc](
      Job.WindowMs, Job.Lateness)(_.route)(_.ts)(partialStats)
    val stopWindows = Windows.sliding[Int, Ride, Job.Acc, Job.Acc](
      Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(partialStats)

    val onRoute: Pane[Int, Job.Acc] => Unit = p =>
      if p.start > lower && p.end <= upper then
        sink.route(Pane(p.start, p.end, p.key, Job.stats.present(p.value)))
      else keep(partialRoute, Job.WindowMs)(p)
    val onStop: Pane[Int, Job.Acc] => Unit = p =>
      if p.start > lower && p.end <= upper then
        sink.stop(Pane(p.start, p.end, p.key, Job.stats.present(p.value)))
      else keep(partialStop, Job.SlideMs)(p)

    val lastSeen = mutable.LongMap.empty[Long]
    val source = Chunks.fromIterator(feed.events.iterator.slice(from, until), chunk)
    val rs = Chunks.map(Chunks.filter(source)(d => d.route >= 0 && d.route < tram.length))(d =>
      new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))

    Chunks.foldLeft(rs)(())((_, r) => {
      routeWindows.add(r)(onRoute)
      stopWindows.add(r)(onStop)
      val key = (r.route.toLong << 20) | r.stop.toLong
      val prev = lastSeen.getOrElse(key, Long.MinValue)
      if prev == Long.MinValue then first.update(key, r.ts)
      else
        val gap = Math.abs(r.ts - prev)
        if gap < Job.BunchMs then sink.bunch(gap)
      lastSeen.update(key, r.ts)
      last.update(key, r.ts)
      ()
    })
    routeWindows.close()(onRoute)
    stopWindows.close()(onStop)
    new Part(sink, partialRoute, partialStop, first, last)
  }

  /** merge one pane map across the slices, then hand each merged
   * accumulator to the sink — this is `Aggregator.merge` doing the job
   * a shuffle does in an engine */
  private def merged(maps: Seq[mutable.LongMap[Job.Acc]], slide: Long)
                    (emit: (Long, Int, Job.Acc) => Unit): Unit =
    val all = mutable.LongMap.empty[Job.Acc]
    for m <- maps do m.foreachKey { id =>
      all.update(id, all.get(id).fold(m(id))(Job.stats.merge(_, m(id))))
    }
    all.foreachKey(id => emit((id >>> 20) * slide, (id & 0xfffffL).toInt, all(id)))
}
