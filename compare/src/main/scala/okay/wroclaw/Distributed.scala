package okay.wroclaw

import okay.{Aggregator, Pane, Sequential}
import okay.codec.Schema
import okay.cluster.{Flow, Job as Submitted, Jobs, Wire}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * STAGE 4 AS ALGEBRA, SAID ONCE (specs/dataflow.md, Claim 2).
 *
 * Bus bunching is keyed STATE and depends on per-key order, which is
 * why Flink answers it with a `KeyedProcessFunction` over
 * `ValueState` and therefore with a shuffle. The same question asked
 * of a SLICE has an answer that combines: the slice's ends, the
 * bunches strictly inside it, and their gaps. Merging two of them
 * asks one more question — whether the gap ACROSS the boundary is
 * short — and that question is not the same one in the other order,
 * which is exactly why this is a `Sequential` and not an
 * `Aggregator`.
 *
 * It lives in `src/main` because three lanes now need the same value:
 * the in-process acceptance (`TestWroclawFlow`), the measurement, and
 * the DISTRIBUTED job, whose workers build it from a name.
 */
object Bunching {
  final case class Runs(first: Long, last: Long, n: Long, bunches: Long, gap: Long) derives Schema

  val algebra: Sequential[Ride, Runs, (Long, Long)] = new Sequential[Ride, Runs, (Long, Long)]:
    def init: Runs = Runs(0, 0, 0, 0, 0)
    def add(a: Runs, r: Ride): Runs =
      if a.n == 0 then Runs(r.ts, r.ts, 1, 0, 0)
      else
        val g = Math.abs(r.ts - a.last)
        if g < Job.BunchMs then Runs(a.first, r.ts, a.n + 1, a.bunches + 1, a.gap + g)
        else Runs(a.first, r.ts, a.n + 1, a.bunches, a.gap)
    def merge(a: Runs, b: Runs): Runs =
      if a.n == 0 then b else if b.n == 0 then a
      else
        val g = Math.abs(b.first - a.last)
        val hit = if g < Job.BunchMs then 1L else 0L
        val add = if hit == 1L then g else 0L
        Runs(a.first, b.last, a.n + b.n, a.bunches + b.bunches + hit, a.gap + b.gap + add)
    def present(a: Runs): (Long, Long) = (a.bunches, a.gap)

  /** the key: one route leaving one stop */
  val key: Ride => Long = r => (r.route.toLong << 20) | r.stop.toLong

  val totals: Aggregator[(Long, (Long, Long)), (Long, Long), (Long, Long)] =
    Aggregator[(Long, (Long, Long)), (Long, Long), (Long, Long)]((0L, 0L))((t, kv) =>
      (t._1 + kv._2._1, t._2 + kv._2._2))((a, b) => (a._1 + b._1, a._2 + b._2))(identity)
}

/**
 * WROCŁAW'S JOB AS SOMETHING A WORKER PROCESS CAN BE ASKED FOR
 * (specs/dataflow.md, stage 7).
 *
 * Everything §20 measures in one JVM, submitted by NAME to workers
 * that build the plan themselves from one parameter — how many
 * service days. No closure crosses the wire, no data crosses on the
 * way in, and what crosses on the way out is three partials whose
 * Schemas are written here and nowhere else.
 *
 * WHAT HAD TO CHANGE TO GET HERE, and it is worth saying because the
 * answer is "the terminals, not the job". `Job.stats`' accumulator is
 * a tuple tree, so the wire form uses `Job.summaryStats` — the same
 * three numbers in four flat longs, which §20 already prices and
 * `TestNativeLanes` already asserts identical. And `OkayLane.Sink`,
 * the assembly every in-process lane shares, is a mutable class with
 * a `LongMap` in it: a partition's terminal has to be a VALUE to
 * cross, so the two tallies below are that assembly written as one.
 * The eleven checksums are then asserted equal to `OkayLane.run`'s,
 * which is the bar §20 holds every other engine to.
 */
object Distributed {

  given Schema[Aggregator.Summary] = Schema.derived

  /**
   * ONE ROUTE'S STANDING IN ONE WINDOW, as it travels.
   *
   * `Aggregator.topK`'s accumulator IS the k greatest in the
   * ranking's own descending order, so a flat vector of these
   * round-trips it faithfully — the window they belong to is the key
   * beside them.
   */
  final case class Top(start: Long, route: Int, n: Long, sum: Long, max: Int) derives Schema

  /** the route stage's terminal, as it travels: stage 2's counters
   * and hash, and stage 5's ranking accumulators beside them */
  final case class RouteWire(wins: Long, events: Long, delay: Long, hash: Long,
                             ranked: Vector[Top]) derives Schema

  /** the stop stage's terminal, as it travels */
  final case class StopWire(wins: Long, events: Long, hash: Long) derives Schema

  /**
   * THE ROUTE STAGE'S TERMINAL AS THE PARTITION HOLDS IT — mutable,
   * with a `LongMap` for the ranking, exactly as `OkayLane.Sink` is.
   *
   * A terminal is added to once per PANE (102 442 of them here) and
   * merged a handful of times, so its accumulator wants to be a
   * mutable object and its WIRE FORM wants to be a value. `Schema`
   * says both with `SIso`: the accumulator travels as `RouteWire` and
   * every algebra sees only that.
   *
   * The first version made the accumulator itself the value, with the
   * ranking as a `Vector` compacted when it passed a threshold — and
   * the threshold (4 096) was BELOW the compacted size (1 152 windows
   * x 5), so it compacted on every add and the lane read 10x its own
   * in-process reference. Written down because the number looked like
   * a distribution cost and was arithmetic.
   */
  final class RouteAcc:
    var wins: Long = 0L
    var events: Long = 0L
    var delay: Long = 0L
    var hash: Long = 0L
    val ranked: mutable.LongMap[List[(Int, Job.Stats)]] =
      mutable.LongMap.empty[List[(Int, Job.Stats)]]

    def absorb(o: RouteAcc): Unit =
      wins += o.wins; events += o.events; delay += o.delay; hash ^= o.hash
      o.ranked.foreachKey { start =>
        ranked.update(start,
          ranked.get(start).fold(o.ranked(start))(Job.top.merge(_, o.ranked(start))))
      }

  final class StopAcc:
    var wins: Long = 0L
    var events: Long = 0L
    var hash: Long = 0L
    def absorb(o: StopAcc): Unit = { wins += o.wins; events += o.events; hash ^= o.hash }

  private def routeOut(a: RouteAcc): RouteWire =
    val b = Vector.newBuilder[Top]
    a.ranked.foreachKey { start =>
      for (route, s) <- a.ranked(start) do b += Top(start, route, s.n, s.sum, s.max)
    }
    RouteWire(a.wins, a.events, a.delay, a.hash, b.result())

  private def routeIn(w: RouteWire): RouteAcc =
    val a = new RouteAcc
    a.wins = w.wins; a.events = w.events; a.delay = w.delay; a.hash = w.hash
    for t <- w.ranked do
      val e = (t.route, Job.Stats(t.n, t.sum, t.max))
      a.ranked.update(t.start, Job.top.merge(a.ranked.getOrElse(t.start, Job.top.init), List(e)))
    a

  /** the accumulator travels as its wire form, and nothing else in
   * the engine knows the difference (codec-iso) */
  given Schema[RouteAcc] =
    Schema.SIso[RouteAcc, RouteWire](() => summon[Schema[RouteWire]], w => Right(routeIn(w)), routeOut)()

  given Schema[StopAcc] =
    Schema.SIso[StopAcc, StopWire](() => summon[Schema[StopWire]],
      w => Right({ val a = new StopAcc; a.wins = w.wins; a.events = w.events; a.hash = w.hash; a }),
      a => StopWire(a.wins, a.events, a.hash))()

  /** the ranking's own answer */
  def topOf(a: RouteAcc): (Long, Long) =
    var wins = 0L
    var hash = 0L
    a.ranked.foreachKey { start =>
      wins += 1
      hash ^= Job.topHash(start, Job.top.present(a.ranked(start)))
    }
    (wins, hash)

  /**
   * Stage 2's terminal, and stage 5 fed from it — `OkayLane.Sink`'s
   * `route` and `stop` written as the two aggregators the engine
   * asks for. NOT `Aggregator.apply`: its `z` is by-value, so `init`
   * would hand every partition the SAME accumulator.
   */
  def routeInto(tram: Array[Boolean]): Aggregator[Pane[Int, Job.Stats], RouteAcc, RouteAcc] =
    new Aggregator[Pane[Int, Job.Stats], RouteAcc, RouteAcc]:
      def init: RouteAcc = new RouteAcc
      def add(a: RouteAcc, p: Pane[Int, Job.Stats]): RouteAcc =
        val s = p.value
        a.wins += 1; a.events += s.n; a.delay += s.sum
        a.hash ^= Job.hash(p.start, p.key.toLong, s.n, s.sum, s.max.toLong)
        if tram(p.key) then
          a.ranked.update(p.start,
            Job.top.add(a.ranked.getOrElse(p.start, Job.top.init), (p.key, s)))
        a
      def merge(a: RouteAcc, b: RouteAcc): RouteAcc = { a.absorb(b); a }
      def present(a: RouteAcc): RouteAcc = a

  val stopInto: Aggregator[Pane[Int, Job.Stats], StopAcc, StopAcc] =
    new Aggregator[Pane[Int, Job.Stats], StopAcc, StopAcc]:
      def init: StopAcc = new StopAcc
      def add(a: StopAcc, p: Pane[Int, Job.Stats]): StopAcc =
        val s = p.value
        a.wins += 1; a.events += s.n
        a.hash ^= Job.hash(p.start, p.key.toLong, s.n, s.sum, s.max.toLong)
        a
      def merge(a: StopAcc, b: StopAcc): StopAcc = { a.absorb(b); a }
      def present(a: StopAcc): StopAcc = a

  /** what a run answers, before the eleven checksums are assembled */
  type Answer = ((RouteAcc, StopAcc), (Long, Long))

  def assemble(v: Answer): Job.Result =
    val ((route, stop), (bunches, gap)) = v
    val (topWins, topHash) = topOf(route)
    Job.Result(route.wins, route.events, route.delay, route.hash,
      stop.wins, stop.events, stop.hash, bunches, gap, topWins, topHash)

  /**
   * THE FEED, BUILT ONCE PER PROCESS AND NOT CHARGED TO THE ENGINE.
   *
   * A worker is told which partition of how many and derives it from
   * the parameters — here by parsing 1.16 million GTFS stop times and
   * replaying them as events, which takes seconds and is the
   * BENCHMARK'S data, not the engine's work. A real deployment reads
   * a file range or a topic offset; the memo is what stands in for
   * the data already being on the machine. The measurement warms
   * every worker before it times anything, so no row pays for it.
   */
  private val feeds = mutable.HashMap.empty[Int, (Feed, Array[Boolean])]

  def feed(days: Int): (Feed, Array[Boolean]) = synchronized {
    feeds.getOrElseUpdate(days, {
      val f = Gtfs.events(days)
      (f, f.routes.iterator.map(_.tram).toArray)
    })
  }
}

/** the parameters a submission carries: how many service days to
 * replay. One `Int` — the data itself never crosses. */
final case class Days(days: Int) derives Schema

/**
 * The whole job — three keyed stages over one pass — as a
 * `Job[Days, Answer]`. `Jobs.register(WroclawJob)` is what a worker
 * process runs to know it (`WroclawJobs`, below).
 */
object WroclawJob extends Submitted[Days, Distributed.Answer] {
  import Distributed.given

  type A = Ride

  def name: String = "wroclaw.whole"
  def params: Schema[Days] = summon[Schema[Days]]

  def flow(d: Days, parts: Int): Flow[Ride] =
    val (f, tram) = Distributed.feed(d.days)
    Flow.slices(ArraySeq.unsafeWrapArray(f.events), parts)
      .filter(e => e.route >= 0 && e.route < tram.length)
      .map(e => new Ride(e.ts, e.route, e.stop, e.vehicle, e.delay, tram(e.route)))

  def sink(d: Days): Wire[Ride, Distributed.Answer] =
    val (_, tram) = Distributed.feed(d.days)
    Wire.tumbling(Job.WindowMs, Job.Lateness,
      (r: Ride) => r.route, (r: Ride) => r.ts, Job.summaryStats)(Distributed.routeInto(tram))
      .and(Wire.sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness,
        (r: Ride) => r.stop, (r: Ride) => r.ts, Job.summaryStats)(Distributed.stopInto))
      .and(Wire.keyed(Bunching.key, Bunching.algebra)(Bunching.totals))
}

/**
 * THE THREE STAGES, SUBMITTABLE ONE AT A TIME.
 *
 * The whole job's partial is one value and weighing it says how much
 * crosses, not WHAT. These three answer that: the same flow, one
 * stage each, so the bytes and the accumulator counts can be read per
 * stage — and stage 4's row is Claim 2's own number, the keyed state
 * that crosses as accumulators rather than as records.
 */
object WroclawRouteJob extends Submitted[Days, Distributed.RouteAcc] {
  import Distributed.given
  type A = Ride
  def name: String = "wroclaw.route"
  def params: Schema[Days] = summon[Schema[Days]]
  def flow(d: Days, parts: Int): Flow[Ride] = WroclawJob.flow(d, parts)
  def sink(d: Days): Wire[Ride, Distributed.RouteAcc] =
    Wire.tumbling(Job.WindowMs, Job.Lateness, (r: Ride) => r.route, (r: Ride) => r.ts,
      Job.summaryStats)(Distributed.routeInto(Distributed.feed(d.days)._2))
}

object WroclawStopJob extends Submitted[Days, Distributed.StopAcc] {
  import Distributed.given
  type A = Ride
  def name: String = "wroclaw.stop"
  def params: Schema[Days] = summon[Schema[Days]]
  def flow(d: Days, parts: Int): Flow[Ride] = WroclawJob.flow(d, parts)
  def sink(d: Days): Wire[Ride, Distributed.StopAcc] =
    Wire.sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness,
      (r: Ride) => r.stop, (r: Ride) => r.ts, Job.summaryStats)(Distributed.stopInto)
}

object WroclawBunchJob extends Submitted[Days, (Long, Long)] {
  type A = Ride
  def name: String = "wroclaw.bunch"
  def params: Schema[Days] = summon[Schema[Days]]
  def flow(d: Days, parts: Int): Flow[Ride] = WroclawJob.flow(d, parts)
  def sink(d: Days): Wire[Ride, (Long, Long)] =
    Wire.keyed(Bunching.key, Bunching.algebra)(Bunching.totals)
}

/** what a Wrocław worker process knows how to run — the class
 * `WorkerMain` is handed on its command line */
object WroclawJobs {
  Jobs.register(WroclawJob)
  Jobs.register(WroclawRouteJob)
  Jobs.register(WroclawStopJob)
  Jobs.register(WroclawBunchJob)
  def install(): Unit = ()
}
