package okay.wroclaw

import okay.Aggregator

/**
 * THE JOB, DEFINED ONCE — the part that must be the same in both
 * engines, so that what the two numbers differ by is the engine.
 *
 * Four stages over one event-time stream of departures:
 *
 *   1. ENRICH — every event is joined against the routes table (a
 *      map-side join: 139 rows, held by value in both lanes) for the
 *      tram flag, and events of unknown routes are dropped.
 *   2. TUMBLING, per route, 5 minutes — count, sum and max of the
 *      delay, computed by ONE okay `Aggregator`. This is the stage the
 *      module exists for: `FlinkInterop.toFlink(Job.stats)` hands the
 *      SAME VALUE to Flink as its `AggregateFunction`, so the
 *      arithmetic is not merely equivalent between the lanes, it is
 *      the same object graph.
 *   3. SLIDING, per stop, 15 minutes every 5 — the same aggregator
 *      again, now with each event in three panes at once.
 *   4. BUNCHING, keyed by (route, stop) — pure keyed STATE, no
 *      window: two departures of one route from one stop less than
 *      two minutes apart is the classic bus-bunching symptom. Flink
 *      answers this with a KeyedProcessFunction and ValueState; the
 *      okay lane keeps the same last-seen map. Ordering is the
 *      arrival order per key, which both engines preserve at any
 *      parallelism, so the answer is deterministic.
 *   5. TOP-K — the tumbling windows of stage 2, ranked per window by
 *      mean delay, trams only (which is what makes the enrichment
 *      load-bearing: get the join wrong and this checksum moves).
 *
 * The answer of a run is `Result`, a set of ORDER-INDEPENDENT
 * checksums: sums and XORs of per-record hashes, because a lane at
 * parallelism 4 emits the same records in a different order. Ranked
 * lists are order-SENSITIVE inside a window and hashed as such.
 */
object Job {

  /** tumbling window, per route */
  val WindowMs: Long = 5 * 60 * 1000L

  /** sliding window, per stop: 15 minutes every 5 — three panes per event */
  val SlideWindowMs: Long = 15 * 60 * 1000L
  val SlideMs: Long = 5 * 60 * 1000L

  /** how far out of order events may arrive (Gtfs jitters by up to 25 s) */
  val Lateness: Long = 30 * 1000L

  /** two departures of one route from one stop closer than this is bunching */
  val BunchMs: Long = 120 * 1000L

  /** how many routes the per-window ranking keeps */
  val K: Int = 5

  /** what a window says about its departures */
  final case class Stats(n: Long, sum: Long, max: Int) {
    def mean: Double = if n == 0 then 0.0 else sum.toDouble / n.toDouble
  }

  /** the accumulator of `stats` — a triple, because the aggregator is one */
  type Acc = ((Long, Long), Option[Int])

  /**
   * The statistic, as ALGEBRA: count zip sum zip max, contramapped
   * onto the event. `Aggregator.zip` computes all three in one pass
   * and `merge` combines panes — which is exactly Flink's
   * `AggregateFunction` contract, and `FlinkInterop.toFlink` is the
   * whole adaptation.
   */
  val stats: Aggregator[Ride, Acc, Stats] =
    Aggregator.count[Ride]
      .zip(Aggregator.sum[Long].contramap[Ride](_.delay.toLong))
      .zip(Aggregator.max[Int].contramap[Ride](_.delay))
      .map { case ((n, sum), mx) => Stats(n, sum, mx.getOrElse(Int.MinValue)) }

  /** the window a timestamp falls in, for a tumbling window */
  def windowOf(ts: Long): Long = ts - Math.floorMod(ts, WindowMs)

  /** the FIRST of the sliding windows a timestamp falls in */
  def slideFirst(ts: Long): Long =
    val last = ts - Math.floorMod(ts, SlideMs)
    last - SlideWindowMs + SlideMs

  /** how many sliding windows an event belongs to */
  val Panes: Int = (SlideWindowMs / SlideMs).toInt

  /**
   * The answer of one run: order-independent checksums of the four
   * result streams. Two lanes agree when these agree — nothing here
   * is a tolerance, because the arithmetic is integral by
   * construction (delays are whole seconds) and no event is late.
   */
  final case class Result(
    routeWins: Long, routeEvents: Long, routeDelay: Long, routeHash: Long,
    stopWins: Long, stopEvents: Long, stopHash: Long,
    bunches: Long, bunchGap: Long,
    topWins: Long, topHash: Long,
  ) {
    def merge(o: Result): Result = Result(
      routeWins + o.routeWins, routeEvents + o.routeEvents, routeDelay + o.routeDelay, routeHash ^ o.routeHash,
      stopWins + o.stopWins, stopEvents + o.stopEvents, stopHash ^ o.stopHash,
      bunches + o.bunches, bunchGap + o.bunchGap,
      topWins + o.topWins, topHash ^ o.topHash)

    override def toString: String =
      f"routeWins=$routeWins%,d events=$routeEvents%,d delay=$routeDelay%,d h=${routeHash}%016x | " +
        f"stopWins=$stopWins%,d events=$stopEvents%,d h=${stopHash}%016x | " +
        f"bunches=$bunches%,d gap=$bunchGap%,d | topWins=$topWins%,d h=${topHash}%016x"
  }

  object Result {
    val empty: Result = Result(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  /** a stable hash of one emitted record, so a lane's ORDER cannot
   * change the checksum (the results are XORed, not appended) */
  def hash(xs: Long*): Long =
    var h = 0xcbf29ce484222325L
    for x <- xs do
      h = (h ^ x) * 0x100000001b3L
      h ^= h >>> 29
    h

  /** the per-window ranking, order-SENSITIVE: rank enters the hash */
  def topHash(win: Long, ranked: Seq[(Int, Stats)]): Long =
    var h = hash(win)
    var i = 0
    for (route, s) <- ranked do
      h ^= hash(i.toLong, route.toLong, s.n, s.sum, s.max.toLong)
      i += 1
    h

  /**
   * The ranking rule, said once: the GREATEST is the worst-delayed
   * route, ties broken by the smaller route index — a total order, so
   * both engines rank identically. `Aggregator.topK` emits the k
   * greatest descending, which is the ranking the report wants.
   */
  val ranking: Ordering[(Int, Stats)] =
    Ordering.fromLessThan { (a, b) =>
      val (ra, sa) = a; val (rb, sb) = b
      if sa.mean != sb.mean then sa.mean < sb.mean else ra > rb
    }

  /** the ranking as an aggregator — the fifth use of the same triple,
   * and the second value handed to Flink through `toFlink` */
  val top: Aggregator[(Int, Stats), List[(Int, Stats)], List[(Int, Stats)]] =
    Aggregator.topK(K)(using ranking)
}
