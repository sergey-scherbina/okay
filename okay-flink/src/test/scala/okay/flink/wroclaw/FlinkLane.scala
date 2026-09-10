package okay.flink.wroclaw

import okay.wroclaw.{Depart, Feed, Job, Ride}

import org.apache.flink.api.common.accumulators.LongCounter
import org.apache.flink.api.common.eventtime.{SerializableTimestampAssigner, WatermarkStrategy}
import org.apache.flink.api.common.functions.{AggregateFunction, FilterFunction, MapFunction, OpenContext}
import org.apache.flink.api.common.state.{ValueState, ValueStateDescriptor}
import org.apache.flink.api.common.typeinfo.{TypeHint, TypeInformation, Types}
import org.apache.flink.api.java.functions.KeySelector
import org.apache.flink.configuration.{CheckpointingOptions, Configuration}
import org.apache.flink.api.connector.source.util.ratelimit.RateLimiterStrategy
import org.apache.flink.connector.datagen.source.{DataGeneratorSource, GeneratorFunction}
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.streaming.api.functions.sink.v2.DiscardingSink
import org.apache.flink.streaming.api.functions.windowing.ProcessWindowFunction
import org.apache.flink.streaming.api.windowing.assigners.{SlidingEventTimeWindows, TumblingEventTimeWindows}
import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.util.Collector
import java.time.Duration

/**
 * THE FLINK LANE: the same job, on a real engine, in a local
 * MiniCluster.
 *
 * WRITTEN IN FLINK'S OWN IDIOM, and the idiom in question is the JAVA
 * DataStream API — which is not a compromise but Flink's own advice
 * since 1.18, when the Scala API was deprecated. It could not have
 * been used here anyway: it is published for Scala 2.13 and its
 * `TypeInformation` macros do not exist for Scala 3. The consequence
 * is visible in every operator below: a Scala lambda erases the types
 * Flink's extractor would have read off a Java lambda, so each stage
 * names its `TypeInformation` explicitly.
 *
 * THE FUNCTIONS ARE TOP-LEVEL CLASSES, not anonymous ones, and that is
 * not style: an anonymous class written inside a method captures its
 * enclosing instance, and Flink serializes every function into the job
 * graph. A named top-level class captures exactly its constructor
 * arguments — here, the 139-row routes table, which is the map-side
 * join of stage 1.
 *
 * THE ARITHMETIC IS FLINK'S OWN (bench-engine-native-arithmetic).
 * This lane used to accumulate through `FlinkInterop.toFlink(Job.stats)`
 * — the very okay `Aggregator` the in-process lane folds with — and
 * that made a fine claim about the interop and a poor benchmark row:
 * a competitor's number must measure THEIR api. Worse, it was a
 * handicap nobody had noticed: the accumulator was
 * `((Long, Long), Option[Int])`, which Flink's type extractor cannot
 * read, so the window STATE went through Kryo. `RideStats` and
 * `TopFive` below are what a Flink user writes — `AggregateFunction`
 * over a POJO accumulator, mutated in place as Flink's contract
 * permits — and the eleven checksums say they compute the same
 * answer.
 *
 * `FlinkInterop.toFlink` is unchanged and still proved: an okay
 * `Aggregator` IS an `AggregateFunction`, asserted by
 * `TestWroclawStream`'s interop lane and okay-flink's own suite —
 * including serialization into a job graph, which a unit test alone
 * never reached. That claim belongs in a test, not in a table.
 */
object FlinkLane {

  /**
   * The events, handed to the source through the JVM rather than
   * through the job graph.
   *
   * `env.fromData(collection)` would serialize 2.4M events INTO the
   * job graph — that is what it is for (a handful of test elements)
   * and it is not a source anyone runs a benchmark through. A real
   * job reads Kafka; a local benchmark cannot, so the source reads
   * the same array the okay lane reads, by index, from a
   * `DataGeneratorSource` (Flink's own, non-deprecated replay
   * source). Both lanes therefore pay for the same thing: a walk over
   * an array in one JVM.
   */
  @volatile private var replay: Array[Depart] = Array.empty

  private[wroclaw] def event(i: Long): Depart = replay(i.toInt)

  /** the accumulator names the checksums come back under */
  private[wroclaw] object N {
    val RouteWins = "routeWins"; val RouteEvents = "routeEvents"
    val RouteDelay = "routeDelay"; val RouteHash = "routeHash"
    val StopWins = "stopWins"; val StopEvents = "stopEvents"; val StopHash = "stopHash"
    val Bunches = "bunches"; val BunchGap = "bunchGap"
    val TopWins = "topWins"; val TopHash = "topHash"
  }

  /**
   * One run of the job.
   *
   * @param parallelism  the engine's parallelism (the source stays at
   *                     1 — a single partition, so that the arrival
   *                     ORDER the okay lane sees is the order Flink
   *                     sees, and the bunching stage of both is the
   *                     same computation)
   * @param checkpointMs 0 for no checkpointing; otherwise the interval
   *                     at which the guarantee is paid for
   */
  def run(feed: Feed, parallelism: Int, checkpointMs: Long = 0L,
          objectReuse: Boolean = true, ratePerSecond: Long = 0L): Job.Result = {
    replay = feed.events

    val conf = new Configuration()
    // the checkpointing lane writes real files: where the guarantee is
    // configured, rather than asserted (`setCheckpointStorage` on the
    // CheckpointConfig is deprecated in 1.20 — this is its replacement)
    if checkpointMs > 0 then
      val dir = java.nio.file.Files.createTempDirectory("okay-flink-ckpt")
      dir.toFile.deleteOnExit()
      conf.set(CheckpointingOptions.CHECKPOINT_STORAGE, "filesystem")
      conf.set(CheckpointingOptions.CHECKPOINTS_DIRECTORY, dir.toUri.toString): Unit

    val env = StreamExecutionEnvironment.createLocalEnvironment(parallelism, conf)
    env.setParallelism(parallelism)
    if objectReuse then env.getConfig.enableObjectReuse(): Unit
    env.getConfig.setAutoWatermarkInterval(200L) // Flink's own default, said out loud
    if checkpointMs > 0 then env.enableCheckpointing(checkpointMs): Unit

    val depart = TypeInformation.of(classOf[Depart])
    val ride = TypeInformation.of(classOf[Ride])
    val win = TypeInformation.of(classOf[Win])
    // the accumulators are POJOs now, so Flink's extractor reads them
    // instead of falling back to Kryo (bench-engine-native-arithmetic)
    val acc = TypeInformation.of(classOf[StatsAcc])
    val stats = TypeInformation.of(new TypeHint[Job.Stats] {})
    val topAcc = TypeInformation.of(classOf[TopAcc])

    val watermarks = WatermarkStrategy
      .forBoundedOutOfOrderness[Depart](Duration.ofMillis(Job.Lateness))
      .withTimestampAssigner(new Timestamps)

    // A RATE is how this lane asks the question §20's second asymmetry
    // names: Flink's watermark generator fires every 200 ms of WALL
    // time, so a replay at full speed pushes days of event time through
    // in seconds and the engine holds panes a real deployment would
    // have closed. `RateLimiterStrategy` puts the event-time-to-
    // wall-time ratio back under control (docs/benchmarks.md, "the
    // replay, priced").
    val count = feed.events.length.toLong
    val source =
      if ratePerSecond > 0 then
        new DataGeneratorSource[Depart](new Replay, count,
          RateLimiterStrategy.perSecond(ratePerSecond.toDouble), depart)
      else new DataGeneratorSource[Depart](new Replay, count, depart)
    val events = env.fromSource(source, watermarks, "wroclaw-gtfs", depart).setParallelism(1)

    // stage 1 — the enrichment: a map-side join against the routes table.
    //
    // PINNED TO ONE, and this is the benchmark's sharpest lesson about
    // event streams rather than about either engine. A stream with ONE
    // partition, read by one reader, feeding a filter at parallelism 4
    // is REBALANCED — round-robin — and two events of one key then
    // reach the keyed operator through two racing channels. Flink's
    // per-key order guarantee is per CHANNEL, so it does not survive
    // that: measured here, the bunching stage saw 44 852 out-of-order
    // pairs at p4 against 41 at p1, and answered 451 instead of 913.
    // Pinning stage 1 to the source's parallelism keeps one channel
    // into the keyBy, which is what a partitioned source (Kafka keyed
    // by route) would have given for free. The windows and the keyed
    // state below still run at `parallelism` — the part worth
    // parallelising is the part that keeps state.
    val tram = feed.routes.map(_.tram)
    val rides = events
      .filter(new Known(tram.length)).setParallelism(1)
      .map(new Enrich(tram), ride).setParallelism(1)

    // stage 2 — tumbling, per route, by the SHARED aggregator
    val routeWindows = rides
      .keyBy(new ByRoute, Types.INT)
      .window(TumblingEventTimeWindows.of(Duration.ofMillis(Job.WindowMs)))
      .aggregate(new RideStats, new RouteWindow(tram), acc, stats, win)

    // stage 3 — sliding, per stop: three panes per event, same aggregator
    rides
      .keyBy(new ByStop, Types.INT)
      .window(SlidingEventTimeWindows.of(
        Duration.ofMillis(Job.SlideWindowMs), Duration.ofMillis(Job.SlideMs)))
      .aggregate(new RideStats, new StopWindow, acc, stats, Types.LONG)
      .sinkTo(new DiscardingSink[java.lang.Long]): Unit

    // stage 4 — keyed state, no window: the bunching detector
    rides
      .keyBy(new ByRouteStop, Types.LONG)
      .process(new Bunching, Types.LONG)
      .sinkTo(new DiscardingSink[java.lang.Long]): Unit

    // stage 5 — the ranking of stage 2's windows, trams only
    routeWindows
      .filter(new Trams)
      .keyBy(new ByWindow, Types.LONG)
      .window(TumblingEventTimeWindows.of(Duration.ofMillis(Job.WindowMs)))
      .aggregate(new TopFive, new Ranking, topAcc, topAcc, Types.LONG)
      .sinkTo(new DiscardingSink[java.lang.Long]): Unit

    val result = env.execute("wroclaw-departures")

    def n(name: String): Long = result.getAccumulatorResult[java.lang.Long](name).longValue
    Job.Result(
      n(N.RouteWins), n(N.RouteEvents), n(N.RouteDelay), n(N.RouteHash),
      n(N.StopWins), n(N.StopEvents), n(N.StopHash),
      n(N.Bunches), n(N.BunchGap), n(N.TopWins), n(N.TopHash))
  }
}

/** the replay source: index -> event, in the JVM the cluster runs in */
private final class Replay extends GeneratorFunction[java.lang.Long, Depart] {
  def map(i: java.lang.Long): Depart = FlinkLane.event(i.longValue)
}

/**
 * STAGE 2 AND 3'S ARITHMETIC, AS A FLINK USER WRITES IT
 * (bench-engine-native-arithmetic): `AggregateFunction` over a POJO
 * accumulator, mutated in place, which is what Flink's own contract
 * asks for and what its type extractor can serialize without Kryo.
 *
 * It computes what `Job.stats` computes — the equality of the eleven
 * checksums is asserted before any number is printed — but it is not
 * that value, and that is the point of the change: a competitor's row
 * must measure THEIR api. `FlinkInterop.toFlink` still says an okay
 * `Aggregator` IS an `AggregateFunction`, and `TestFlinkInterop`
 * still proves it; that claim belongs in a test, not in a table.
 */
private final class RideStats extends AggregateFunction[Ride, StatsAcc, Job.Stats] {
  def createAccumulator(): StatsAcc = new StatsAcc

  def add(r: Ride, acc: StatsAcc): StatsAcc =
    acc.n += 1L
    acc.sum += r.delay.toLong
    if r.delay > acc.max then acc.max = r.delay
    acc

  def getResult(acc: StatsAcc): Job.Stats = Job.Stats(acc.n, acc.sum, acc.max)

  def merge(a: StatsAcc, b: StatsAcc): StatsAcc =
    val out = new StatsAcc
    out.n = a.n + b.n
    out.sum = a.sum + b.sum
    out.max = if a.max > b.max then a.max else b.max
    out
}

/**
 * STAGE 5'S RANKING, likewise: five slots of primitives kept sorted
 * in the job's own total order (greatest mean first, ties to the
 * smaller route index), where the lane used to hand Flink
 * `Aggregator.topK`'s `List[(Int, Job.Stats)]` for Kryo to carry.
 */
private final class TopFive extends AggregateFunction[Win, TopAcc, TopAcc] {
  def createAccumulator(): TopAcc = new TopAcc

  def add(w: Win, acc: TopAcc): TopAcc =
    acc.offer(w.key, w.n, w.sum, w.max)
    acc

  def getResult(acc: TopAcc): TopAcc = acc

  def merge(a: TopAcc, b: TopAcc): TopAcc =
    var i = 0
    while i < b.size do
      a.offer(b.route(i), b.n(i), b.sum(i), b.max(i))
      i += 1
    a
}

/** event time is the departure's own time */
private final class Timestamps extends SerializableTimestampAssigner[Depart] {
  def extractTimestamp(d: Depart, recordTs: Long): Long = d.ts
}

/** stage 1a — a route the table knows */
private final class Known(routes: Int) extends FilterFunction[Depart] {
  def filter(d: Depart): Boolean = d.route >= 0 && d.route < routes
}

/** stage 1b — the map-side join: the whole table travels in the closure */
private final class Enrich(tram: Array[Boolean]) extends MapFunction[Depart, Ride] {
  def map(d: Depart): Ride = new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route))
}

private final class ByRoute extends KeySelector[Ride, Integer] {
  def getKey(r: Ride): Integer = r.route
}

private final class ByStop extends KeySelector[Ride, Integer] {
  def getKey(r: Ride): Integer = r.stop
}

/** the bunching key: one route at one stop */
private final class ByRouteStop extends KeySelector[Ride, java.lang.Long] {
  def getKey(r: Ride): java.lang.Long = (r.route.toLong << 20) | r.stop.toLong
}

private final class ByWindow extends KeySelector[Win, java.lang.Long] {
  def getKey(w: Win): java.lang.Long = w.win
}

private final class Trams extends FilterFunction[Win] {
  def filter(w: Win): Boolean = w.tram
}

/**
 * Stage 2's window function: it both FOLDS the checksum (into Flink
 * accumulators, which is how a number comes back from a cluster) and
 * emits the window for the ranking stage.
 */
private final class RouteWindow(tram: Array[Boolean])
  extends ProcessWindowFunction[Job.Stats, Win, Integer, TimeWindow] {
  @transient private var wins: LongCounter = scala.compiletime.uninitialized
  @transient private var events: LongCounter = scala.compiletime.uninitialized
  @transient private var delay: LongCounter = scala.compiletime.uninitialized
  @transient private var hash: XorLong = scala.compiletime.uninitialized

  override def open(ctx: OpenContext): Unit =
    wins = new LongCounter; events = new LongCounter
    delay = new LongCounter; hash = new XorLong
    getRuntimeContext.addAccumulator(FlinkLane.N.RouteWins, wins)
    getRuntimeContext.addAccumulator(FlinkLane.N.RouteEvents, events)
    getRuntimeContext.addAccumulator(FlinkLane.N.RouteDelay, delay)
    getRuntimeContext.addAccumulator(FlinkLane.N.RouteHash, hash)

  def process(route: Integer, ctx: ProcessWindowFunction[Job.Stats, Win, Integer, TimeWindow]#Context,
              in: java.lang.Iterable[Job.Stats], out: Collector[Win]): Unit =
    val s = in.iterator.next()
    val start = ctx.window.getStart
    wins.add(1L); events.add(s.n); delay.add(s.sum)
    hash.add(Job.hash(start, route.toLong, s.n, s.sum, s.max.toLong))
    // the aggregate keeps no tram flag, so the ranking stage's filter
    // needs it from the table — which travels in this function's
    // closure, exactly as it travels in stage 1's
    out.collect(new Win(start, route.intValue, s.n, s.sum, s.max, tram(route.intValue)))
}

/** stage 3's window function: it folds only — nothing follows it */
private final class StopWindow extends ProcessWindowFunction[Job.Stats, java.lang.Long, Integer, TimeWindow] {
  @transient private var wins: LongCounter = scala.compiletime.uninitialized
  @transient private var events: LongCounter = scala.compiletime.uninitialized
  @transient private var hash: XorLong = scala.compiletime.uninitialized

  override def open(ctx: OpenContext): Unit =
    wins = new LongCounter; events = new LongCounter; hash = new XorLong
    getRuntimeContext.addAccumulator(FlinkLane.N.StopWins, wins)
    getRuntimeContext.addAccumulator(FlinkLane.N.StopEvents, events)
    getRuntimeContext.addAccumulator(FlinkLane.N.StopHash, hash)

  def process(stop: Integer, ctx: ProcessWindowFunction[Job.Stats, java.lang.Long, Integer, TimeWindow]#Context,
              in: java.lang.Iterable[Job.Stats], out: Collector[java.lang.Long]): Unit =
    val s = in.iterator.next()
    wins.add(1L); events.add(s.n)
    hash.add(Job.hash(ctx.window.getStart, stop.toLong, s.n, s.sum, s.max.toLong))
}

/** stage 4: Flink's answer to keyed state — a ValueState per (route, stop) */
private final class Bunching extends KeyedProcessFunction[java.lang.Long, Ride, java.lang.Long] {
  @transient private var last: ValueState[java.lang.Long] = scala.compiletime.uninitialized
  @transient private var bunches: LongCounter = scala.compiletime.uninitialized
  @transient private var gap: LongCounter = scala.compiletime.uninitialized

  override def open(ctx: OpenContext): Unit =
    last = getRuntimeContext.getState(new ValueStateDescriptor[java.lang.Long]("last", Types.LONG))
    bunches = new LongCounter; gap = new LongCounter

    getRuntimeContext.addAccumulator(FlinkLane.N.Bunches, bunches)
    getRuntimeContext.addAccumulator(FlinkLane.N.BunchGap, gap)

  def processElement(r: Ride, ctx: KeyedProcessFunction[java.lang.Long, Ride, java.lang.Long]#Context,
                     out: Collector[java.lang.Long]): Unit =
    val prev = last.value
    if prev != null then
      val d = Math.abs(r.ts - prev.longValue)
      if d < Job.BunchMs then { bunches.add(1L); gap.add(d); out.collect(d) }
    last.update(r.ts)
}

/** stage 5: the ranking, folded */
private final class Ranking
  extends ProcessWindowFunction[TopAcc, java.lang.Long, java.lang.Long, TimeWindow] {
  @transient private var wins: LongCounter = scala.compiletime.uninitialized
  @transient private var hash: XorLong = scala.compiletime.uninitialized

  override def open(ctx: OpenContext): Unit =
    wins = new LongCounter; hash = new XorLong
    getRuntimeContext.addAccumulator(FlinkLane.N.TopWins, wins)
    getRuntimeContext.addAccumulator(FlinkLane.N.TopHash, hash)

  def process(w: java.lang.Long,
              ctx: ProcessWindowFunction[TopAcc, java.lang.Long, java.lang.Long, TimeWindow]#Context,
              in: java.lang.Iterable[TopAcc], out: Collector[java.lang.Long]): Unit =
    val top = in.iterator.next()
    val ranked = Seq.tabulate(top.size)(i =>
      (top.route(i), Job.Stats(top.n(i), top.sum(i), top.max(i))))
    wins.add(1L)
    // `Job.topHash` is the ANSWER's format, not a way of computing it:
    // both engines must agree on the same hash of the same ranking
    hash.add(Job.topHash(ctx.window.getStart, ranked))
}
