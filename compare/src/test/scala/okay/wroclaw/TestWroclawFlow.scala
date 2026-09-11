package okay.wroclaw

import okay.{Aggregator, Chunks, Pane, Sequential}
import okay.cluster.{Finish, Flow, Flows, Sink}
import okay.given
import scala.collection.immutable.ArraySeq

/**
 * THE STAGE-1 ACCEPTANCE of specs/dataflow.md: the engine computes
 * Wrocław's job, and its answer is `OkayLane`'s answer — the same
 * eleven checksums docs/benchmarks.md §20 already holds every engine
 * to, at every parallelism.
 *
 * WHAT IS BEING CLAIMED, precisely. §20's parallel okay lane reaches
 * this answer with a hand-written slice stitch: a pre-pass for the
 * slice maxima, a rule for which panes are complete in a slice, a
 * boundary walk for the bunching pairs — fifty lines living inside a
 * benchmark, which no user of this repository could reuse. Here the
 * job is a PLAN and the stitching is the engine's, so what the lane
 * writes is the job: a source cut into partitions, a filter, a map, a
 * window, an aggregator.
 *
 * THREE PASSES, NOT ONE. Stage 1 has no exchange, hence at most one
 * keyed stage per flow, so the three keyed stages are three flows
 * over the same source and the feed is read three times. §20's lane
 * reads it once. That is a real cost and it is the whole content of
 * stage 3 (one pass, many sinks); until then the number this lane
 * would print is not comparable and is not printed.
 */
class TestWroclawFlow extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")

  /** service days of the feed — the measurement subclasses this to
   * get a run long enough to be measured (MeasureWroclawFlow) */
  def days: Int = 1
  lazy val feed: Feed = Gtfs.events(days)
  lazy val tram: Array[Boolean] = feed.routes.iterator.map(_.tram).toArray
  lazy val expected: Job.Result = OkayLane.run(feed)

  /** stage 1 of the job — the map-side join — as the plan's own
   * filter and map, over the arrival order cut into `p` partitions */
  def rides(p: Int): Flow[Ride] =
    Flow.slices(ArraySeq.unsafeWrapArray(feed.events), p)
      .filter(d => d.route >= 0 && d.route < tram.length)
      .map(d => new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))

  /** the checksums, assembled by §20's own Sink so that a difference
   * between this lane and that one cannot be an assembly difference */
  def into(feedPane: (OkayLane.Sink, Pane[Int, Job.Stats]) => Unit)
  : Aggregator[Pane[Int, Job.Stats], OkayLane.Sink, Job.Result] =
    new Aggregator[Pane[Int, Job.Stats], OkayLane.Sink, Job.Result]:
      def init: OkayLane.Sink = new OkayLane.Sink(tram)
      def add(s: OkayLane.Sink, p: Pane[Int, Job.Stats]): OkayLane.Sink = { feedPane(s, p); s }
      def merge(a: OkayLane.Sink, b: OkayLane.Sink): OkayLane.Sink = { a.absorb(b); a }
      def present(s: OkayLane.Sink): Job.Result = s.result

  test("stage 2 — tumbling windows per route, and stage 5 ranked from them") {
    for p <- Vector(1, 2, 4, 8) do
      val got = Flows.run(
        rides(p).tumbling(Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats),
        into((s, pane) => s.route(pane))).runWith
      assertEquals(got.dropped, 0L, s"nothing is late in this feed ($p partitions)")
      assertEquals(got.value.routeWins, expected.routeWins, s"$p partitions")
      assertEquals(got.value.routeEvents, expected.routeEvents, s"$p partitions")
      assertEquals(got.value.routeDelay, expected.routeDelay, s"$p partitions")
      assertEquals(got.value.routeHash, expected.routeHash, s"$p partitions")
      assertEquals(got.value.topWins, expected.topWins, s"$p partitions")
      assertEquals(got.value.topHash, expected.topHash, s"$p partitions")
  }

  test("stage 3 — sliding windows per stop, three panes per event") {
    for p <- Vector(1, 2, 4, 8) do
      val got = Flows.run(
        rides(p).sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats),
        into((s, pane) => s.stop(pane))).runWith
      assertEquals(got.dropped, 0L, s"$p partitions")
      assertEquals(got.value.stopWins, expected.stopWins, s"$p partitions")
      assertEquals(got.value.stopEvents, expected.stopEvents, s"$p partitions")
      assertEquals(got.value.stopHash, expected.stopHash, s"$p partitions")
  }

  /**
   * STAGE 4 AS ALGEBRA — Claim 2 of specs/dataflow.md, on the job it
   * was written for. Flink answers this with a KeyedProcessFunction
   * over ValueState, which must have every record of a key on one
   * machine. The same question asked of a SLICE has an answer that
   * combines: the slice's ends, the bunches strictly inside it, and
   * their gaps. Merging two of them asks one more question — whether
   * the gap ACROSS the boundary is short — and that question is not
   * the same one in the other order, which is exactly why this is a
   * `Sequential` and not an `Aggregator`.
   *
   * The value itself now lives in `src/main` beside the distributed
   * job, which builds the same algebra on a worker from a name
   * (specs/dataflow.md, stage 7).
   */
  val bunching: Sequential[Ride, Bunching.Runs, (Long, Long)] = Bunching.algebra
  val totals: Aggregator[(Long, (Long, Long)), (Long, Long), (Long, Long)] = Bunching.totals

  test("stage 4 — keyed state as a Sequential: no shuffle, and the same count") {
    for p <- Vector(1, 2, 4, 8) do
      val (bunches, gap) = Flows.fold(
        rides(p).keyBy(r => (r.route.toLong << 20) | r.stop.toLong)(bunching), totals).runWith
      assertEquals(bunches, expected.bunches, s"$p partitions")
      assertEquals(gap, expected.bunchGap, s"$p partitions")
  }

  test("the exchange computes the same job — and Auto declines it, correctly") {
    // stage 2's honest result on THIS job. Wrocław has ~138 routes
    // over a few thousand five-minute windows, so the accumulators
    // that reach the merge are ~10^4 against 10^6 events: three
    // orders of magnitude under the crossover MeasureExchange found.
    // The exchange is the road you take when you have to, and this
    // job does not have to.
    val merged = Flows.fold(
      rides(8).tumbling(Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats),
      into((s, pane) => s.route(pane))).runWith
    assertEquals(merged, expected.copy(
      stopWins = 0, stopEvents = 0, stopHash = 0, bunches = 0, bunchGap = 0))

    for r <- Vector(2, 4, 8) do
      val shuffled = Flows.run(
        rides(8).tumbling(Job.WindowMs, Job.Lateness, finish = Finish.Shuffle(r))(_.route)(_.ts)(Job.stats),
        into((s, pane) => s.route(pane))).runWith
      assertEquals(shuffled.value, merged, s"$r reducers")
      assertEquals(shuffled.reducers, r)

    val auto = Flows.run(
      rides(8).tumbling(Job.WindowMs, Job.Lateness, finish = Finish.Auto)(_.route)(_.ts)(Job.stats),
      into((s, pane) => s.route(pane))).runWith
    assertEquals(auto.value, merged)
    assertEquals(auto.reducers, 1, "this job's panes are far under the exchange's crossover")
  }

  /** the three stages as sinks, separately — the decomposition the
   * measurement needs, and what `wholeJob` is built from */
  def routeSink: Sink[Ride, Job.Result] =
    Sink.tumbling(Job.WindowMs, Job.Lateness, (r: Ride) => r.route, (r: Ride) => r.ts, Job.stats)(
      into((s, pane) => s.route(pane)))
  def stopSink: Sink[Ride, Job.Result] =
    Sink.sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness,
      (r: Ride) => r.stop, (r: Ride) => r.ts, Job.stats)(into((s, pane) => s.stop(pane)))
  def bunchSink: Sink[Ride, (Long, Long)] =
    Sink.keyed((r: Ride) => (r.route.toLong << 20) | r.stop.toLong, bunching)(totals)

  /** the job as ONE plan: three sinks, one pass (stage 3) */
  def wholeJob: Sink[Ride, ((Job.Result, Job.Result), (Long, Long))] =
    routeSink.and(stopSink).and(bunchSink)

  def assembled(v: ((Job.Result, Job.Result), (Long, Long))): Job.Result =
    val ((route, stop), (bunches, gap)) = v
    route.merge(stop).copy(bunches = bunches, bunchGap = gap)

  test("ONE PASS: the whole job as three sinks over one source") {
    // this is what stage 3 is for. Before it, Wrocław's three keyed
    // stages were three plans and the feed was read three times where
    // §20's hand-written lane reads it once.
    for p <- Vector(1, 2, 4, 8) do
      val got = Flows.fan(rides(p), wholeJob).runWith
      assertEquals(got.dropped, 0L, s"$p partitions")
      assertEquals(assembled(got.value), expected, s"$p partitions")
  }

  /** the job as THREE plans, which is what the engine did before
   * stage 3 — kept so the measurement can say what one pass bought */
  def threeFlows(p: Int): Job.Result =
    val route = Flows.fold(
      rides(p).tumbling(Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats),
      into((s, pane) => s.route(pane))).runWith
    val stop = Flows.fold(
      rides(p).sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats),
      into((s, pane) => s.stop(pane))).runWith
    val (bunches, gap) = Flows.fold(
      rides(p).keyBy(r => (r.route.toLong << 20) | r.stop.toLong)(bunching), totals).runWith
    route.merge(stop).copy(bunches = bunches, bunchGap = gap)

  test("the whole job: all eleven checksums, at parallelism 8") {
    val p = 8
    val route = Flows.fold(
      rides(p).tumbling(Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats),
      into((s, pane) => s.route(pane))).runWith
    val stop = Flows.fold(
      rides(p).sliding(Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats),
      into((s, pane) => s.stop(pane))).runWith
    val (bunches, gap) = Flows.fold(
      rides(p).keyBy(r => (r.route.toLong << 20) | r.stop.toLong)(bunching), totals).runWith
    val got = route.merge(stop).copy(bunches = bunches, bunchGap = gap)
    assertEquals(got, expected)
  }

  test("the source is read three times, and the suite says so out loud") {
    // stage 1 has no exchange: three keyed stages are three flows. The
    // plan refuses to pretend otherwise.
    val two = rides(2).keyBy(_.route)(Job.stats).map((k, s) => k -> s.n)
    val e = intercept[IllegalArgumentException](
      Flows.fold(two.keyBy(_._1)(Aggregator.count), Aggregator.count).runWith)
    assert(e.getMessage.contains("exchange"), e.getMessage)
  }

  /** the reference itself, one line, so the suite is honest about what
   * "expected" is */
  test("the reference is §20's own lane") {
    assertEquals(OkayLane.run(feed), expected)
    assert(Chunks.foldLeft(Chunks.fromIterator(feed.events.iterator, 256))(0L)((n, _) => n + 1) > 100000L)
  }
}
