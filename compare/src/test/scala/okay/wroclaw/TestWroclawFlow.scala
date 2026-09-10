package okay.wroclaw

import okay.{Aggregator, Chunks, Pane, Sequential}
import okay.cluster.{Flow, Flows}
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

  lazy val feed: Feed = Gtfs.events(1)
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
   */
  final case class Runs(first: Long, last: Long, n: Long, bunches: Long, gap: Long)
  val bunching: Sequential[Ride, Runs, (Long, Long)] = new Sequential[Ride, Runs, (Long, Long)]:
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

  val totals: Aggregator[(Long, (Long, Long)), (Long, Long), (Long, Long)] =
    Aggregator[(Long, (Long, Long)), (Long, Long), (Long, Long)]((0L, 0L))((t, kv) =>
      (t._1 + kv._2._1, t._2 + kv._2._2))((a, b) => (a._1 + b._1, a._2 + b._2))(identity)

  test("stage 4 — keyed state as a Sequential: no shuffle, and the same count") {
    for p <- Vector(1, 2, 4, 8) do
      val (bunches, gap) = Flows.fold(
        rides(p).keyBy(r => (r.route.toLong << 20) | r.stop.toLong)(bunching), totals).runWith
      assertEquals(bunches, expected.bunches, s"$p partitions")
      assertEquals(gap, expected.bunchGap, s"$p partitions")
  }

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
