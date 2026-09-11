package okay.wroclaw

import okay.cluster.Flows
import okay.given

/**
 * WHAT THE ENGINE COSTS AGAINST THE HAND-WRITTEN LANE (specs/
 * dataflow.md, stage 3).
 *
 * Until stage 3 the engine read Wrocław's feed three times where
 * §20's lane reads it once, and both previous lanes refused to quote
 * a number for that reason. One pass exists now, so the comparison is
 * finally like for like: the same job, the same feed, the same JVM,
 * the same eleven checksums asserted before anything is timed.
 *
 * WHAT THIS IS NOT. It is not a §20 table row. That table gives every
 * lane its own JVM (`scripts/wroclaw-bench.sh`) precisely so no lane
 * inherits another's heap or JIT state, and these lanes share all
 * three. It answers one narrower question — what does expressing the
 * job as a PLAN cost against writing the slice stitch by hand — and a
 * row in §20 is stage 7's business.
 *
 * The three-flow lane is kept deliberately: it is what the engine did
 * before this stage, so the table says what one pass bought rather
 * than asserting it.
 */
class MeasureWroclawFlow extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  /**
   * FOUR SERVICE DAYS, NOT ONE, AND THE LANES INTERLEAVED.
   *
   * The first version of this harness ran each lane's rounds to
   * completion before starting the next, over one service day, and
   * the two readings it produced for the same tree were 1.07x and
   * 1.64x. Both lanes swung 20-27% between runs, which is larger than
   * the difference being quoted — a lane that swings a quarter cannot
   * price a tenth (instrument-bars-smaller-than-effect).
   *
   * So: a feed four times the size, so a lane is hundreds of
   * milliseconds rather than tens; every lane run once per ROUND, so
   * drift in the machine hits all of them alike; and the minimum of
   * the rounds reported with the worst beside it, so a reader can see
   * whether the row is the engine or the box.
   */
  val Rounds = 7
  val Warmup = 3
  val Parts = 8

  /** the sinks and the assembly, defined once — over a feed four
   * times the size, which is the fixture's own `days` knob */
  val fixture = new TestWroclawFlow { override def days: Int = 4 }
  lazy val feed: Feed = fixture.feed
  lazy val expected: Job.Result = fixture.expected

  /** one lane: a name and a run that must answer the job */
  final case class Lane(name: String, run: () => Job.Result)

  /** every lane once per round, so the machine's drift is shared */
  def interleaved(lanes: Vector[Lane]): Vector[(String, Long, Long)] =
    for _ <- 0 until Warmup do
      for l <- lanes do assertEquals(l.run(), expected, s"${l.name} computes something else")
    val lo = Array.fill(lanes.length)(Long.MaxValue)
    val hi = Array.fill(lanes.length)(0L)
    for _ <- 0 until Rounds do
      System.gc()
      for (l, i) <- lanes.zipWithIndex do
        val t0 = System.nanoTime()
        val got = l.run()
        val ms = (System.nanoTime() - t0) / 1000000L
        assertEquals(got, expected, s"${l.name} computes something else")
        if ms < lo(i) then lo(i) = ms
        if ms > hi(i) then hi(i) = ms
    lanes.indices.toVector.map(i => (lanes(i).name, lo(i), hi(i)))

  /** the same timing, for a lane that answers a PIECE of the job —
   * so the table can say where the time goes instead of asserting it */
  def part(name: String, f: () => Any): (String, Long) =
    for _ <- 0 until Warmup do f(): Unit
    var lo = Long.MaxValue
    for _ <- 0 until Rounds do
      System.gc()
      val t0 = System.nanoTime()
      f(): Unit
      lo = math.min(lo, (System.nanoTime() - t0) / 1000000L)
    (name, lo)

  test("where the engine's time goes: one sink at a time") {
    // the completeness rule's own number: how many accumulators
    // actually reach the coordinator, against how many panes the job
    // produces in total
    val whole = Flows.fan(fixture.rides(Parts), fixture.wholeJob).runWith
    val total = expected.routeWins + expected.stopWins
    println(f"%n  panes the job produces: route ${expected.routeWins}%,d + " +
      f"stop ${expected.stopWins}%,d = $total%,d")
    println(f"  accumulators reaching the COORDINATOR at $Parts partitions: " +
      f"${whole.merged}%,d (${100.0 * whole.merged / total}%.1f%% of one partition's worth)%n")
    val lanes = Vector(
      part("engine: the source alone (count)", () =>
        Flows.fold(fixture.rides(Parts), okay.Aggregator.count[Ride]).runWith),
      part("engine: route windows only (tumbling, 138 keys)", () =>
        Flows.fan(fixture.rides(Parts), fixture.routeSink).runWith),
      part("engine: stop windows only (sliding, 2482 keys x 3 panes)", () =>
        Flows.fan(fixture.rides(Parts), fixture.stopSink).runWith),
      part("engine: bunching only (keyed state)", () =>
        Flows.fan(fixture.rides(Parts), fixture.bunchSink).runWith),
      part("engine: all three, one pass", () =>
        Flows.fan(fixture.rides(Parts), fixture.wholeJob).runWith),
    )
    println("  lane                                                       |    ms")
    println("  -----------------------------------------------------------|------")
    for (name, ms) <- lanes do println(f"  $name%-58s | $ms%,5d")
    println()
  }

  test("the plan against the hand-written lane, on the same feed") {
    val n = feed.events.length
    val rows = interleaved(Vector(
      Lane("hand-written, 1 thread (OkayLane.run)", () => OkayLane.run(feed)),
      Lane(s"hand-written, $Parts threads (OkayLane.parallel)", () => OkayLane.parallel(feed, Parts)),
      Lane("engine, 1 partition, one pass", () =>
        fixture.assembled(Flows.fan(fixture.rides(1), fixture.wholeJob).runWith.value)),
      Lane(s"engine, $Parts partitions, one pass", () =>
        fixture.assembled(Flows.fan(fixture.rides(Parts), fixture.wholeJob).runWith.value)),
      // NOT a measurement of the pass count: `threeFlows` drives the
      // single-stage road (`Flows.run`), which has no completeness
      // rule and merges every pane at the coordinator. It is what the
      // engine was before this lane, kept for exactly that
      Lane(s"engine, $Parts partitions, three plans via Flows.run", () =>
        fixture.threeFlows(Parts)),
    ))
    val floor = rows.map(_._2).min
    println(f"%n  ${n}%,d events, four service days, best of $Rounds interleaved rounds%n")
    println("  lane                                              |    ms | (worst) |    ev/s | vs best")
    println("  --------------------------------------------------|-------|---------|---------|--------")
    for (name, ms, worst) <- rows do
      val evs = if ms == 0 then 0L else n.toLong * 1000L / ms
      println(f"  $name%-49s | $ms%,5d | $worst%,7d | $evs%,7d | ${ms.toDouble / floor}%.2fx")
    println()
  }
