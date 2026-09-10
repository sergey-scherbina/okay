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

  lazy val feed: Feed = Gtfs.events(1)
  lazy val expected: Job.Result = OkayLane.run(feed)

  val Rounds = 7
  val Warmup = 3
  val Parts = 8

  val fixture = new TestWroclawFlow      // the sinks and the assembly, defined once

  def best(name: String, f: () => Job.Result): (String, Long) =
    for _ <- 0 until Warmup do assertEquals(f(), expected, s"$name computes something else")
    var lo = Long.MaxValue
    for _ <- 0 until Rounds do
      System.gc()
      val t0 = System.nanoTime()
      val got = f()
      lo = math.min(lo, (System.nanoTime() - t0) / 1000000L)
      assertEquals(got, expected, s"$name computes something else")
    (name, lo)

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
    println(f"%n  panes the coordinator merges: route ${expected.routeWins}%,d, " +
      f"stop ${expected.stopWins}%,d (per partition, before merging)%n")
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
    val lanes = Vector(
      best("hand-written, 1 thread (OkayLane.run)", () => OkayLane.run(feed)),
      best(s"hand-written, $Parts threads (OkayLane.parallel)", () => OkayLane.parallel(feed, Parts)),
      best("engine, 1 partition, one pass", () =>
        fixture.assembled(Flows.fan(fixture.rides(1), fixture.wholeJob).runWith.value)),
      best(s"engine, $Parts partitions, one pass", () =>
        fixture.assembled(Flows.fan(fixture.rides(Parts), fixture.wholeJob).runWith.value)),
      best(s"engine, $Parts partitions, THREE passes (what stage 2 did)", () =>
        fixture.threeFlows(Parts)),
    )
    val floor = lanes.map(_._2).min
    println(f"%n  ${n}%,d events, one service day, minimum of $Rounds rounds%n")
    println("  lane                                              |    ms |    ev/s | vs best")
    println("  --------------------------------------------------|-------|---------|--------")
    for (name, ms) <- lanes do
      val evs = if ms == 0 then 0L else n.toLong * 1000L / ms
      println(f"  $name%-49s | $ms%,5d | $evs%,7d | ${ms.toDouble / floor}%.2fx")
    println()
  }
