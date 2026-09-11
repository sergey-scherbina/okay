package okay.wroclaw

import okay.{Aggregator, Pane}
import okay.cluster.{Flows, Sink}
import okay.given

/**
 * WHERE THE THIRD OF A FAN THAT IS IN NONE OF ITS SINKS GOES
 * (BACKLOG: dataflow-fan-overhead).
 *
 * Stage 3's own decomposition summed the sinks to 104 ms against a
 * fan of 154 on the same feed, and named three candidates it could
 * not tell apart from the outside: the PRE-PASS computing a column
 * per event-time function, the tuple PLUMBING of `Sink.and`, and
 * three operators' STATE live at once. This separates them, each by a
 * lane that changes one of the three and nothing else.
 *
 * TWO THINGS THE FIRST ARITHMETIC GOT WRONG, and they are why this
 * had to be measured rather than reasoned:
 *
 *   - a fan's pre-pass computes EVERY column in ONE pass over the
 *     source, so three sinks do not cost three passes; and
 *   - `Sink.keyed` has no event-time function at all, so the bunching
 *     lane ran with NO pre-pass while the fan's has two columns. The
 *     "104" was the sum of three lanes minus one source read, which
 *     is not the same decomposition.
 */
class MeasureFanOverhead extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  val Rounds = 21
  val Warmup = 5
  val Parts = 8

  val fixture = new TestWroclawFlow { override def days: Int = 4 }
  lazy val feed: Feed = fixture.feed

  /** a window so wide that every event of the feed lands in one pane
   * per key — the operator still keys, still watermarks and still
   * evicts, so its PRE-PASS is a real one and its main work is as
   * small as a windowed sink's can be */
  val Wide: Long = 30L * 24 * 60 * 60 * 1000

  def cheapOn(at: Ride => Long): Sink[Ride, Long] =
    Sink.tumbling(Wide, Job.Lateness, (_: Ride) => 0, at, Job.summaryStats)(
      Aggregator.count[Pane[Int, Job.Stats]])

  /** a sink with no event time and almost no work: what an extra arm
   * of `Sink.and` costs when the arm itself is free */
  val nothing: Sink[Ride, Long] = Sink.fold(Aggregator.count[Ride])

  def best(name: String, f: () => Any): (String, Long, Long) =
    for _ <- 0 until Warmup do f(): Unit
    var lo = Long.MaxValue
    var hi = 0L
    for _ <- 0 until Rounds do
      System.gc()
      val t0 = System.nanoTime()
      f(): Unit
      val ms = (System.nanoTime() - t0) / 1000000L
      if ms < lo then lo = ms
      if ms > hi then hi = ms
    (name, lo, hi)

  /** every lane once per ROUND, so the machine's drift is shared */
  def interleaved(lanes: Vector[(String, () => Any)]): Vector[(String, Long, Long)] =
    for _ <- 0 until Warmup; (_, f) <- lanes do f(): Unit
    val lo = Array.fill(lanes.length)(Long.MaxValue)
    val hi = Array.fill(lanes.length)(0L)
    for _ <- 0 until Rounds do
      System.gc()
      for ((_, f), i) <- lanes.zipWithIndex do
        val t0 = System.nanoTime()
        f(): Unit
        val ms = (System.nanoTime() - t0) / 1000000L
        if ms < lo(i) then lo(i) = ms
        if ms > hi(i) then hi(i) = ms
    lanes.indices.toVector.map(i => (lanes(i)._1, lo(i), hi(i)))

  test("the fan's overhead, separated into its three candidates") {
    def rides = fixture.rides(Parts)
    val rows = interleaved(Vector(
      // the floor: one pass, no pre-pass, no operator
      ("source alone (count, no pre-pass)", () =>
        Flows.fold(rides, Aggregator.count[Ride]).runWith),
      // THE PRE-PASS: the same trivial sink with an event time, so a
      // column is computed over every element before the main pass
      ("+ a cheap WINDOW (1 pre-pass column)", () =>
        Flows.fan(rides, cheapOn(_.ts)).runWith),
      ("+ a second cheap window, SAME time function (2 columns)", () =>
        Flows.fan(rides, cheapOn(_.ts).and(cheapOn(_.ts))).runWith),
      // THE PLUMBING: an arm that does nothing, added to a real sink
      ("route windows alone", () => Flows.fan(rides, fixture.routeSink).runWith),
      ("route + an arm that only counts", () =>
        Flows.fan(rides, fixture.routeSink.and(nothing)).runWith),
      ("route + two arms that only count", () =>
        Flows.fan(rides, fixture.routeSink.and(nothing).and(nothing)).runWith),
      // THE STATE: two real operators live at once
      ("stop windows alone", () => Flows.fan(rides, fixture.stopSink).runWith),
      ("route + stop", () => Flows.fan(rides, fixture.routeSink.and(fixture.stopSink)).runWith),
      ("bunching alone (keyed, no pre-pass)", () => Flows.fan(rides, fixture.bunchSink).runWith),
      ("THE FAN: route + stop + bunching", () => Flows.fan(rides, fixture.wholeJob).runWith),
      // and what the fan is supposed to be better than
      ("three separate fans, one after another", () =>
        val _ = Flows.fan(rides, fixture.routeSink).runWith
        val _ = Flows.fan(rides, fixture.stopSink).runWith
        Flows.fan(rides, fixture.bunchSink).runWith),
    ))
    println(f"%n  ${feed.events.length}%,d events, $Parts partitions, best of $Rounds interleaved rounds%n")
    println("  lane                                                     |    ms | (worst)")
    println("  ---------------------------------------------------------|-------|--------")
    for (name, ms, worst) <- rows do println(f"  $name%-56s | $ms%,5d | $worst%,7d")
    println()
    val by = rows.map(r => r._1 -> r._2).toMap
    def d(a: String, b: String): Long = by(a) - by(b)
    println(f"  a PRE-PASS column (a cheap window over the source)       | ${d("+ a cheap WINDOW (1 pre-pass column)", "source alone (count, no pre-pass)")}%,5d ms")
    println(f"  a SECOND column in the same pre-pass                     | ${d("+ a second cheap window, SAME time function (2 columns)", "+ a cheap WINDOW (1 pre-pass column)")}%,5d ms")
    println(f"  one arm of Sink.and that does nothing                    | ${d("route + an arm that only counts", "route windows alone")}%,5d ms")
    println(f"  a second such arm                                        | ${d("route + two arms that only count", "route + an arm that only counts")}%,5d ms")
    println(f"  the fan against its three sinks run separately           | ${d("THE FAN: route + stop + bunching", "three separate fans, one after another")}%,5d ms")
    println()
  }
