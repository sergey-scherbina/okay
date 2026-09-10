package okay.flink

import okay.flink.wroclaw.*
import scala.concurrent.duration.Duration

/**
 * OKAY'S STREAMS AGAINST APACHE FLINK, on one event-time job over
 * Wrocław's own timetable (docs/benchmarks.md §20).
 *
 * The job is `Job` — five stages, two of them windowed by the SAME
 * `Aggregator` value in both lanes (that is what `okay-flink` is for).
 * The lanes are `OkayLane` (one JVM, one thread, `Chunks`) and
 * `FlinkLane` (a local MiniCluster at a chosen parallelism, with and
 * without checkpointing).
 *
 * CORRECTNESS COMES FIRST, and it is not decoration: the first test
 * asserts the two lanes' checksums are EQUAL — same window count, same
 * event count, same delay sum, same XOR of every emitted record's
 * hash, same ranking. Only then are the lanes timed. A benchmark whose
 * lanes compute different things measures nothing, and the equality is
 * reachable here because the feed jitters arrivals by less than the
 * watermark bound, so neither engine ever drops a late event.
 *
 * LIVE-tagged: it needs the GTFS snapshot on disk (see `Gtfs`) and a
 * quiet box, so it is out of `sbt test` and runs under
 * `sbt integrationTest`. `OKAY_FLINK_DAYS` sets how many service days
 * are replayed (default 1 ≈ 600k events; 4 ≈ 2.4M).
 */
class TestWroclawStream extends munit.FunSuite {
  override def munitTimeout: Duration = Duration(30, "min")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  private val days = sys.env.get("OKAY_FLINK_DAYS").map(_.toInt).getOrElse(1)
  private val rounds = sys.env.get("OKAY_FLINK_ROUNDS").map(_.toInt).getOrElse(3)

  private lazy val feed: Feed = {
    val t0 = System.nanoTime()
    val f = Gtfs.events(days)
    println(f"  feed: ${f.events.length}%,d events over $days day(s) ${f.from} .. ${f.to}, " +
      f"${f.routes.length} routes, ${f.stops} stops, built in ${(System.nanoTime() - t0) / 1000000}%,d ms")
    f
  }

  /** one run, timed AND watched: the peak heap a run reaches, sampled
   * every 20 ms from a fibre. Crude on its own — it counts garbage the
   * collector has not taken yet — but comparable between lanes in one
   * JVM, which is what the rows below use it for */
  private def sampled[A](a: => A): (A, Long, Long) =
    System.gc()
    val peak = new java.util.concurrent.atomic.AtomicLong(0L)
    val rt = Runtime.getRuntime
    val sampler = Thread.startVirtualThread { () =>
      try while true do
        val used = rt.totalMemory - rt.freeMemory
        if used > peak.get then peak.set(used)
        Thread.sleep(20)
      catch case _: InterruptedException => ()
    }
    val (r, ns) = timed(a)
    sampler.interrupt()
    (r, ns, peak.get)

  private def timed[A](a: => A): (A, Long) =
    val t0 = System.nanoTime()
    val r = a
    (r, System.nanoTime() - t0)

  /** ONE ROUND LIES (docs/benchmarks.md): the reported number is the
   * best of `rounds`, and the spread is printed beside it */
  private def best(label: String, expect: Option[Job.Result])(run: => Job.Result): Long =
    val runs = (1 to rounds).map(_ => timed(run))
    for (r, _) <- runs do expect.foreach(e => assertEquals(r, e, s"$label disagrees with the okay lane"))
    val ns = runs.map(_._2)
    val ms = ns.min / 1000000L
    val n = feed.events.length.toLong
    println(f"  $label%-34s ${n * 1000L / math.max(1L, ms)}%,10d ev/s  ${ms}%,7d ms" +
      f"   (spread ${ns.max.toDouble / ns.min}%.2fx over $rounds)")
    ms

  test("the two lanes compute the same answer") {
    val (okay, _) = timed(OkayLane.run(feed))
    println(s"  okay:  $okay")
    // the core operator (okay.Windows) and the hand-packed one this
    // benchmark carried before it must agree, or the §20 rows compare
    // two different computations
    assertEquals(OkayLane.packed(feed), okay, "the packed baseline differs from the core operator")
    // parallelism by MERGE: three different slicings, because the whole
    // question is whether the boundary rule holds wherever the cuts
    // fall — a wrong rule shows up as a different answer, not a
    // plausible one
    for lanes <- Seq(2, 4, 8) do
      assertEquals(OkayLane.parallel(feed, lanes), okay, s"the merge-parallel lane at $lanes differs")
    // the third engine: java.util.stream through okay-java's Collector
    // interop, sequential and parallel — the same aggregator again, on
    // a PREFIX, because that lane holds the whole history (see the
    // jdk test below for the size it fits in and why)
    val part = feed.copy(events = feed.events.take(jdkSize))
    val okayPart = OkayLane.run(part)
    assertEquals(JavaLane.run(part, parallel = false), okayPart, "the JDK lane differs")
    assertEquals(JavaLane.run(part, parallel = true), okayPart, "the PARALLEL JDK lane differs")
    // and the same lane with event time IN the collector
    assertEquals(JavaLane.windowed(part), okayPart, "the windowed JDK lane differs")
    val (flink, _) = timed(FlinkLane.run(feed, parallelism = 1))
    println(s"  flink: $flink")
    assertEquals(flink, okay, "Flink's answer differs from okay's")
    val (flink4, _) = timed(FlinkLane.run(feed, parallelism = 4))
    assertEquals(flink4, okay, "Flink at parallelism 4 differs from parallelism 1")
  }

  test("throughput") {
    val answer = OkayLane.run(feed)
    println(s"  ${feed.events.length} events, best of $rounds")
    val okayMs = best("okay, 1 thread", Some(answer))(OkayLane.run(feed))
    best("okay, 1 thread, packed-key windows", Some(answer))(OkayLane.packed(feed)): Unit
    val p2 = best("okay, 2 fibres (merge)", Some(answer))(OkayLane.parallel(feed, 2))
    val p4 = best("okay, 4 fibres (merge)", Some(answer))(OkayLane.parallel(feed, 4))
    val p8 = best("okay, 8 fibres (merge)", Some(answer))(OkayLane.parallel(feed, 8))
    val f1 = best("flink p1", Some(answer))(FlinkLane.run(feed, 1))
    val f4 = best("flink p4", Some(answer))(FlinkLane.run(feed, 4))
    val f4c = best("flink p4 + checkpoints 5 s", Some(answer))(FlinkLane.run(feed, 4, checkpointMs = 5000L))
    val f4n = best("flink p4, no object reuse", Some(answer))(FlinkLane.run(feed, 4, objectReuse = false))
    println(f"  okay scaling: 2 fibres ${okayMs.toDouble / p2}%.2fx  4 ${okayMs.toDouble / p4}%.2fx  8 ${okayMs.toDouble / p8}%.2fx")
    println(f"  ratios: flink p1 ${f1.toDouble / okayMs}%.1fx  p4 ${f4.toDouble / okayMs}%.1fx" +
      f"  p4+ckpt ${f4c.toDouble / okayMs}%.1fx  p4 no-reuse ${f4n.toDouble / okayMs}%.1fx  of the okay lane")
  }

  /**
   * THE WINDOW OPERATOR, THREE ROADS (stream-event-time-window).
   *
   * Stage 2 alone, because a `Stage` is linear and the job is a
   * fan-out. `chunks` is the road a user takes; `elementwise` is the
   * same operator driven by a per-element `Writer` producer, so the
   * delta is the PRODUCER; `stage` is `Windows.stage` under `through`
   * on that same producer, so the delta to `elementwise` is the
   * Take/Writer coroutine and nothing else.
   */
  test("the window operator: the loop, the producer, the coroutine") {
    val n = feed.events.length.toLong
    val expect = OkayLane.routeWindowsOnly(feed, "chunks")
    for road <- Seq("chunks", "elementwise", "stage") do
      val runs = (1 to rounds).map(_ => timed(OkayLane.routeWindowsOnly(feed, road)))
      for (w, _) <- runs do assertEquals(w, expect, s"road $road windowed differently")
      val ms = runs.map(_._2).min / 1000000L
      println(f"  route windows via $road%-14s ${n * 1000L / math.max(1L, ms)}%,10d ev/s  ${ms}%,7d ms")
  }

  /** the size the JDK lane fits in — a quarter of the feed. Not a
   * preference: `java.util.stream` has no event time, so its windows
   * are keys and every pane of the run stays live; at the full 2.4M
   * events the parallel lane dies with an OutOfMemoryError on an 8 GB
   * heap, where the okay and Flink lanes — both of which evict on a
   * watermark — never come near it */
  private def jdkSize: Int = feed.events.length / 4

  /**
   * THE THIRD ENGINE (bench-java-stream-lane): the same five stages
   * over `java.util.stream`, through okay-java's `Collect.collector`.
   *
   * Every lane is re-measured at the JDK lane's size so the four
   * numbers are comparable, and each is measured for PEAK HEAP as well
   * as time — because on this lane the memory is the finding, not a
   * footnote.
   */
  test("java.util.stream, through the Collector interop") {
    val part = feed.copy(events = feed.events.take(jdkSize))
    val n = part.events.length.toLong
    val answer = OkayLane.run(part)
    println(f"  at $n%,d events (a quarter of the feed), best of $rounds")

    def lane(label: String)(run: => Job.Result): Unit =
      val runs = (1 to rounds).map(_ => sampled(run))
      for (r, _, _) <- runs do assertEquals(r, answer, s"$label disagrees with the okay lane")
      val ms = runs.map(_._2).min / 1000000L
      val heap = runs.map(_._3).max / (1024L * 1024L)
      println(f"  $label%-34s ${n * 1000L / math.max(1L, ms)}%,10d ev/s  ${ms}%,7d ms  peak heap ${heap}%,6d MB")

    lane("okay, 1 thread")(OkayLane.run(part))
    lane("java.util.stream, sequential")(JavaLane.run(part, parallel = false))
    lane("java.util.stream, windowed collector")(JavaLane.windowed(part))
    lane("java.util.stream, parallel")(JavaLane.run(part, parallel = true))
    lane("flink p4")(FlinkLane.run(part, 4))
  }

  /**
   * WHAT EVENT TIME BUYS THE JDK LANE (jdk-event-time-collector).
   *
   * The lane that could not finish this feed at all — `groupingBy`
   * holds every pane of the run, and the parallel road died with an
   * OutOfMemoryError at this size on 4 GB and on 8 — is asked to run
   * it, with `okay.java.Windowed` in place of `groupingBy` for the two
   * windowed stages and the bunching state folded into an aggregator
   * instead of a list. If it finishes, the state model was the whole
   * problem.
   */
  test("java.util.stream with event time, on the full feed") {
    val answer = OkayLane.run(feed)
    val (r, ns, heap) = sampled(JavaLane.windowed(feed))
    assertEquals(r, answer, "the windowed JDK lane differs from okay's")
    val n = feed.events.length.toLong
    println(f"  ${"java.util.stream, windowed collector"}%-38s ${n * 1000000000L / ns}%,10d ev/s" +
      f"  ${ns / 1000000L}%,7d ms  peak heap ${heap / (1024 * 1024)}%,6d MB  at $n%,d events")
  }

  /**
   * THE REPLAY, PRICED (flink-window-memory).
   *
   * §20 quotes no memory number for Flink and says why: okay advances
   * its watermark per ELEMENT and evicts as it goes, while Flink's
   * generator fires every 200 ms of WALL time — so a full-speed replay
   * pushes days of event time through in seconds, the watermark
   * advances a handful of times, and the engine holds panes a real
   * deployment would have closed long before. This measures that
   * claim instead of asserting it: the same job at three rates, and
   * okay's pane count read EXACTLY (`Windows.live`) rather than
   * sampled off a heap.
   */
  test("the replay, priced: rate against window state") {
    val part = feed.copy(events = feed.events.take(jdkSize))
    val n = part.events.length.toLong
    val ((answer, panes), okayNs, okayHeap) = sampled(OkayLane.peakPanes(part))
    println(f"  at $n%,d events")
    println(f"  ${"okay, 1 thread"}%-32s ${okayNs / 1000000L}%,7d ms  peak heap ${okayHeap / (1024 * 1024)}%,6d MB" +
      f"  peak panes ${panes}%,9d (exact)")
    for rate <- Seq(0L, 2000000L, 500000L, 50000L) do
      val (r, ns, heap) = sampled(FlinkLane.run(part, 4, ratePerSecond = rate))
      assertEquals(r, answer, s"flink at rate $rate disagrees with the okay lane")
      // the ACHIEVED rate, not the asked one: Flink's gated limiter
      // grants a batch per cycle, so a number above what the pipeline
      // reaches is an upper bound rather than a target
      val achieved = n * 1000000000L / math.max(1L, ns)
      val label = if rate == 0 then "flink p4, full speed" else f"flink p4, asked $rate%,d ev/s"
      println(f"  $label%-32s ${ns / 1000000L}%,7d ms  peak heap ${heap / (1024 * 1024)}%,6d MB" +
        f"  achieved ${achieved}%,9d ev/s")
  }

  /**
   * WHERE FLINK'S TIME GOES — the row that makes the ratio above
   * readable rather than merely large.
   *
   * A Flink job pays a FIXED cost before it has seen an event: a
   * MiniCluster starts, a job graph is built and serialized, tasks are
   * deployed. A benchmark that runs one job over a fixed dataset
   * charges that cost to the events, and the smaller the dataset the
   * worse the engine looks — which is a statement about the benchmark,
   * not about Flink, whose jobs run for weeks.
   *
   * So each lane is run over PREFIXES of the same arrival-ordered
   * stream and the two costs are separated by a least-squares fit:
   * the intercept is what the run costs before the first event, the
   * slope is what one event costs. Both numbers are reported, and it
   * is the SLOPE that says what the engines are worth per element.
   */
  test("fixed cost and marginal cost") {
    val n = feed.events.length
    val sizes = Seq(n / 4, n / 2, n)

    def sweep(label: String)(run: Feed => Job.Result): Unit =
      val points = sizes.map { size =>
        val part = feed.copy(events = feed.events.take(size))
        val ms = (1 to rounds).map(_ => timed(run(part))._2).min / 1000000L
        (size.toDouble, ms.toDouble)
      }
      // least squares over three points: ms = fixed + size / rate
      val sx = points.map(_._1).sum; val sy = points.map(_._2).sum
      val sxx = points.map(p => p._1 * p._1).sum; val sxy = points.map(p => p._1 * p._2).sum
      val k = points.length
      val slope = (k * sxy - sx * sy) / (k * sxx - sx * sx)
      val fixed = (sy - slope * sx) / k
      println(f"  $label%-34s fixed ${fixed}%,8.0f ms   marginal ${1000.0 / slope}%,12.0f ev/s" +
        s"   points ${points.map((x, y) => f"${x.toLong}%,d:${y.toLong}%,dms").mkString(" ")}")

    sweep("okay, 1 thread")(OkayLane.run(_))
    sweep("flink p1")(FlinkLane.run(_, 1))
    sweep("flink p4")(FlinkLane.run(_, 4))
    sweep("okay, 4 fibres (merge)")(OkayLane.parallel(_, 4))
  }
}
