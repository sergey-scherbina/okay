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
    val f1 = best("flink p1", Some(answer))(FlinkLane.run(feed, 1))
    val f4 = best("flink p4", Some(answer))(FlinkLane.run(feed, 4))
    val f4c = best("flink p4 + checkpoints 5 s", Some(answer))(FlinkLane.run(feed, 4, checkpointMs = 5000L))
    val f4n = best("flink p4, no object reuse", Some(answer))(FlinkLane.run(feed, 4, objectReuse = false))
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
  }
}
