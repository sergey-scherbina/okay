package okay.cluster

import okay.{Chunks, given}
import okay.codec.Schema

/** a job whose partitions spend real time in a "foreign function": a
 * chunk-wise stage that sleeps and reports the sleep to the worker's
 * meter, the way okay-foreign-cluster's stages report theirs */
object SlowJob extends Job[Feed, Long] {
  import Feeds.*
  type A = Ev
  def name: String = "test.observed"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(f: Feed, parts: Int): Flow[Ev] =
    Flow.Local(Flow.slices(events(f), parts, chunk = 250), "far", (c: Chunks[Ev]) =>
      Chunks.mapWith(c) { chunk =>
        val t0 = System.nanoTime()
        Thread.sleep(20)
        Meter.foreign(System.nanoTime() - t0)
        chunk
      })
  def sink(f: Feed): Wire[Ev, Long] = Wire.fold(value)
}

/**
 * A JOB SAYS WHERE ITS TIME WENT (specs/dataflow.md, stage 15).
 *
 * The trace is asserted against the WALL CLOCK of the run it describes,
 * not against itself: the phases must add up to the root within 5%, so
 * time the engine spent outside any phase would show as a gap. And an
 * attempt's split is asserted EXACT — engine, foreign and wire add up to
 * its round trip to the nanosecond — because each is measured, none is
 * the remainder of an estimate.
 */
class TestObserved extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  Jobs.register(SlowJob)
  val feed: Feed = Feed(20000, Late - 1)

  def ns(s: JobTrace.Span, key: String): Long = s.attrs.find(_._1 == key).fold(0L)(_._2.toLong)

  test("a job's trace: the phases add up to the wall clock within 5%, an attempt splits exactly") {
    val trace = JobTrace()
    val stats = JobStats()
    val workers = Vector.fill(4)(Cluster.measured(Cluster.local))
    val got = Cluster.run(SlowJob, feed, 8, workers, probe = Probe.both(trace, stats.probe())).runWith
    assertEquals(got.value, events(feed).map(_.v.toLong).sum)

    val spans = trace.spans
    val root = spans.head
    assertEquals(root.parent, None)
    val wall = root.end - root.start
    val phases = spans.filter(_.parent.contains(root.id))
    assertEquals(phases.map(_.name).toSet, Set("plan", "extent", "partitions", "merge"))
    val covered = phases.map(s => s.end - s.start).sum
    assert(math.abs(wall - covered) <= wall / 20,
      s"the phases cover $covered ns of a $wall ns run — more than 5% went unaccounted for")

    val runs = spans.filter(_.name.startsWith("run "))
    assertEquals(runs.length, 8)
    assertEquals(runs.map(ns(_, "rows")).sum, feed.n.toLong, "rows per partition do not add up to the input")
    for s <- runs do
      assertEquals(ns(s, "engine_ns") + ns(s, "foreign_ns") + ns(s, "wire_ns"), s.end - s.start,
        s"attempt ${s.name} does not split into its round trip")
      assert(ns(s, "foreign_ns") > 0 && ns(s, "foreign_calls") > 0, s"${s.name} spent nothing in the foreign stage")
    assert(stats.value("okay_job_seconds_total", "test.observed", """,where="foreign"""") > 0.0)
    assertEquals(stats.value("okay_job_rows_total", "test.observed"), feed.n.toDouble)
  }

  test("a killed worker shows as a lost attempt, a burial and a recompute — in the trace and the metrics") {
    val trace = JobTrace()
    val stats = JobStats()
    val dead: Cluster.Serve = _ => throw java.io.IOException("this one is gone")
    val workers = Vector(dead) ++ Vector.fill(2)(Cluster.measured(Cluster.local))
    val got = Cluster.run(FanJob, feed, 6, workers, probe = Probe.both(trace, stats.probe())).runWith
    assert(got.retried > 0)
    val spans = trace.spans
    assert(spans.exists(s => s.error.exists(_.contains("this one is gone"))), "no attempt span carries the death")
    assert(spans.exists(_.name == "buried worker 0"), "the burial is not in the trace")
    assertEquals(stats.value("okay_job_workers_buried_total", "test.fan"), 1.0)
    assert(stats.value("okay_job_recomputes_total", "test.fan") >= 1.0, stats.render)
    assert(stats.render.contains("okay_job_workers_buried_total{job=\"test.fan\"} 1"), stats.render)
  }

  test("a stream reports every epoch and its watermark lag") {
    val trace = JobTrace()
    val stats = JobStats()
    val got = Cluster.stream(WindowJob, feed, 4, Vector.fill(2)(Cluster.measured(Cluster.local)), 1024,
      probe = Probe.both(trace, stats.probe())).runWith
    val epochs = trace.spans.filter(_.name.startsWith("epoch "))
    assert(epochs.length >= 2, s"${epochs.length} epochs traced")
    assert(epochs.forall(_.attrs.exists((k, v) => k == "lag" && v.toLong >= 0L)), epochs.toString)
    assertEquals(stats.value("okay_job_epoch", "test.window"), epochs.length.toDouble)
    assert(trace.spans.exists(_.name == "close"), "the closing sweep is not traced")
    assert(got.value.n > 0)
  }

  test("a shuffled job traces its map and reduce sides") {
    val trace = JobTrace()
    val boxes = Vector.tabulate(3)(i => s"w$i")
    lazy val directory: Map[String, Cluster.Serve] =
      boxes.map(a => a -> Cluster.measured(Cluster.exchanging(a, peer => directory(peer)))).toMap
    val peers = boxes.map(a => Cluster.Peer(a, directory(a)))
    val _ = Cluster.shuffle(ShuffleJob, feed, 6, 3, peers, probe = trace).runWith
    val names = trace.spans.map(_.name)
    assert(names.contains("map") && names.contains("reduce"), names.toString)
    assertEquals(names.count(_.startsWith("shuffle ")), 6)
    assertEquals(names.count(_.startsWith("reduce ")), 3)
  }

  test("no probe, no cost: a probe that is off is never called") {
    val silent = new Probe:
      def apply(seen: Seen): Unit = fail(s"an event was built for a probe that is off: $seen")
      override def on: Boolean = false
    val got = Cluster.run(FanJob, feed, 4, Vector(Cluster.measured(Cluster.local)), probe = silent).runWith
    assertEquals(got.value, Cluster.run(FanJob, feed, 4, Vector(Cluster.local)).runWith.value)
  }
}
