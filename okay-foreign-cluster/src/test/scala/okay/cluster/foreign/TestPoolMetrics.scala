package okay.cluster.foreign

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Flow, JobStats, Jobs, Wire}
import okay.codec.Schema
import okay.foreign.{Foreign, TestPy}
import java.nio.file.Files

/** a python map whose interpreter kills ITSELF on its first frame — once
 * per JVM, by a marker file — and doubles every row after that */
object DiesOnce:
  val marker: java.nio.file.Path =
    val f = Files.createTempFile("okay-dies-once", ".marker")
    Files.delete(f)
    f
  val mod = Foreign.module("dies_once", s"""
    import os

    def double(frame):
        try:
            # O_EXCL: of every interpreter racing here, exactly one dies
            os.close(os.open("$marker", os.O_CREAT | os.O_EXCL))
            os._exit(3)
        except FileExistsError:
            pass
        return {"key": frame["key"], "v": [x * 2 for x in frame["v"]]}
  """)

  object Job extends okay.cluster.Job[Scale, Long]:
    type A = Out
    def name: String = "test.foreign.py.dies-once"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Long] = summon[Schema[Long]]
    def flow(p: Scale, parts: Int): Flow[Out] =
      Flow.slices(Rows.of(p.n), parts).mapPy[Out](mod, "double", PyJobs.python, workers = 2)
    def sink(p: Scale): Wire[Out, Long] = Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))

  Jobs.register(Job)

/** foreign-pool-metrics (specs/dataflow.md, stage 15): the pools under a
 * foreign stage say what they did, in the job's metrics (Live: python3) */
class TestPoolMetrics extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  test("an interpreter killed mid-run: one restart in the rendered metrics, the answer unchanged") {
    val stats = JobStats()
    val workers = Vector.fill(2)(Cluster.measured(Cluster.local))
    val got = Cluster.run(DiesOnce.Job, Scale(20000), 4, workers, probe = stats.probe()).runWith
    assertEquals(got.value, Rows.doubled(20000))
    assert(Files.exists(DiesOnce.marker), "the interpreter never killed itself: nothing was tested")
    val text = stats.render
    val job = DiesOnce.Job.name
    assertEquals(stats.value("okay_job_foreign_restarts_total", job), 1.0, text)
    assert(stats.value("okay_job_foreign_interpreters_opened_total", job) >= 2.0, text)
    assert(text.contains(s"""okay_job_foreign_restarts_total{job="$job"} 1"""), text)
    assert(text.linesIterator.exists(_.startsWith("okay_job_foreign_interpreters{")), text)
    assert(text.linesIterator.exists(_.startsWith("okay_job_foreign_borrowed{")), text)
  }
