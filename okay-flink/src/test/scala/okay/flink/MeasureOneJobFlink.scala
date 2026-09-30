package okay.flink

import okay.*
import okay.wroclaw.{Gtfs, OneJob}
import _root_.java.io.File

/** docs/one-job-everywhere.md: the same job on Flink (MiniCluster, 4). Live */
class MeasureOneJobFlink extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")
  override def munitIgnore: Boolean = !Gtfs.present
  def file(name: String): String = new File(Gtfs.dir, name).getAbsolutePath

  test("one job: Flink") {
    val flink = FlinkBulk.local(4)
    val (n, ms) = OneJob.timed(3)(() => Tables.run(flink)(OneJob.departures(file)))
    val (n0, _) = OneJob.timed(1)(() => Tables.run(okay.localBulk)(OneJob.departures(file)))
    println(f"  ONE-JOB flink4     $ms%,7d ms  ($n%,d rows)")
    assertEquals(n, n0)
  }
