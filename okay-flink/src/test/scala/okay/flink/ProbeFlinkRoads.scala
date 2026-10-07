package okay.flink

import okay.*

import okay.wroclaw.{Gtfs, OneJob}
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import _root_.java.io.File

/** PROBE (flink-typed-road), ignored by default: the one-job page's job on
 * Flink, road by road — where the 29.8 s went. Best of 2 each. Un-ignore and
 * run with the feed (OKAY_WROCLAW_GTFS) to re-measure on a new Flink */
class ProbeFlinkRoads extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(30, "min")
  override def munitIgnore: Boolean = !Gtfs.present
  def file(name: String): String = new File(Gtfs.dir, name).getAbsolutePath

  test("the roads, one by one".ignore) {
    def env = StreamExecutionEnvironment.createLocalEnvironment(4)
    val roads = Vector(
      "first cut (again, warm): client csv, windowed coGroup" -> (() => FlinkBulk.FlinkBulk(env, csvInTask = false, objectReuse = false, joinBy = FlinkBulk.JoinBy.WindowedCoGroup)),
      "first cut: client csv, windowed coGroup" -> (() => FlinkBulk.FlinkBulk(env, csvInTask = false, objectReuse = false, joinBy = FlinkBulk.JoinBy.WindowedCoGroup)),
      "csv in task" -> (() => FlinkBulk.FlinkBulk(env, csvInTask = true, objectReuse = false, joinBy = FlinkBulk.JoinBy.WindowedCoGroup)),
      "+ object reuse" -> (() => FlinkBulk.FlinkBulk(env, csvInTask = true, objectReuse = true, joinBy = FlinkBulk.JoinBy.WindowedCoGroup)),
      "+ keyed-state join" -> (() => FlinkBulk.FlinkBulk(env, csvInTask = true, objectReuse = true, joinBy = FlinkBulk.JoinBy.Process)),
      "+ STREAMING mode (no sort)" -> (() => FlinkBulk.FlinkBulk(env, joinBy = FlinkBulk.JoinBy.Process,
        mode = org.apache.flink.api.common.RuntimeExecutionMode.STREAMING)),
      "BATCH, parallelism 1" -> (() => FlinkBulk.FlinkBulk(StreamExecutionEnvironment.createLocalEnvironment(1))),
      "STREAMING, parallelism 1" -> (() => FlinkBulk.FlinkBulk(StreamExecutionEnvironment.createLocalEnvironment(1),
        mode = org.apache.flink.api.common.RuntimeExecutionMode.STREAMING)))
    val (n0, _) = OneJob.timed(1)(() => Tables.run(okay.localBulk)(OneJob.departures(file)))
    for (name, make) <- roads do
      val (n, ms) = OneJob.timed(2)(() => Tables.run(make())(OneJob.departures(file)))
      println(f"  ROAD $name%-42s $ms%,7d ms  ($n%,d rows)")
      assertEquals(n, n0, name)
  }
