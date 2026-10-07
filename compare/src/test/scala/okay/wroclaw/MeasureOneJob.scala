package okay.wroclaw



import okay.{Tables, Bulk, BulkParallel}
import okay.given
import okay.cluster.FlowBulk
import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** docs/one-job-everywhere.md: the job on one JVM — local, parallel, the
 * cluster engine. Live: it wants the downloaded feed */
class MeasureOneJob extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")
  override def munitIgnore: Boolean = !Gtfs.present
  def file(name: String): String = new File(Gtfs.dir, name).getPath
  val lines: String => Iterator[String] = p => Files.lines(Path.of(p)).iterator().asScala
  val bytes: String => Option[Long] = p => Some(Files.size(Path.of(p)))

  test("one job: local, parallel, the engine") {
    val (n0, local) = OneJob.timed(3)(() => Tables.run(Bulk.local(lines, bytes))(OneJob.departures(file)))
    val (n1, par) = OneJob.timed(3)(() => Tables.run(BulkParallel(4, lines, bytes))(OneJob.departures(file)))
    val (n2, flow) = OneJob.timed(3)(() => Tables.run(FlowBulk(4, lines, bytes))(OneJob.departures(file)))
    println(f"  ONE-JOB local      $local%,7d ms  ($n0%,d rows)")
    println(f"  ONE-JOB parallel4  $par%,7d ms  ($n1%,d rows)")
    println(f"  ONE-JOB engine4    $flow%,7d ms  ($n2%,d rows)")
    assertEquals(Set(n0, n1, n2).size, 1)
  }
