package okay.spark



import okay.{Tables}
import okay.wroclaw.{Gtfs, OneJob}
import org.apache.spark.sql.SparkSession
import java.io.File

/** docs/one-job-everywhere.md: the same job on Spark (local[4]). Live */
class MeasureOneJobSpark extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")
  override def munitIgnore: Boolean = !Gtfs.present
  def file(name: String): String = new File(Gtfs.dir, name).getAbsolutePath
  lazy val spark = SparkSession.builder().master("local[4]").appName("one-job").config("spark.ui.enabled", "false")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer").getOrCreate()
  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  test("one job: Spark") {
    val (n, ms) = OneJob.timed(3)(() => Tables.run(SparkBulk(spark))(OneJob.departures(file)))
    val (n0, _) = OneJob.timed(1)(() => Tables.run(okay.localBulk)(OneJob.departures(file)))
    println(f"  ONE-JOB spark4     $ms%,7d ms  ($n%,d rows)")
    assertEquals(n, n0)
  }
