package okay.spark

import okay.*
import okay.given
import okay.Tables.{read, of}
import okay.RowLift.plus
import okay.Direct.{direct, unary_!}
import org.apache.spark.sql.SparkSession
import java.io.File

/**
 * Where the time goes, stage by stage, on both platforms (bulk-rewrite,
 * 2026-09-09). Written to test the backlog's claim that the RDD-level
 * join was why the seam's build read 18 s against 7 s through
 * DataFrames — and the claim was wrong: the plan builds in 6.8 s, the
 * 18 s was `cache` persisting 4.6M boxed elements through Java
 * serialization. Kept as the profile to re-read before the next
 * optimisation is believed.
 */
class TestWroclawStages extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  val gtfs = { val h = new File("okay-spark/target/data/gtfs"); if h.isDirectory then h else new File("target/data/gtfs") }
  def file(name: String): String = new File(gtfs, name).getPath
  override def munitIgnore: Boolean = !new File(gtfs, "stop_times.txt").isFile
  lazy val spark = SparkSession.builder().master("local[4]").appName("stages").config("spark.ui.enabled", "false")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer") // persist is serialization; Java's cost 18 s here
    .getOrCreate()
  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  def timed[A](label: String)(a: => A): A =
    val t0 = System.nanoTime(); val r = a
    println(f"  $label%-42s ${(System.nanoTime() - t0) / 1000000}%,7d ms"); r

  def stages[D[_]](B: Bulk[D], name: String): Unit =
    println(s"  --- $name")
    // 1. read + count: the CSV -> Map rows cost
    timed("read stop_times -> Map rows, count") {
      Tables.run(B)(read(file("stop_times.txt")).aggregate(Aggregator.count[Csv.Row])) }
    // 2. read + select pair + count
    timed("read + select(pair), count") {
      Tables.run(B)(read(file("stop_times.txt")).select(r => r("trip_id") -> r("departure_time")).aggregate(Aggregator.count[(String, String)])) }
    // 3. the three joins, count (no expand)
    timed("three joins, count") {
      Tables.run(B)(direct {
        val st = !read(file("stop_times.txt")).select(r => r("trip_id") -> r("departure_time"))
        val tr = !read(file("trips.txt")).select(r => r("trip_id") -> (r("route_id"), r("service_id")))
        val ro = !read(file("routes.txt")).select(r => r("route_id") -> (r("route_type2_id").toInt == 31))
        val ca = !read(file("calendar.txt")).select(r => r("service_id") -> r("start_date"))
        !st.join(tr).select { case (_, (t, (r, s))) => r -> (t, s) }.join(ro).select { case (_, ((t, s), tram)) => s -> (t, tram) }.join(ca)
          .aggregate(Aggregator.count[Any])
      }) }
    // 4. the full program incl. expand, count
    timed("full departures (with expand), count") {
      Tables.run(B)(Gtfs.departures(file).aggregate(Aggregator.count[Dep])) }
    // 5. cache, then count twice: what persisting costs, and what it buys
    timed("cache + count + count") {
      Tables.run(B)(direct {
        val d = !Gtfs.departures(file).cache
        (!d.aggregate(Aggregator.count[Dep]), !d.aggregate(Aggregator.count[Dep]))
      }) }

  test("stages") {
    stages(SparkBulk(spark), "spark")
    stages(okay.localBulk, "local")
  }
