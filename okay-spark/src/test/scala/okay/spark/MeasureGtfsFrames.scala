package okay.spark



import okay.{Tables}
import okay.freer.*
import okay.Tables.read
import okay.Direct.{direct, unary_!}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.col
import java.io.File

/**
 * THE MEASURE OF LANE 5 (tables-structural; specs/streams-seam.md): the
 * same three joins over the Wrocław GTFS — stop_times ⋈ trips ⋈ routes ⋈
 * calendar, counted — three ways: through our seam on Spark (`SparkBulk`,
 * the RDD level, columns pruned at the parser), as a HAND-WRITTEN
 * DataFrame (what Catalyst does when it sees everything), and in one JVM
 * (`localBulk`). The docs had quoted "18 s here against 7 s through
 * DataFrames" with no DataFrame version anywhere in the repository, and
 * TestWroclawStages had already found the 18 s was `cache` under Java
 * serialization. This is the number the lane is justified by, or not.
 *
 * Rounds alternate the three arms (ratio-arms-must-alternate), best of
 * each kept; every arm must count the same rows. `Live`-tagged: it wants
 * the downloaded feed and a Spark session.
 */
class MeasureGtfsFrames extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  val gtfs = { val h = new File("okay-spark/target/data/gtfs"); if h.isDirectory then h else new File("target/data/gtfs") }
  def file(name: String): String = new File(gtfs, name).getPath
  override def munitIgnore: Boolean = !new File(gtfs, "stop_times.txt").isFile
  lazy val spark = SparkSession.builder().master("local[4]").appName("gtfs-frames").config("spark.ui.enabled", "false")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    .getOrCreate()
  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  /** the seam's program: the join chain of `Gtfs.departures`, without the expand */
  def joins: Long ! Tables = direct {
    val st = !read(file("stop_times.txt")).columns("trip_id", "departure_time").select(r => r("trip_id") -> r("departure_time"))
    val tr = !read(file("trips.txt")).columns("trip_id", "route_id", "service_id").select(r => r("trip_id") -> (r("route_id"), r("service_id")))
    val ro = !read(file("routes.txt")).columns("route_id", "route_type2_id").select(r => r("route_id") -> (r("route_type2_id").toInt == 31))
    val ca = !read(file("calendar.txt")).columns("service_id", "start_date").select(r => r("service_id") -> r("start_date"))
    !st.join(tr).select { case (_, (t, (r, s))) => r -> (t, s) }
      .join(ro).select { case (_, ((t, s), tram)) => s -> (t, tram) }
      .join(ca).aggregate(Aggregator.count[Any])
  }

  /** the same joins, as a DataFrame user writes them */
  def frames(): Long =
    def csv(name: String) =
      val df = spark.read.option("header", "true").csv(file(name))
      df.toDF(df.columns.map(_.stripPrefix("﻿"))*)
    val st = csv("stop_times.txt").select("trip_id", "departure_time")
    val tr = csv("trips.txt").select("trip_id", "route_id", "service_id")
    val ro = csv("routes.txt").select(col("route_id"), (col("route_type2_id") === "31").as("tram"))
    val ca = csv("calendar.txt").select("service_id", "start_date")
    st.join(tr, "trip_id").join(ro, "route_id").join(ca, "service_id").count()

  def timed[A](a: => A): (A, Long) =
    val t0 = System.nanoTime(); val r = a
    (r, (System.nanoTime() - t0) / 1000000)

  test("three joins over the GTFS: our seam on Spark, a hand DataFrame, one JVM") {
    val arms = Vector[(String, () => Long)](
      "seam on Spark (RDD)" -> (() => Tables.run(SparkBulk(spark))(joins)),
      "hand DataFrame" -> (() => frames()),
      "seam in one JVM" -> (() => Tables.run(okay.localBulk)(joins)))
    val best = Array.fill(arms.length)(Long.MaxValue)
    val counts = Array.fill(arms.length)(-1L)
    for round <- 1 to 3; (i, (name, run)) <- arms.indices.zip(arms) do
      val (n, ms) = timed(run())
      println(f"  round $round  $name%-22s $ms%,7d ms  ($n%,d rows)")
      best(i) = best(i) min ms
      counts(i) = n
    for (i, (name, _)) <- arms.indices.zip(arms) do println(f"  BEST $name%-22s ${best(i)}%,7d ms")
    assertEquals(counts.toSet.size, 1, s"the arms disagree: ${counts.toVector}")
  }
