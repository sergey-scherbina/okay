package okay.spark

import okay.*
import okay.given
import okay.codec.Schema
import okay.Row.plus
import okay.Chunks.elements
import okay.Tables.{collect, select}
import okay.sql.{Query, Structured}
import okay.sql.Structured.{matching, joinOn}
import org.apache.spark.sql.SparkSession

/**
 * `SparkFrames` (specs/streams-seam.md, lane 5): a DataFrame enters a
 * `Tables` program and leaves it, and the structural operators on a
 * DataFrame-born table run in Catalyst — the law is the SAME ANSWER as
 * `Structured.viaTables` on the local platform, and Catalyst's own plan
 * is read to show the operator reached it.
 */
/** top level: a case class inside the suite carries `$outer`, and Spark
 * would have to ship the suite to an executor with every row */
final case class FrameTrip(id: Long, route: String, tram: Boolean)
final case class FrameStop(trip: Long, time: String, seq: Int)
object FrameRows:
  given Schema[FrameTrip] = Schema.derived
  given Schema[FrameStop] = Schema.derived

class TestSparkFrames extends munit.FunSuite {
  import FrameRows.given
  type Trip = FrameTrip
  type Stop = FrameStop
  val Trip = FrameTrip
  val Stop = FrameStop
  val route = Query.field[Trip, String]("route").toOption.get
  val tram = Query.field[Trip, Boolean]("tram").toOption.get
  val tripId = Query.field[Trip, Long]("id").toOption.get
  val stopTrip = Query.field[Stop, Long]("trip").toOption.get
  val seq = Query.field[Stop, Int]("seq").toOption.get

  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean = javaFeature == 24
  lazy val spark = SparkSession.builder().master("local[2]").appName("okay-spark-frames")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = if !munitIgnore then spark.stop()
  lazy val frames = SparkFrames(spark, SparkBulk(spark))

  val trips = Vector.tabulate(30)(i => Trip(i.toLong, s"r${i % 5}", i % 3 == 0))
  val stops = Vector.tabulate(200)(i => Stop((i * 7 % 35).toLong, f"${i % 24}%02d:${i % 60}%02d", i % 13))
  def local[A](p: A ! Tables + Structured): A = Tables.run(localBulk)(Structured.viaTables(p))

  test("matching on a loaded DataFrame answers the local answer, and the filter is in Catalyst's plan") {
    val w = (route === "r1" or route === "r3") and (tram === true)
    val got = frames.run(for
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      m <- t.matching(w).plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.queryExecution.analyzed.toString))
    assertEquals(got._1, local(Tables.of(trips).plus[Structured].matching(w).collect.map(_.elements.toVector.sortBy(_.id))))
    // the ANALYZED plan: over rows already in memory Catalyst's optimizer
    // evaluates the Filter itself and leaves a LocalRelation, which proves
    // it had the filter and says nothing a test can read
    assert(got._2.contains("Filter"), got._2)
  }

  test("joinOn of two loaded DataFrames is a Catalyst join, and answers the local join") {
    val got = frames.run(for
      s <- frames.load[Stop](SparkSchema.dataFrame(spark, stops)).plus[Tables + Structured]
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      j <- s.joinOn(t)(stopTrip, tripId).plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      rows <- j.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield rows.elements.toVector.sortBy((s, _) => (s.trip, s.seq, s.time)))
    val expected = local(Tables.of(stops).plus[Structured].joinOn(Tables.of(trips).plus[Structured])(stopTrip, tripId)
      .collect.map(_.elements.toVector.sortBy((s, _) => (s.trip, s.seq, s.time))))
    assertEquals(got, expected)
    assert(got.nonEmpty)
  }

  test("a Parquet read pruned to A's fields takes matching as a pushed filter") {
    val dir = java.nio.file.Files.createTempDirectory("frames").resolve("trips.parquet").toString
    SparkSchema.dataFrame(spark, trips).write.parquet(dir)
    val got = frames.run(for
      t <- frames.read[Trip](dir, "parquet").plus[Tables + Structured]
      m <- t.matching(route === "r2").plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.queryExecution.executedPlan.toString))
    assertEquals(got._1, trips.filter(_.route == "r2"))
    assert(got._2.contains("PushedFilters: [") && got._2.contains("EqualTo(route,r2)"), got._2)
  }

  test("after an opaque step the table leaves Catalyst, and a structural step still answers through the RDD") {
    val got = frames.run(for
      t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
      u <- t.select(x => x.copy(route = x.route.toUpperCase)).plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
      m <- u.matching(route === "R4").plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
      df <- frames.frame[Trip](m).plus[Tables + Structured]
      rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
    yield (rows.elements.toVector.sortBy(_.id), df.collect().length))
    assertEquals(got._1, trips.filter(_.route == "r4").map(x => x.copy(route = "R4")))
    assertEquals(got._2, got._1.length, "frame of a table that is not DataFrame-born encodes its rows")
  }
}
