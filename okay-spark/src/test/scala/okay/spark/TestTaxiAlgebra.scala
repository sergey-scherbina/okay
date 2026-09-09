package okay.spark

import okay.{Aggregator, Chunks, Monoid, sliding}
import okay.given // Group[N] for every Numeric — the window's evidence
import SparkInterop.*
import org.apache.spark.sql.SparkSession
import java.io.File
import java.time.LocalDateTime

/** One trip, reduced to the four numbers this demo aggregates. */
final case class Trip(minute: Int, hour: Int, fare: Double, tip: Double)

/** A running maximum: a Monoid with no inverse — nothing un-sees a peak. */
final case class Peak(value: Double)
object Peak:
  given Monoid[Peak] with
    def empty: Peak = Peak(Double.NegativeInfinity)
    def combine(x: Peak, y: Peak): Peak = if x.value >= y.value then x else y

/**
 * The aggregation algebra against real data on real Spark: NYC yellow
 * taxi trips, January 2024 (the TLC's own parquet, ~3M rows).
 *
 * `Live`-tagged: it wants a downloaded file and a Spark session, which
 * is exactly what the default gate must not depend on. Fetch with
 *
 *   curl -o okay-spark/target/data/yellow_tripdata_2024-01.parquet \
 *     https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2024-01.parquet
 */
class TestTaxiAlgebra extends munit.FunSuite:
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  val data: File =
    val here = new File("okay-spark/target/data/yellow_tripdata_2024-01.parquet")
    if here.isFile then here else new File("target/data/yellow_tripdata_2024-01.parquet")

  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean = javaFeature >= 24 || !data.isFile

  override def beforeAll(): Unit =
    if javaFeature >= 24 then println("  taxi demo: skipped — Spark 4.0.0 wants Java 17 or 21")
    else if !data.isFile then println(s"  taxi demo: skipped — no ${data.getPath}")

  lazy val spark: SparkSession = SparkSession.builder()
    .master("local[4]").appName("okay-taxi-algebra")
    .config("spark.ui.enabled", "false")
    .getOrCreate()

  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  // The month as trips. Tips are only recorded for card payments, so a
  // tip figure over cash rides would be a statistic about zeros.
  lazy val trips: org.apache.spark.rdd.RDD[Trip] =
    val t0 = System.nanoTime()
    val df = spark.read.parquet(data.getPath)
      .selectExpr(
        "CAST(tpep_pickup_datetime AS TIMESTAMP) AS t",
        "CAST(fare_amount AS DOUBLE) AS fare",
        "CAST(tip_amount AS DOUBLE) AS tip",
        "CAST(payment_type AS LONG) AS pay")
      .where("pay = 1 AND fare > 0 AND tip >= 0")
      .where("t >= TIMESTAMP '2024-01-01 00:00:00' AND t < TIMESTAMP '2024-02-01 00:00:00'")
    val rdd = df.rdd.map { r =>
      val t = r.getAs[java.sql.Timestamp]("t").toLocalDateTime
      val minute = (t.getDayOfMonth - 1) * 1440 + t.getHour * 60 + t.getMinute
      Trip(minute, t.getHour, r.getAs[Double]("fare"), r.getAs[Double]("tip"))
    }.persist(org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK)
    val n = rdd.count() // materialise, so no timing below is a parquet read
    println(f"  loaded $n%,d card-paid trips in ${(System.nanoTime() - t0) / 1000000}%,d ms")
    rdd

  // ---------------------------------------------------------------- the algebra
  val rides = Aggregator.count[Trip]
  val fares = Aggregator.sum[Double].contramap[Trip](_.fare)
  val tips = Aggregator.sum[Double].contramap[Trip](_.tip)

  /** tips as a percentage of fares: two sums, one pass, presented as their ratio */
  val tipPct = fares.zip(tips).map((f, t) => t / f * 100.0)

  /** three statistics per hour of the day, still one pass */
  val hourly = Aggregator.groupBy((t: Trip) => t.hour)(rides.zip(fares).zip(tipPct))

  test("the same aggregator: distributed on Spark, and local over Chunks") {
    trips.count() // load and cache OUTSIDE the timer: no lane here is a parquet read
    val t0 = System.nanoTime()
    val onSpark = aggregate(trips)(hourly)
    val sparkMs = (System.nanoTime() - t0) / 1000000

    val rows = trips.collect()
    val t1 = System.nanoTime()
    val local = hourly.present(Chunks.fold(Chunks.fromIterator(rows.iterator))(using hourly.fold))
    val localMs = (System.nanoTime() - t1) / 1000000

    println(f"  aggregate: spark(4 partitions) ${sparkMs}%,d ms · local single pass ${localMs}%,d ms")
    println("  hour    rides         fares    tip%   $/ride")
    for h <- 0 to 23 do
      val ((n, rev), tip) = onSpark(h)
      println(f"  $h%4d  $n%,9d  $$$rev%,12.2f  $tip%5.2f%%  $$${rev / n}%6.2f")

    // the answers this pass exists for
    def by[A: Ordering](f: ((Long, Double), Double) => A) =
      onSpark.toSeq.sortBy((_, v) => f(v._1, v._2))
    val byRides = by((c, _) => c._1)
    val byTip = by((_, t) => t)
    val byFare = by((c, _) => c._2 / c._1)
    println(f"  busiest hours: ${byRides.takeRight(3).reverse.map(_._1).mkString(", ")}" +
      f" · quietest: ${byRides.take(3).map(_._1).mkString(", ")}")
    println(f"  best tipping hour: ${byTip.last._1} at ${byTip.last._2._2}%.2f%%" +
      f" · worst: ${byTip.head._1} at ${byTip.head._2._2}%.2f%%")
    println(f"  fattest fare hour: ${byFare.last._1} at $$${byFare.last._2._1._2 / byFare.last._2._1._1}%.2f a ride" +
      f" · thinnest: ${byFare.head._1} at $$${byFare.head._2._1._2 / byFare.head._2._1._1}%.2f")

    assertEquals(onSpark.keySet, local.keySet)
    for h <- onSpark.keys do
      val ((ns, revS), tipS) = onSpark(h)
      val ((nl, revL), tipL) = local(h)
      assertEquals(ns, nl, s"count differs at hour $h")
      assert(math.abs(revS - revL) / revL < 1e-9, s"fares at $h: $revS vs $revL")
      assert(math.abs(tipS - tipL) < 1e-9, s"tip% at $h: $tipS vs $tipL")
  }

  test("a group is a window: 44,640 minutes of January, rolling revenue") {
    val perMinute = aggregate(trips)(Aggregator.groupBy((t: Trip) => t.minute)(fares))
    val minutes = 31 * 1440
    val series = LazyList.tabulate(minutes)(m => perMinute.getOrElse(m, 0.0))

    def bench(w: Int): (Vector[Double], Double, Double) =
      // once to warm the JIT, then the round that is reported
      sliding(series)(w).drop(w - 1).foreach(_ => ())
      series.sliding(w).map(_.sum).foreach(_ => ())
      val t0 = System.nanoTime()
      val rolling = sliding(series)(w).drop(w - 1).toVector
      val groupMs = (System.nanoTime() - t0) / 1000000.0
      val t1 = System.nanoTime()
      val naive = series.sliding(w).map(_.sum).toVector
      val naiveMs = (System.nanoTime() - t1) / 1000000.0
      assertEquals(rolling.length, naive.length)
      rolling.zip(naive).zipWithIndex.foreach { case ((a, b), i) =>
        assert(math.abs(a - b) / math.max(1.0, b) < 1e-9, s"window $w at $i: $a vs $b") }
      (rolling, groupMs, naiveMs)

    for w <- List(60, 1440) do
      val (rolling, groupMs, naiveMs) = bench(w)
      val (peakValue, peakEnd) = rolling.zipWithIndex.maxBy(_._1)
      val end = LocalDateTime.of(2024, 1, 1, 0, 0).plusMinutes(peakEnd + w)
      println(f"  window $w%5d min: ${rolling.length}%,d windows · subtract-what-aged-out $groupMs%6.1f ms" +
        f" · recompute $naiveMs%7.1f ms (${naiveMs / groupMs}%4.1fx) · peak $$$peakValue%,.0f ending $end")
  }

  test("no inverse, no window — the compile error is the point") {
    val err = compileErrors("okay.sliding(LazyList(Peak(1.0), Peak(2.0)))(2)")
    assert(err.contains("Group[Peak]") || err.contains("okay.Group"), err)
    println(s"  refused, as it should be: ${err.linesIterator.find(_.contains("Group")).getOrElse(err)}")
  }
