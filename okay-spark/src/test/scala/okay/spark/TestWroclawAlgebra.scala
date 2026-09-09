package okay.spark

import okay.{Aggregator, Chunks, Monoid, sliding}
import okay.given // Group[N] for every Numeric — the window's evidence
import SparkInterop.*
import org.apache.spark.sql.{DataFrame, SparkSession}
import java.io.File
import java.time.{LocalDate, LocalDateTime}

/** One scheduled departure: a vehicle leaving a stop at a known minute. */
final case class Dep(minute: Int, hour: Int, tram: Boolean, route: Int)

/** A running maximum: a Monoid with no inverse — nothing un-sees a peak. */
final case class Busiest(value: Double)
object Busiest:
  given Monoid[Busiest] with
    def empty: Busiest = Busiest(Double.NegativeInfinity)
    def combine(x: Busiest, y: Busiest): Busiest = if x.value >= y.value then x else y

/**
 * The aggregation algebra on the city's own timetable: Wrocław's GTFS
 * feed, expanded from 1.16M scheduled stop times into every departure
 * of the fortnight it is valid for.
 *
 * `Live`-tagged: it wants a downloaded feed and a Spark session, which
 * is exactly what the default gate must not depend on. Fetch with
 *
 *   curl -sL https://api.open-data.cui.wroclaw.pl/od2-files/131/download/ \
 *     -o okay-spark/target/data/wroclaw_gtfs.zip &&
 *   unzip -o okay-spark/target/data/wroclaw_gtfs.zip \
 *     -d okay-spark/target/data/gtfs
 *
 * (od2-files/131 is one weekly snapshot; the current list of snapshots
 * is https://api.open-data.cui.wroclaw.pl/od2/6/.)
 */
class TestWroclawAlgebra extends munit.FunSuite:
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  val gtfs: File =
    val here = new File("okay-spark/target/data/gtfs")
    if here.isDirectory then here else new File("target/data/gtfs")

  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean =
    javaFeature >= 24 || !new File(gtfs, "stop_times.txt").isFile

  override def beforeAll(): Unit =
    if javaFeature >= 24 then println("  wroclaw demo: skipped — Spark 4.0.0 wants Java 17 or 21")
    else if munitIgnore then println(s"  wroclaw demo: skipped — no ${gtfs.getPath}/stop_times.txt")

  lazy val spark: SparkSession = SparkSession.builder()
    .master("local[4]").appName("okay-wroclaw-algebra")
    .config("spark.ui.enabled", "false")
    .getOrCreate()

  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  /** GTFS files carry a BOM, so the first column arrives with one glued on. */
  def gtfsFile(name: String): DataFrame =
    val df = spark.read.option("header", "true").csv(new File(gtfs, name).getPath)
    df.withColumnRenamed(df.columns.head, df.columns.head.replace("﻿", ""))

  /**
   * The timetable is a PLAN, and a plan is not yet events: one row of
   * stop_times is a departure on every date its service pattern runs.
   * Expanding it is the join the aggregation is then asked about.
   */
  lazy val departuresRdd: org.apache.spark.rdd.RDD[Dep] =
    val t0 = System.nanoTime()
    val stopTimes = gtfsFile("stop_times.txt").selectExpr("trip_id", "departure_time")
    val trips = gtfsFile("trips.txt").selectExpr("trip_id", "route_id", "service_id")
    val routes = gtfsFile("routes.txt").selectExpr("route_id", "CAST(route_type2_id AS INT) AS kind")
    val calendar = gtfsFile("calendar.txt").selectExpr(
      "service_id", "CAST(start_date AS INT) AS d0", "CAST(end_date AS INT) AS d1",
      "CAST(monday AS INT) AS w1", "CAST(tuesday AS INT) AS w2", "CAST(wednesday AS INT) AS w3",
      "CAST(thursday AS INT) AS w4", "CAST(friday AS INT) AS w5", "CAST(saturday AS INT) AS w6",
      "CAST(sunday AS INT) AS w7")

    val joined = stopTimes.join(trips, "trip_id").join(routes, "route_id").join(calendar, "service_id")
    val firstDay = calendar.selectExpr("MIN(d0)").collect().head.getInt(0)
    val day0 = LocalDate.parse(firstDay.toString, java.time.format.DateTimeFormatter.BASIC_ISO_DATE)

    val rdd = joined.rdd.flatMap { r =>
      val hhmmss = r.getAs[String]("departure_time")
      val h = hhmmss.substring(0, 2).toInt
      val m = hhmmss.substring(3, 5).toInt
      val kind = r.getAs[Int]("kind")
      val route = r.getAs[String]("route_id").hashCode
      val d0 = LocalDate.parse(r.getAs[Int]("d0").toString, java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
      val d1 = LocalDate.parse(r.getAs[Int]("d1").toString, java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
      val runs = (1 to 7).map(i => r.getAs[Int](s"w$i") == 1)
      Iterator.iterate(d0)(_.plusDays(1)).takeWhile(!_.isAfter(d1))
        .filter(d => runs(d.getDayOfWeek.getValue - 1))
        .map { d =>
          val dayIndex = (d.toEpochDay - day0.toEpochDay).toInt
          // an "25:10" departure is 01:10 the next day, and lands there
          Dep(dayIndex * 1440 + h * 60 + m, (h * 60 + m) / 60 % 24, kind == 31, route)
        }
    }.persist(org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK)

    val n = rdd.count() // materialise, so no timing below is a CSV read
    println(f"  $n%,d scheduled departures from ${day0} (${(System.nanoTime() - t0) / 1000000}%,d ms)")
    rdd

  // ---------------------------------------------------------------- the algebra
  val departures = Aggregator.count[Dep]
  val trams = Aggregator.sum[Long].contramap[Dep](d => if d.tram then 1L else 0L)

  /** what share of departures is rail: two counts, one pass, presented as a ratio */
  val tramShare = departures.zip(trams).map((n, t) => t * 100.0 / n)

  /** how much of the network is awake: exact distinct routes */
  val routesRunning = Aggregator.distinct[Int].contramap[Dep](_.route)

  /** three statistics per hour of the day, one pass */
  val hourly = Aggregator.groupBy((d: Dep) => d.hour)(departures.zip(routesRunning).zip(tramShare))

  test("routes are counted by a hash, and the hash does not collide here") {
    // .iterator.map, not .map: with `import okay.given` in scope the Id
    // monad's extension gets in the way of ArrayOps on an Array[Row]
    val ids = gtfsFile("routes.txt").select("route_id").collect().iterator.map(_.getString(0)).toVector
    assertEquals(ids.map(_.hashCode).distinct.length, ids.distinct.length,
      "two route ids share a hashCode — `distinct` would undercount")
    println(f"  ${ids.distinct.length}%d routes, ${ids.map(_.hashCode).distinct.length}%d distinct hashes")
  }

  test("the same aggregator: distributed on Spark, and local over Chunks") {
    departuresRdd.count()
    val t0 = System.nanoTime()
    val onSpark = aggregate(departuresRdd)(hourly)
    val sparkMs = (System.nanoTime() - t0) / 1000000

    val rows = departuresRdd.collect()
    val t1 = System.nanoTime()
    val local = hourly.present(Chunks.fold(Chunks.fromIterator(rows.iterator))(using hourly.fold))
    val localMs = (System.nanoTime() - t1) / 1000000

    println(f"  aggregate: spark(4 partitions) ${sparkMs}%,d ms · local single pass ${localMs}%,d ms")
    println("  hour  departures   routes   tram%")
    for h <- 0 to 23 do
      val ((n, routes), tram) = onSpark(h)
      println(f"  $h%4d  $n%,10d  $routes%7d  $tram%5.1f%%")

    val byDeps = onSpark.toSeq.sortBy((_, v) => v._1._1)
    val byTram = onSpark.toSeq.sortBy((_, v) => v._2)
    println(f"  busiest hours: ${byDeps.takeRight(3).reverse.map(_._1).mkString(", ")}" +
      f" · quietest: ${byDeps.take(3).map(_._1).mkString(", ")}")
    println(f"  most rail: ${byTram.last._1} at ${byTram.last._2._2}%.1f%% tram" +
      f" · least: ${byTram.head._1} at ${byTram.head._2._2}%.1f%%")

    assertEquals(onSpark.keySet, local.keySet)
    for h <- onSpark.keys do
      assertEquals(onSpark(h)._1, local(h)._1, s"counts differ at hour $h")
      assert(math.abs(onSpark(h)._2 - local(h)._2) < 1e-9, s"tram% at $h")
  }

  test("a group is a window: the fortnight minute by minute, rolling") {
    val perMinute = aggregate(departuresRdd)(Aggregator.groupBy((d: Dep) => d.minute)(departures))
    val minutes = perMinute.keys.max + 1
    val series = LazyList.tabulate(minutes)(m => perMinute.getOrElse(m, 0L).toDouble)

    def bench(w: Int): (Vector[Double], Double, Double) =
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
        assert(math.abs(a - b) < 1e-6, s"window $w at $i: $a vs $b") }
      (rolling, groupMs, naiveMs)

    val day0 = LocalDate.parse("20260906", java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
    for w <- List(60, 1440) do
      val (rolling, groupMs, naiveMs) = bench(w)
      val (peak, peakEnd) = rolling.zipWithIndex.maxBy(_._1)
      val end = LocalDateTime.of(day0, java.time.LocalTime.MIDNIGHT).plusMinutes(peakEnd + w)
      println(f"  window $w%5d min: ${rolling.length}%,d windows · subtract-what-aged-out $groupMs%6.1f ms" +
        f" · recompute $naiveMs%7.1f ms (${naiveMs / groupMs}%5.1fx) · peak ${peak}%,.0f departures ending $end")
  }

  test("no inverse, no window — the compile error is the point") {
    val err = compileErrors("okay.sliding(LazyList(Busiest(1.0), Busiest(2.0)))(2)")
    assert(err.contains("Group[Busiest]") || err.contains("okay.Group"), err)
    println(s"  refused, as it should be: ${err.linesIterator.find(_.contains("Group")).getOrElse(err)}")
  }
