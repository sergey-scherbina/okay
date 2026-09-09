package okay.spark

import okay.{Aggregator, Chunks, Group, Monoid, sliding}
import okay.given // Stream[LazyList, Pure] and friends
import SparkInterop.*
import org.apache.spark.sql.{DataFrame, SparkSession}
import java.io.File
import java.time.{LocalDate, LocalDateTime}

/** One scheduled departure: a vehicle leaving a stop at a known minute. */
final case class Dep(minute: Int, hour: Int, tram: Boolean, route: Int)

/**
 * How much service a stretch of time carries. This is the type the
 * whole demo aggregates into, and it is a GROUP:
 *
 *   empty                      the unit — no service at all
 *   combine(x, y)              two stretches side by side
 *   inverse(a)                 the same service, negated
 *
 * with `combine(a, inverse(a)) == empty` (asserted below). The
 * aggregation only ever needs the first two — that is the monoid, and
 * it is what lets Spark merge partial results from any partitioning.
 * The rolling window needs the third as well: `inverse` is how a minute
 * that has aged out of the window is REMOVED without recomputing what
 * is left. No inverse, no window.
 */
final case class Load(departures: Long, trams: Long):
  def tramPct: Double = if departures == 0 then 0.0 else trams * 100.0 / departures

object Load:
  given group: Group[Load] with
    def empty: Load = Load(0, 0)
    def combine(x: Load, y: Load): Load = Load(x.departures + y.departures, x.trams + y.trams)
    def inverse(a: Load): Load = Load(-a.departures, -a.trams)

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
  //
  // WHERE THE MONOID IS. An `Aggregator[In, Acc, Out]` is four
  // functions — `init`, `add`, `merge`, `present` — and two of them are
  // the algebra: `(init, merge)` is a MONOID on the accumulator `Acc`,
  // `init` its unit and `merge` its associative operation. `add` is how
  // an input acts on the accumulator, `present` the projection out of
  // it; neither is part of the monoid.
  //
  // That is the whole reason the same value runs distributed: Spark's
  // `aggregate(zero)(seqOp, combOp)` IS `(init, add, merge)`, and it is
  // associativity that licenses Spark to split the input into whatever
  // partitions it likes and combine them in whatever order it finishes
  // them. A merge that were not associative would give a different
  // answer per run and nobody would see it happen.

  /**
   * The service an hour carries. `fromMonoid` is the aggregator that IS
   * its algebra: no accumulator of its own, `init` is `Load.empty` and
   * `merge` is `Load.combine` — the group above, used here as a monoid.
   */
  val load = Aggregator.fromMonoid[Load].contramap[Dep](d => Load(1, if d.tram then 1 else 0))

  /** how much of the network is awake: the monoid of sets, init empty, merge union */
  val routesRunning = Aggregator.distinct[Int].contramap[Dep](_.route)

  /**
   * Both statistics per hour of the day, in one pass. `zip` is the
   * PRODUCT of two monoids — a pair of units, merged componentwise —
   * and `groupBy` the monoid of finite MAPS into a monoid: the unit is
   * the empty map, and merging two maps merges the accumulators of the
   * keys they share. Every partition builds its own 24-key map; the
   * merge is what makes the four of them one.
   */
  val hourly = Aggregator.groupBy((d: Dep) => d.hour)(load.zip(routesRunning))

  test("Load is a group: the law the window depends on") {
    val G = Load.group
    val samples = List(Load(0, 0), Load(1, 0), Load(17, 5), Load(4593288, 1500000))
    for a <- samples do assertEquals(G.combine(a, G.inverse(a)), G.empty, s"no inverse for $a")
    for a <- samples; b <- samples; c <- samples do
      assertEquals(G.combine(G.combine(a, b), c), G.combine(a, G.combine(b, c)), "not associative")
    println("  Load: combine(a, inverse(a)) == empty, and combine is associative")
  }

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
      val (l, routes) = onSpark(h)
      println(f"  $h%4d  ${l.departures}%,10d  $routes%7d  ${l.tramPct}%5.1f%%")

    val byDeps = onSpark.toSeq.sortBy((_, v) => v._1.departures)
    val byTram = onSpark.toSeq.sortBy((_, v) => v._1.tramPct)
    println(f"  busiest hours: ${byDeps.takeRight(3).reverse.map(_._1).mkString(", ")}" +
      f" · quietest: ${byDeps.take(3).map(_._1).mkString(", ")}")
    println(f"  most rail: ${byTram.last._1} at ${byTram.last._2._1.tramPct}%.1f%% tram" +
      f" · least: ${byTram.head._1} at ${byTram.head._2._1.tramPct}%.1f%%")

    assertEquals(onSpark.keySet, local.keySet)
    for h <- onSpark.keys do assertEquals(onSpark(h), local(h), s"hour $h differs")
  }

  /**
   * WHERE THE GROUP IS. The aggregation above used `Load`'s `empty` and
   * `combine` only — a monoid is all a merge needs. A rolling window
   * needs the third function: `sliding` admits the newcomer with
   * `combine(acc, x)` and drops what aged out with
   * `combine(_, inverse(oldest))`, so a step costs one add and one
   * subtract no matter how wide the window is, where recomputing costs
   * the width. Same type, same operation, one extra law.
   */
  test("a group is a window: the fortnight minute by minute, rolling") {
    val perMinute = aggregate(departuresRdd)(Aggregator.groupBy((d: Dep) => d.minute)(load))
    val minutes = perMinute.keys.max + 1
    val G = Load.group
    val series = LazyList.tabulate(minutes)(m => perMinute.getOrElse(m, G.empty))

    def bench(w: Int): (Vector[Load], Double, Double) =
      sliding(series)(w).drop(w - 1).foreach(_ => ())
      series.sliding(w).map(_.reduce(G.combine)).foreach(_ => ())
      val t0 = System.nanoTime()
      val rolling = sliding(series)(w).drop(w - 1).toVector
      val groupMs = (System.nanoTime() - t0) / 1000000.0
      val t1 = System.nanoTime()
      val naive = series.sliding(w).map(_.reduce(G.combine)).toVector
      val naiveMs = (System.nanoTime() - t1) / 1000000.0
      assertEquals(rolling, naive, s"the window of $w disagrees with recomputing it")
      (rolling, groupMs, naiveMs)

    val day0 = LocalDate.parse("20260906", java.time.format.DateTimeFormatter.BASIC_ISO_DATE)
    for w <- List(60, 1440) do
      val (rolling, groupMs, naiveMs) = bench(w)
      val (peak, peakEnd) = rolling.zipWithIndex.maxBy(_._1.departures)
      val end = LocalDateTime.of(day0, java.time.LocalTime.MIDNIGHT).plusMinutes(peakEnd + w)
      println(f"  window $w%5d min: ${rolling.length}%,d windows · subtract-what-aged-out $groupMs%6.1f ms" +
        f" · recompute $naiveMs%7.1f ms (${naiveMs / groupMs}%5.1fx) · peak ${peak.departures}%,d departures" +
        f" (${peak.tramPct}%.1f%% tram) ending $end")
  }

  test("no inverse, no window — the compile error is the point") {
    val err = compileErrors("okay.sliding(LazyList(Busiest(1.0), Busiest(2.0)))(2)")
    assert(err.contains("Group[Busiest]") || err.contains("okay.Group"), err)
    println(s"  refused, as it should be: ${err.linesIterator.find(_.contains("Group")).getOrElse(err)}")
  }
