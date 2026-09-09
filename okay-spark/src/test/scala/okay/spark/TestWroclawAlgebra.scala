package okay.spark

import okay.*
import okay.given // Group[N] for every Numeric, and the local Bulk[Chunks]
import okay.Tables.{Table, read, of}
import okay.Chunks.elements
import okay.Sort.sortBy
import okay.RowLift.plus
import okay.Direct.{direct, unary_!}
import org.apache.spark.sql.SparkSession
import java.io.File
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter.BASIC_ISO_DATE

/** One scheduled departure: a vehicle leaving a stop at a known minute. */
final case class Dep(minute: Int, hour: Int, tram: Boolean, route: Int)

/** A running maximum: a Monoid with no inverse — nothing un-sees a peak. */
final case class Busiest(value: Double)
object Busiest:
  given Monoid[Busiest] with
    def empty: Busiest = Busiest(Double.NegativeInfinity)
    def combine(x: Busiest, y: Busiest): Busiest = if x.value >= y.value then x else y

/** a GTFS service pattern: which weekdays, between which dates */
final case class Service(from: Long, to: Long, days: Vector[Boolean]):
  /** every date this pattern runs on, as epoch days */
  def dates: Iterator[Long] =
    Iterator.iterate(from)(_ + 1).takeWhile(_ <= to)
      .filter(d => days(LocalDate.ofEpochDay(d).getDayOfWeek.getValue - 1))

/**
 * WHERE THE DATA COMES FROM. Wrocław publishes its public-transport
 * timetable as GTFS on the city's open-data portal
 * (https://open-data.cui.wroclaw.pl — dataset "Rozkład jazdy transportu
 * publicznego", /hdb/metadane/13/). The portal's API lists the weekly
 * snapshots at https://api.open-data.cui.wroclaw.pl/od2/6/ ; this demo
 * uses snapshot 131, OtwartyWroclaw_rozklad_jazdy_GTFS_06092026, valid
 * 2026-09-06 .. 2026-09-20:
 *
 *   curl -sL https://api.open-data.cui.wroclaw.pl/od2-files/131/download/ \
 *     -o okay-spark/target/data/wroclaw_gtfs.zip &&
 *   unzip -o okay-spark/target/data/wroclaw_gtfs.zip -d okay-spark/target/data/gtfs
 *
 * Four of its files are read: stop_times.txt (1,158,821 scheduled stop
 * departures, one per trip and stop), trips.txt (the route and the
 * service pattern of each trip), routes.txt (route_type2_id 31 is a
 * tram), calendar.txt (which weekdays a service pattern runs, and the
 * dates the feed is valid for).
 *
 * THE ETL, SAID ONCE — AS A VALUE. `departures` is a program of the
 * `Tables` effect (specs/bulk.md, the effect layer): it names no
 * platform, it can be printed before it runs (`!.tracing`), and the
 * test runs the SAME value on Spark and on the local `Chunks` platform
 * and asserts the two agree. A timetable is a PLAN, not events — one
 * stop_times row is a departure on every date its service pattern runs
 * — so the last step expands it.
 */
object Gtfs:
  def departures(file: String => String): Table[Dep] ! Tables = direct {
    // `columns` is structural, so the rewrite pushes it into the read and
    // the platform prunes at the parser; `select` is a function, opaque
    val stopTimes = !read(file("stop_times.txt")).columns("trip_id", "departure_time")
      .select(r => r("trip_id") -> r("departure_time"))
    val trips = !read(file("trips.txt")).columns("trip_id", "route_id", "service_id")
      .select(r => r("trip_id") -> (r("route_id"), r("service_id")))
    val routes = !read(file("routes.txt")).columns("route_id", "route_type2_id")
      .select(r => r("route_id") -> (r("route_type2_id").toInt == 31))
    val calendar = !read(file("calendar.txt")).select { r =>
      val days = Vector("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday").map(r(_) == "1")
      r("service_id") -> Service(epochDay(r("start_date")), epochDay(r("end_date")), days)
    }
    // the first day of the feed, by the algebra: the minimum start date
    val day0 = (!calendar.aggregate(Aggregator.min[Long].contramap((kv: (String, Service)) => kv._2.from))).get

    !stopTimes.join(trips)                                                      // trip    -> (time, (route, service))
      .select { case (_, (time, (route, service))) => route -> (time, service) } // route   -> (time, service)
      .join(routes)                                                             // route   -> ((time, service), tram)
      .select { case (route, ((time, service), tram)) => service -> (time, tram, route.hashCode) }
      .join(calendar)                                                           // service -> ((time, tram, route), Service)
      .expand { case (_, ((time, tram, route), service)) =>
        val h = time.substring(0, 2).toInt // GTFS lets a late trip run past 24:00
        val m = time.substring(3, 5).toInt
        service.dates.map { d =>
          // an "25:10" departure is 01:10 the next day, and lands there
          Dep(((d - day0).toInt) * 1440 + h * 60 + m, (h * 60 + m) / 60 % 24, tram, route)
        }
      }
  }

  private def epochDay(yyyymmdd: String): Long = LocalDate.parse(yyyymmdd, BASIC_ISO_DATE).toEpochDay

/**
 * The aggregation algebra on the city's own timetable, on two
 * platforms from one program.
 *
 * `Live`-tagged: it wants the downloaded feed and a Spark session,
 * which is exactly what the default gate must not depend on.
 */
class TestWroclawAlgebra extends munit.FunSuite:
  override def munitTests(): Seq[Test] =
    super.munitTests().map(_.tag(new munit.Tag("Live")))

  val gtfs: File =
    val here = new File("okay-spark/target/data/gtfs")
    if here.isDirectory then here else new File("target/data/gtfs")
  def file(name: String): String = new File(gtfs, name).getPath

  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean =
    javaFeature >= 24 || !new File(gtfs, "stop_times.txt").isFile

  override def beforeAll(): Unit =
    if javaFeature >= 24 then println("  wroclaw demo: skipped — Spark 4.0.0 wants Java 17 or 21")
    else if munitIgnore then println(s"  wroclaw demo: skipped — no ${gtfs.getPath}/stop_times.txt")

  lazy val spark: SparkSession = SparkSession.builder()
    .master("local[4]").appName("okay-wroclaw-algebra")
    .config("spark.ui.enabled", "false")
    .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer") // persist is serialization; Java's cost 18 s here
    .getOrCreate()

  override def afterAll(): Unit = if !munitIgnore then spark.stop()

  val sparkBulk = SparkBulk(spark)
  val localBulk: Bulk[Chunks] = okay.localBulk

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

  /** the monoid of counting: Acc = Long, init 0, merge + */
  val departures = Aggregator.count[Dep]

  /** the same monoid, over a value the input carries rather than 1 */
  val trams = Aggregator.sum[Long].contramap[Dep](d => if d.tram then 1L else 0L)

  /** what share of departures is rail: `zip` is the PRODUCT of two
   * monoids, so both counts ride one pass; `map` moves the ratio into
   * `present`, leaving the monoid alone */
  val tramShare = departures.zip(trams).map((n, t) => t * 100.0 / n)

  /** how much of the network is awake: the monoid of sets, init empty, merge union */
  val routesRunning = Aggregator.distinct[Int].contramap[Dep](_.route)

  /** per hour of the day, one pass: `groupBy` is the monoid of finite
   * MAPS into a monoid — the empty map its unit, and merging two maps
   * merges the accumulators of the keys they share. Every partition
   * builds its own 24-key map; the merge is what makes them one. */
  val hourly = Aggregator.groupBy((d: Dep) => d.hour)(departures.zip(routesRunning).zip(tramShare))

  /** how many minutes the fortnight has, and how many departures each */
  val perMinute = Aggregator.groupBy((d: Dep) => d.minute)(departures)

  /**
   * THE WHOLE ANALYSIS AS ONE PROGRAM, over a row that says what it
   * needs: tables, and a sort. `Sort` is not in `Bulk` — it is an
   * operation added beside the seam. Spark answers it natively
   * (`SparkBulk.sort`), the local platform through the primitives
   * (`Sort.viaTables`); the program is the same value both times.
   */
  def analysis(file: String => String)
  : (Long, Map[Int, ((Long, Long), Double)], Map[Int, Long], Vector[(Int, Long)]) ! (Tables + Sort) = direct {
    // a mark takes the block's own row: a `! Tables` program says `.plus[Sort]`
    val deps = !Gtfs.departures(file).cache.plus[Sort]
    val n = !deps.aggregate(departures).plus[Sort]
    val hours = !deps.aggregate(hourly).plus[Sort]
    val minutes = !deps.aggregate(perMinute).plus[Sort]
    val busiest = !of(minutes.toVector).plus[Sort]
    val sorted = !busiest.sortBy((kv: (Int, Long)) => -kv._2).plus[Tables]
    val top = !sorted.collect.plus[Sort]
    (n, hours, minutes, top.elements.take(3).toVector)
  }

  lazy val onSpark =
    val t0 = System.nanoTime()
    val plans = scala.collection.mutable.ListBuffer.empty[String]
    val r = State.run(Tables.Heap.empty[SparkBulk.Rows])(sparkBulk.sort(Tables.via(sparkBulk, p => plans += Tables.Plan.show(p))(analysis(file))))._2
    // the deepest plan forced is the departures' lineage — printed after the rewrite, as Spark ran it
    println("  the plan Spark ran, after the rewrite:\n" + plans.maxBy(_.linesIterator.size).linesIterator.map("    " + _).mkString("\n"))
    println(f"  spark: ${r._1}%,d scheduled departures, the whole analysis in ${(System.nanoTime() - t0) / 1000000}%,d ms")
    r
  lazy val local =
    val t0 = System.nanoTime()
    val r = Tables.run(localBulk)(Sort.viaTables(analysis(file)))
    println(f"  local: ${r._1}%,d departures from the same program, ${(System.nanoTime() - t0) / 1000000}%,d ms")
    r

  test("the plan, printed before anything runs") {
    val traced = okay.!.tracing(Gtfs.departures(file).plus[okay.Pure])([X] => (e: Tables[X]) => e.productPrefix)
    val handled = State.handle(Tables.Heap.empty[Chunks])(Tables.via(localBulk)(traced))
    val (plan, _) = okay.!.run(okay.Writer.run(handled))
    println(s"  plan: ${plan.mkString(" ")}")
    assertEquals(plan.count(_ == "Join"), 3)
  }

  test("routes are counted by a hash, and the hash does not collide here") {
    val (routes, hashes) = Tables.run(localBulk)(
      read(file("routes.txt")).select(_("route_id"))
        .aggregate(Aggregator.distinct[String].zip(Aggregator.distinct[Int].contramap(_.hashCode))))
    assertEquals(hashes, routes, "two route ids share a hashCode — `distinct` would undercount")
    println(f"  $routes%d routes, $hashes%d distinct hashes")
  }

  test("the same program on Spark and in one JVM: equal departures, equal hours") {
    val (_, sparkHours, _, sparkTop) = onSpark
    val (_, localHours, _, localTop) = local
    println("  hour  departures   routes   tram%")
    for h <- 0 to 23 do
      val ((n, routes), tram) = sparkHours(h)
      println(f"  $h%4d  $n%,10d  $routes%7d  $tram%5.1f%%")
    val byDeps = sparkHours.toSeq.sortBy((_, v) => v._1._1)
    val byTram = sparkHours.toSeq.sortBy((_, v) => v._2)
    println(f"  busiest hours: ${byDeps.takeRight(3).reverse.map(_._1).mkString(", ")}" +
      f" · quietest: ${byDeps.take(3).map(_._1).mkString(", ")}")
    println(f"  most rail: ${byTram.last._1} at ${byTram.last._2._2}%.1f%% tram" +
      f" · least: ${byTram.head._1} at ${byTram.head._2._2}%.1f%%")
    println(s"  busiest minutes (Spark's sort / the local one): $sparkTop / $localTop")

    assertEquals(onSpark._1, local._1)
    assertEquals(sparkHours.keySet, localHours.keySet)
    for h <- sparkHours.keys do
      assertEquals(sparkHours(h)._1, localHours(h)._1, s"counts differ at hour $h")
      assert(math.abs(sparkHours(h)._2 - localHours(h)._2) < 1e-9, s"tram% at $h")
    assertEquals(sparkTop, localTop)
  }

  /**
   * WHERE THE GROUP IS. A `Group[A]` is a `Monoid[A]` plus `inverse`,
   * with the law `combine(a, inverse(a)) == empty`. For `Double` that
   * given is `(0.0, +, negate)` — the summing monoid with subtraction —
   * and `sliding` is written against exactly those three: it admits the
   * newcomer with `combine(acc, x)` and drops what aged out with
   * `combine(_, inverse(oldest))`, so a window step costs one add and
   * one subtract no matter how wide the window is.
   */
  test("a group is a window: the fortnight minute by minute, rolling") {
    val perMinute = onSpark._3
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

    val day0 = LocalDate.parse("20260906", BASIC_ISO_DATE)
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

  /**
   * What that compile error is protecting. The instance CAN be written:
   * a running maximum has an `empty` and a `combine`, and `inverse` will
   * accept anything that type-checks. Then the window is wrong from the
   * first value it is asked to forget, because `combine(a, inverse(a))`
   * is `a`, not `empty` — no value un-sees a peak. The compiler can only
   * ask for the instance; the law is the guarantee, and this is the test
   * of it.
   */
  test("a fake inverse compiles, and the window lies") {
    given fake: Group[Busiest] with
      def empty: Busiest = Busiest(Double.NegativeInfinity)
      def combine(x: Busiest, y: Busiest): Busiest = if x.value >= y.value then x else y
      def inverse(a: Busiest): Busiest = a // nothing else fits

    assertNotEquals(fake.combine(Busiest(9), fake.inverse(Busiest(9))), fake.empty, "the law would hold")

    val series = LazyList(3.0, 9.0, 1.0, 1.0, 1.0, 2.0).map(Busiest(_))
    val truth = series.sliding(2).map(_.map(_.value).max).toList
    val viaGroup = sliding(series)(2).drop(1).toList.map(_.value)
    assertEquals(truth, List(9.0, 9.0, 1.0, 1.0, 2.0))
    assertEquals(viaGroup, List(9.0, 9.0, 9.0, 9.0, 9.0), "the window came down after all")
    println(s"  fake inverse: a window of 2 says $viaGroup where the truth is $truth")
  }
