package okay.wroclaw

import okay.Csv
import java.io.File
import java.time.{LocalDate, ZoneId}
import scala.collection.mutable

/** one route of the network, the table the pipeline enriches against */
final case class RouteInfo(id: String, name: String, tram: Boolean)

/** what one derivation of the feed produced: the events in ARRIVAL
 * order, the routes table both lanes join against, and what the run
 * covers (printed by the harness, so a number is never quoted without
 * the data it came from) */
final case class Feed(events: Array[Depart], routes: Array[RouteInfo], stops: Int,
                      from: String, to: String)

/**
 * WHERE THE STREAM COMES FROM. Wrocław publishes its public-transport
 * timetable as GTFS on the city's open-data portal
 * (https://open-data.cui.wroclaw.pl — dataset "Rozkład jazdy
 * transportu publicznego"); this benchmark uses snapshot 131,
 * OtwartyWroclaw_rozklad_jazdy_GTFS_06092026, valid 2026-09-06 ..
 * 2026-09-20:
 *
 *   mkdir -p okay-flink/target/data &&
 *   curl -sL https://api.open-data.cui.wroclaw.pl/od2-files/131/download/ \
 *     -o okay-flink/target/data/wroclaw_gtfs.zip &&
 *   unzip -o okay-flink/target/data/wroclaw_gtfs.zip -d okay-flink/target/data/gtfs
 *
 * A TIMETABLE IS NOT A STREAM, and the difference is the whole point
 * of the derivation below. GTFS is a PLAN: 1 158 821 rows of "trip t
 * is due at stop s at 06:12:00", each one a departure on every date
 * its service pattern runs. What a streaming engine consumes is what
 * actually happened, in the order a message bus delivered it, so the
 * plan is turned into events by three additions, all deterministic
 * (one splitmix hash of trip, stop-sequence and day — no `Random`,
 * no clock), so that every run of the benchmark, in either engine,
 * sees the same events in the same order:
 *
 *   - a DELAY per departure: an exponential tail scaled by the hour
 *     (rush hours are worse) plus a creep along the trip, which is
 *     what makes the windowed statistics non-trivial;
 *   - an EVENT TIME = scheduled + delay, which is not the order the
 *     events arrive in;
 *   - an ARRIVAL JITTER of up to 25 s, which IS the arrival order.
 *
 * The jitter is bounded BELOW the 30 s watermark bound the two lanes
 * use (Job.Lateness), and that is not a detail: it means no event is
 * ever late, both engines therefore see complete windows, and the two
 * answers can be asserted EQUAL rather than "close". A benchmark
 * whose lanes compute different things measures nothing.
 */
object Gtfs {

  /** the city's own zone — a GTFS clock time is local time */
  val Zone: ZoneId = ZoneId.of("Europe/Warsaw")

  /** where the unpacked feed is expected (the module's target dir) */
  def dir: File =
    val here = new File("okay-flink/target/data/gtfs")
    if here.isDirectory then here else new File("target/data/gtfs")

  def present: Boolean = new File(dir, "stop_times.txt").isFile

  /** the file's lines, and the handle closed when the caller is done */
  private def read[A](name: String)(f: Iterator[String] => A): A =
    val src = scala.io.Source.fromFile(new File(dir, name), "UTF-8")
    try f(src.getLines()) finally src.close()

  /** splitmix64 — a deterministic hash, so the "randomness" is a pure
   * function of the row and the day */
  private def mix(x: Long): Long =
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)

  /** GTFS clock time, which may pass 24:00:00 for an after-midnight trip */
  private def seconds(hhmmss: String): Int =
    val c = hhmmss.split(':')
    if c.length < 3 then -1 else c(0).toInt * 3600 + c(1).toInt * 60 + c(2).toInt

  /**
   * The feed, read once, replayed over `days` consecutive service
   * days from the first date it is valid for.
   *
   * @return the events in ARRIVAL order, and the routes table
   */
  def events(days: Int): Feed = {
    // --- routes: id -> index, and whether it is a tram (route_type2_id 31)
    val routeIdx = mutable.HashMap.empty[String, Int]
    val routes = mutable.ArrayBuffer.empty[RouteInfo]
    read("routes.txt") { ls =>
      for r <- Csv.rows(ls) do
        routeIdx(r("route_id")) = routes.length
        routes += RouteInfo(r("route_id"), r("route_short_name"), r("route_type2_id").toInt == 31)
    }

    // --- calendar: which weekdays a service pattern runs, and from when
    val week = Vector("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
    val runs = mutable.HashMap.empty[String, Vector[Boolean]]
    var first = Long.MaxValue
    read("calendar.txt") { ls =>
      for r <- Csv.rows(ls) do
        runs(r("service_id")) = week.map(d => r(d) == "1")
        first = math.min(first, epochDay(r("start_date")))
    }

    // --- trips: trip_id -> (route, service); one trip is one vehicle run
    val tripIdx = mutable.HashMap.empty[String, Int]
    val tripRoute = mutable.ArrayBuffer.empty[Int]
    val tripDays = mutable.ArrayBuffer.empty[Vector[Boolean]]
    read("trips.txt") { ls =>
      for r <- Csv.rows(ls) do
        routeIdx.get(r("route_id")).foreach { ri =>
          tripIdx(r("trip_id")) = tripRoute.length
          tripRoute += ri
          tripDays += runs.getOrElse(r("service_id"), Vector.fill(7)(false))
        }
    }

    // --- stop_times: 1.16M rows, kept as primitives (no Map per row)
    val stopIdx = mutable.HashMap.empty[Int, Int]
    val stopId = mutable.ArrayBuffer.empty[Int]
    val stTrip = mutable.ArrayBuilder.ofInt()
    val stSec = mutable.ArrayBuilder.ofInt()
    val stStop = mutable.ArrayBuilder.ofInt()
    val stSeq = mutable.ArrayBuilder.ofInt()
    read("stop_times.txt") { it =>
      val header = it.next().stripPrefix("﻿").split(',')
      val cTrip = header.indexOf("trip_id")
      val cDep = header.indexOf("departure_time")
      val cStop = header.indexOf("stop_id")
      val cSeq = header.indexOf("stop_sequence")
      while it.hasNext do
        val f = it.next().split(',')
        if f.length > cSeq then
          tripIdx.get(f(cTrip)).foreach { t =>
            val s = seconds(f(cDep))
            if s >= 0 then {
              val id = f(cStop).toInt
              stTrip += t
              stSec += s
              // stop ids are compacted to DENSE indices: both lanes key by
              // this, and a dense index is what lets the okay lane pack
              // (window, stop) into one Long
              stStop += stopIdx.getOrElseUpdate(id, { stopId += id; stopId.length - 1 })
              stSeq += f(cSeq).toInt
            }
          }
    }
    val trip = stTrip.result(); val sec = stSec.result()
    val stop = stStop.result(); val seq = stSeq.result()

    // --- the days to replay, and which trips run on each
    val day0 = LocalDate.ofEpochDay(first)
    val dates = (0 until days).map(day0.plusDays(_)).toArray
    val dayStart = dates.map(_.atStartOfDay(Zone).toInstant.toEpochMilli)
    val dow = dates.map(_.getDayOfWeek.getValue - 1)
    val trips = tripRoute.length
    val runsOn = Array.ofDim[Boolean](days, trips)
    for d <- 0 until days; t <- 0 until trips do runsOn(d)(t) = tripDays(t)(dow(d))

    var total = 0
    for d <- 0 until days; i <- trip.indices do if runsOn(d)(trip(i)) then total += 1

    // --- the events themselves, and the key they ARRIVE in
    require(total < (1 << 23), s"$total events do not fit the packed sort key; lower `days`")
    val out = new Array[Depart](total)
    val order = new Array[Long](total) // (arrival offset ms << 23) | index
    var n = 0
    var d = 0
    while d < days do
      val base = dayStart(d)
      var i = 0
      while i < trip.length do
        val t = trip(i)
        if runsOn(d)(t) then
          val r = mix(t.toLong * 1315423911L + seq(i).toLong * 2654435761L + d.toLong * 0x2545f4914f6cdd1dL)
          val hour = (sec(i) / 3600) % 24
          val rush = if (hour >= 6 && hour < 9) || (hour >= 14 && hour < 18) then 1.0 else 0.35
          val u = (r >>> 11).toDouble / (1L << 53).toDouble
          val delay = math.max(-90, math.min(1800,
            math.round(-math.log(1.0 - u) * 90.0 * rush + seq(i) * 1.5 - 25.0).toInt))
          val ts = base + sec(i).toLong * 1000L + delay.toLong * 1000L
          out(n) = new Depart(ts, tripRoute(t), stop(i), t, delay)
          val jitter = ((r >>> 5) % 25000L).toInt
          order(n) = ((ts - dayStart(0) + jitter) << 23) | n.toLong
          n += 1
        i += 1
      d += 1

    java.util.Arrays.sort(order)
    val arrived = new Array[Depart](total)
    var k = 0
    while k < total do { arrived(k) = out((order(k) & 0x7fffffL).toInt); k += 1 }
    Feed(arrived, routes.toArray, stopId.length, dates.head.toString, dates.last.toString)
  }

  private def epochDay(yyyymmdd: String): Long =
    LocalDate.parse(yyyymmdd, java.time.format.DateTimeFormatter.BASIC_ISO_DATE).toEpochDay
}
