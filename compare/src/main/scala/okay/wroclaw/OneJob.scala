package okay.wroclaw

import okay.*
import okay.Tables.{read, join, select}

/**
 * ONE JOB, WRITTEN ONCE (streams-seam lane 4; docs/one-job-everywhere.md):
 * the Wrocław timetable's joins — every scheduled stop departure with its
 * trip's route and service, whether the route is a tram, and when its
 * service starts — as a `Tables` program that names no platform. The
 * same value runs on `localBulk`, `parallelBulk`, the cluster engine
 * (`FlowBulk`), Spark (`SparkBulk`) and Flink (`FlinkBulk`); only the
 * instance changes. `file` resolves a feed file's name to its path.
 */
object OneJob:
  def departures(file: String => String): Long ! Tables =
    val stopTimes = read(file("stop_times.txt")).columns("trip_id", "departure_time")
      .select(r => r("trip_id") -> r("departure_time"))
    val trips = read(file("trips.txt")).columns("trip_id", "route_id", "service_id")
      .select(r => r("trip_id") -> (r("route_id"), r("service_id")))
    val routes = read(file("routes.txt")).columns("route_id", "route_type2_id")
      .select(r => r("route_id") -> (r("route_type2_id") == "31"))
    val calendar = read(file("calendar.txt")).columns("service_id", "start_date")
      .select(r => r("service_id") -> r("start_date"))
    stopTimes.join(trips).select { case (_, (time, (route, service))) => route -> (time, service) }
      .join(routes).select { case (_, ((time, service), tram)) => service -> (time, tram) }
      .join(calendar).aggregate(Aggregator.count[Any])

  /** best of `rounds` wall-clock runs, and the answer */
  def timed[A](rounds: Int)(run: () => A): (A, Long) =
    var best = Long.MaxValue
    var answer: Option[A] = None
    for _ <- 1 to rounds do
      val t0 = System.nanoTime()
      answer = Some(run())
      best = best min (System.nanoTime() - t0) / 1000000
    (answer.get, best)
