package okay.spark

// `import okay.*`, and the wildcard is the POINT of this line
// (named-tuple-unblock, 2026-09-12). While the universal `apply` in
// Generate.scala was unguarded, this file had to import okay's names
// one by one, because a named tuple's field access desugars to an
// apply by index and the wildcard captured it. The guard on that
// extension is what lets the ordinary import back in, and this file
// is the proof on real code: if the guard regressed, the twin below
// stops compiling. BUGS.md, `universal-apply-blocks-named-tuples`.
import okay.*
import okay.Tables.{Table, read}
import okay.Direct.{direct, unary_!}

/**
 * `Gtfs.departures` with its shapes NAMED (named-tuples-stage0).
 *
 * The original carries a comment on every line of its join chain
 * saying what the tuple holds; here the type says it, so those
 * comments are gone on purpose. A named tuple erases to the plain
 * one, so the claim attached to this file is that it computes the
 * SAME thing, asserted against the real feed in TestWroclawAlgebra.
 */
object GtfsNamed:
  def departures(file: String => String): Table[Dep] ! Tables = direct {
    val stopTimes = !read(file("stop_times.txt")).columns("trip_id", "departure_time")
      .select(r => r("trip_id") -> r("departure_time"))
    val trips = !read(file("trips.txt")).columns("trip_id", "route_id", "service_id")
      .select(r => r("trip_id") -> (route = r("route_id"), service = r("service_id")))
    val routes = !read(file("routes.txt")).columns("route_id", "route_type2_id")
      .select(r => r("route_id") -> (r("route_type2_id").toInt == 31))
    val calendar = !read(file("calendar.txt")).select { r =>
      val days = Vector("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday").map(r(_) == "1")
      r("service_id") -> Service(Gtfs.epochDay(r("start_date")), Gtfs.epochDay(r("end_date")), days)
    }
    val day0 = (!calendar.aggregate(Aggregator.min[Long].contramap((kv: (String, Service)) => kv._2.from))).get

    !stopTimes.join(trips)
      .select { case (_, (time, trip)) => trip.route -> (time = time, service = trip.service) }
      .join(routes)
      .select { case (route, (dep, tram)) => dep.service -> (time = dep.time, tram = tram, route = route.hashCode) }
      .join(calendar)
      .expand { case (_, (dep, service)) =>
        val h = dep.time.substring(0, 2).toInt
        val m = dep.time.substring(3, 5).toInt
        service.dates.map { d =>
          Dep(((d - day0).toInt) * 1440 + h * 60 + m, (h * 60 + m) / 60 % 24, dep.tram, dep.route)
        }
      }
  }
