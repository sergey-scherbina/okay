package okay.ops

import okay.*
import okay.codec.Schema
import okay.http.{Http, Request, Response}
import okay.resilience.Attempt

/**
 * RED — rate, errors, duration — per route and per outbound client
 * (specs/ops.md, "RED metrics"): a value with a Schema, a wrapper
 * for a server's routes and one for an `Http`, and a rendering in
 * Prometheus's histogram shape. A request is counted by its status
 * CLASS (`2xx` … `5xx`, or `exception` when the route threw); an
 * error is a 5xx or a throw. Durations go into fixed cumulative
 * buckets — fixed, because histograms with different buckets cannot
 * be aggregated across services, which is what a histogram is for.
 */
final class Red(val name: String, clock: () => Long = () => System.currentTimeMillis):
  import Red.*

  private val cell = TRef(Map.empty[String, Series])

  def stats: Stats = Stats(name, cell.get.values.toVector.sortBy(_.route))

  /** what one request did: its class, whether it was an error, and how long */
  def observe(route: String, statusClass: String, error: Boolean, millis: Long): Unit =
    cell.modify { m =>
      val s = m.getOrElse(route, Series(route, 0L, 0L, Vector.empty, 0L, Vector.fill(buckets.size)(0L)))
      val idx = buckets.indexWhere(millis <= _)
      val counts = if idx < 0 then s.buckets else s.buckets.updated(idx, s.buckets(idx) + 1)
      (m.updated(route, s.copy(
        requests = s.requests + 1,
        errors = s.errors + (if error then 1 else 0),
        byClass = bump(s.byClass, statusClass),
        sumMillis = s.sumMillis + millis,
        buckets = counts)), ())
    }

  /** a server's routes, measured; defined exactly where `routes` is */
  def route(label: Request => String)(routes: PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async] = {
    case r if routes.isDefinedAt(r) => measured(label(r))(routes(r))
  }

  /** an outbound client, measured */
  def http(label: Request => String)(inner: Http): Http = new Http:
    def send(r: Request): Response ! Async = measured(label(r))(inner.send(r))

  private def measured(route: String)(prog: => Response ! Async): Response ! Async =
    okay.async(clock()).flatMap { start =>
      Attempt(prog).map { out =>
        val took = clock() - start
        out match
          case Right(res) => observe(route, classOf(res.status), res.status >= 500, took); res
          case Left(t) => observe(route, "exception", true, took); throw t
      }
    }

object Red:
  /** the upper bounds, milliseconds — the usual HTTP set */
  val buckets: Vector[Long] = Vector(5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)

  /** requests in one status class (`2xx` … `5xx`, `exception`) */
  final case class Count(cls: String, n: Long) derives Schema

  final case class Series(route: String, requests: Long, errors: Long,
                          byClass: Vector[Count], sumMillis: Long,
                          /** counts per bucket, NOT cumulative — the
                            * rendering accumulates, so the value stays
                            * additive across nodes */
                          buckets: Vector[Long]) derives Schema
  final case class Stats(name: String, series: Vector[Series]) derives Schema

  def classOf(status: Int): String = s"${status / 100}xx"

  private def bump(cs: Vector[Count], cls: String): Vector[Count] =
    cs.indexWhere(_.cls == cls) match
      case -1 => cs :+ Count(cls, 1L)
      case i => cs.updated(i, cs(i).copy(n = cs(i).n + 1))

  /** the label most servers want: method and path, query dropped */
  val byMethodAndPath: Request => String =
    r => s"${r.method.name} ${r.url.takeWhile(_ != '?')}"
