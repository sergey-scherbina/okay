package okay.ops

import okay.*
import okay.codec.Schema
import okay.http.{Http, Request, Response}
import okay.resilience.Attempt

/**
 * Graceful shutdown as a value (specs/ops.md, "Graceful shutdown"):
 * a draining flag, an in-flight count, a route wrapper that counts
 * and — once draining — refuses, and a drain that waits for the
 * count to reach zero within a grace period. The sequence Kubernetes
 * needs is readiness off FIRST (`Ops.routes(store, lifecycle =
 * Some(l))` makes `/readyz` read this), then a delay for endpoint
 * removal to propagate, then the drain, then the region's release —
 * `Lifecycle.awaitSignal` (JVM) runs the first three on SIGTERM.
 *
 * One wrapper for every server, because the three here all take a
 * `PartialFunction[Request, Response ! Async]`.
 */
final class Lifecycle(clock: () => Long = () => System.currentTimeMillis):
  import Lifecycle.*

  private val cell = TRef(St(false, 0, 0L, None))

  def draining: Boolean = cell.get.draining
  def inFlight: Int = cell.get.inFlight

  def stats: Stats =
    val s = cell.get
    Stats(s.draining, s.inFlight, s.refused, s.drainingSince)

  /** stop taking new work: readiness answers false from here on */
  def beginDrain(): Unit =
    val now = clock()
    cell.modify(s => (if s.draining then s else s.copy(draining = true, drainingSince = Some(now)), ()))

  /** the routes, counted; while draining a new request is answered
    * 503 `Connection: close` and the route is not run */
  def route(routes: PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async] = {
    case r if routes.isDefinedAt(r) =>
      okay.async(enter()).flatMap { admitted =>
        if !admitted then pure(refusal)
        else Attempt(routes(r)).map { out =>
          leave()
          out.fold(t => throw t, identity)
        }
      }
  }

  /** wait until nothing is in flight: true, or false when the grace
    * ran out with requests still running */
  def drain(graceMillis: Long)(using Timer): Boolean ! Async =
    beginDrain()
    val until = clock() + graceMillis
    def go: Boolean ! Async =
      if inFlight == 0 then pure(true)
      else if clock() >= until then pure(false)
      else Async.sleep(pollMillis).flatMap(_ => go)
    go

  private def enter(): Boolean =
    cell.modify { s =>
      if s.draining then (s.copy(refused = s.refused + 1), false)
      else (s.copy(inFlight = s.inFlight + 1), true)
    }

  private def leave(): Unit =
    cell.modify(s => (s.copy(inFlight = s.inFlight - 1), ()))

  private def refusal: Response =
    Response(503, Seq(("content-type", "text/plain; charset=utf-8"), ("connection", "close")),
      Http.one("draining".getBytes("UTF-8")))

object Lifecycle:
  final case class Stats(draining: Boolean, inFlight: Int, refused: Long,
                         drainingSince: Option[Long]) derives Schema

  private final case class St(draining: Boolean, inFlight: Int, refused: Long,
                              drainingSince: Option[Long])

  /** how often the drain looks at the in-flight count */
  val pollMillis: Long = 20
