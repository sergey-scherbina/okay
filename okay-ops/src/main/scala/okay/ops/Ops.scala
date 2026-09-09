package okay.ops

import okay.{!, Async, pure}
import okay.codec.{Json, Schema}
import okay.http.{Http, Request, Response}
import okay.persist.{Offsets, Store, Topic}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The admin surface as a thin okay-http route (specs/ops.md): any
 * server composes it in beside its own routes, exactly as
 * `Secure.bearer` composes protection — `okay-ops` never opens a
 * socket of its own.
 */
object Ops:

  given Schema[Store.PartitionStats] = Schema.derived
  given Schema[Store.TopicStats] = Schema.derived
  given Schema[Store.Stats] = Schema.derived

  private def text(status: Int, body: String, ctype: String = "text/plain; charset=utf-8"): Response ! Async =
    pure(Response(status, Seq("content-type" -> ctype), Http.one(body.getBytes(UTF_8))))

  /** `GET /healthz`, `/readyz`, `/stats`, `/metrics` over `store`.
   * `lagOf` (topic groups to report consumer lag for) is optional —
   * a store keeps no registry of its own consumer groups; `guards`
   * (breakers, bulkheads, limiters), a `lifecycle` (which also makes
   * `/readyz` answer 503 while draining) and `red` (per-route
   * request metrics) join `/metrics` the same way */
  def routes(store: Store, lagOf: Vector[(String, Offsets, Vector[Topic])] = Vector.empty,
             guards: Vector[okay.resilience.Reporting[?]] = Vector.empty,
             pools: Vector[(String, () => okay.sql.Pool.Stats)] = Vector.empty,
             sagas: Vector[() => okay.persist.Saga.Status] = Vector.empty,
             lifecycle: Option[Lifecycle] = None,
             red: Vector[Red] = Vector.empty)
  : PartialFunction[Request, Response ! Async] =
    case r if r.method == okay.http.Method.Get && r.url == "/healthz" =>
      val h = Health.of(store)
      text(if h.live then 200 else 503, s"live=${h.live}" + h.reason.fold("")(x => s" ($x)"))
    case r if r.method == okay.http.Method.Get && r.url == "/readyz" =>
      // liveness above stays true while draining: an un-live pod is
      // restarted, an un-ready one is taken out of the endpoints
      val h = Health.of(store)
      if lifecycle.exists(_.draining) then text(503, "ready=false (draining)")
      else text(if h.ready then 200 else 503, s"ready=${h.ready}" + h.reason.fold("")(x => s" ($x)"))
    case r if r.method == okay.http.Method.Get && r.url == "/stats" =>
      text(200, Json.encode(summon[Schema[Store.Stats]])(store.stats), "application/json")
    case r if r.method == okay.http.Method.Get && r.url == "/metrics" =>
      text(200, Prom.render(store.stats, lagOf) + Prom.guards(guards) + Prom.pools(pools) + Prom.sagas(sagas)
        + lifecycle.fold("")(Prom.lifecycle) + Prom.red(red),
        "text/plain; version=0.0.4; charset=utf-8")
