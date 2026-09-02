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
   * a store keeps no registry of its own consumer groups */
  def routes(store: Store, lagOf: Vector[(String, Offsets, Vector[Topic])] = Vector.empty)
  : PartialFunction[Request, Response ! Async] =
    case r if r.method == okay.http.Method.Get && r.url == "/healthz" =>
      val h = Health.of(store)
      text(if h.live then 200 else 503, s"live=${h.live}" + h.reason.fold("")(x => s" ($x)"))
    case r if r.method == okay.http.Method.Get && r.url == "/readyz" =>
      val h = Health.of(store)
      text(if h.ready then 200 else 503, s"ready=${h.ready}" + h.reason.fold("")(x => s" ($x)"))
    case r if r.method == okay.http.Method.Get && r.url == "/stats" =>
      text(200, Json.encode(summon[Schema[Store.Stats]])(store.stats), "application/json")
    case r if r.method == okay.http.Method.Get && r.url == "/metrics" =>
      text(200, Prom.render(store.stats, lagOf), "text/plain; version=0.0.4; charset=utf-8")
