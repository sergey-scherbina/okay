package okay.obs

import okay.codec.Json

/**
 * The OTLP half that is PURE (specs/obs.md, Export = a consumer):
 * spans as this stack keeps them become the OTLP/HTTP JSON any
 * collector ingests at /v1/traces — Jaeger, Tempo, a vendor. No SDK:
 * the payload is a documented JSON shape, and a documented shape is
 * a mapping, not a dependency.
 *
 * OTLP details honored: 64-bit nanos travel as STRINGS (the JSON
 * proto convention), status code 1 is OK and 2 carries the error
 * message, a root span simply omits parentSpanId.
 */
object Otlp {

  /** one batch of spans as one /v1/traces request body */
  def body(service: String, spans: Vector[Span]): Json =
    Json.JObj(Vector("resourceSpans" -> Json.JArr(Vector(Json.JObj(Vector(
      "resource" -> Json.JObj(Vector("attributes" -> Json.JArr(Vector(
        attr("service.name", service))))),
      "scopeSpans" -> Json.JArr(Vector(Json.JObj(Vector(
        "scope" -> Json.JObj(Vector("name" -> Json.JStr("okay-obs"))),
        "spans" -> Json.JArr(spans.map(span))))))))))))

  private def span(s: Span): Json =
    val base = Vector(
      "traceId" -> Json.JStr(s.traceId),
      "spanId" -> Json.JStr(s.spanId)) ++
      s.parentId.map(p => "parentSpanId" -> Json.JStr(p)).toVector ++ Vector(
      "name" -> Json.JStr(s.name),
      "startTimeUnixNano" -> Json.JStr((s.start * 1000000L).toString),
      "endTimeUnixNano" -> Json.JStr((s.end * 1000000L).toString),
      "kind" -> Json.JNum(1),                       // INTERNAL — the honest default
      "attributes" -> Json.JArr(s.attrs.map(a => attr(a.key, a.value))),
      "status" -> status(s.status))
    Json.JObj(base)

  private def status(s: String): Json =
    if s == "ok" then Json.JObj(Vector("code" -> Json.JNum(1)))
    else Json.JObj(Vector("code" -> Json.JNum(2),
      "message" -> Json.JStr(s.stripPrefix("error: "))))

  private def attr(k: String, v: String): Json =
    Json.JObj(Vector("key" -> Json.JStr(k),
      "value" -> Json.JObj(Vector("stringValue" -> Json.JStr(v)))))
}
