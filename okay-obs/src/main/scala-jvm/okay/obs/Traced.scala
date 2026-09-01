package okay.obs

import okay.{!, Async, async}
import okay.given
import okay.http.{Request, Response}

/**
 * The capability form of context carriage (specs/context-functions.md,
 * obs-traced-routes): specs/obs.md decided the current span is
 * HANDLER state, not an effect — and between an effect row and
 * hand-threading sits `Tracer ?=> Route`. A route written against
 * `using Tracer` serves under `Traced.route`, which installs a
 * PER-REQUEST tracer rooted from the inbound traceparent; a STORED
 * `Tracer ?=> Route` value self-wires at each installation site —
 * a library of already-traced routes as values.
 *
 * The root span covers the route's ANSWER (the Response is ready),
 * not the body's streaming — body-level spans are the child spans
 * the route opens itself. Stated, not hidden.
 */
object Traced {

  type Route = PartialFunction[Request, Response ! Async]

  def route(tracer: () => Tracer)(r: Tracer ?=> Route): Route =
    // definedness must not depend on a live tracer: probe with one
    // that never runs a body (routes match on the request alone)
    lazy val probe: Route = r(using tracer())
    new PartialFunction[Request, Response ! Async]:
      def isDefinedAt(req: Request): Boolean = probe.isDefinedAt(req)
      def apply(req: Request): Response ! Async =
        val t = tracer()
        def h(name: String) = req.headers.collectFirst {
          case (k, v) if k.equalsIgnoreCase(name) => v }
        val name = s"${req.method.toString.toUpperCase} ${pathOf(req.url)}"
        async {
          // the sync root keeps its finally (a throwing route still
          // closes its span); Loom runs the answer to readiness inside
          t.root(name, h("traceparent"), h("tracestate")) {
            okay.!.run(Async.run[Response, Nothing](r(using t)(req)))
          }
        }

  private def pathOf(url: String): String =
    val i = url.indexOf('?')
    val base = if i >= 0 then url.take(i) else url
    val j = base.indexOf("://")
    if j < 0 then base
    else
      val k = base.indexOf('/', j + 3)
      if k < 0 then "/" else base.drop(k)
}
