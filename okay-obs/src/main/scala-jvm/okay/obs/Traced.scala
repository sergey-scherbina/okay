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

  /** the trace a process-wide logger may stamp its lines with: the ids of
   * the span answering on THIS thread, set while `route` runs a request's
   * answer, none off a request (traced-route-named). A line told on another
   * thread carries none — never another request's */
  def context: Option[(String, String)] = Option(here.get)
  private val here = new ThreadLocal[(String, String)]

  /** the span's name when the caller gives none: the method and the path */
  def methodAndPath(req: Request): String = s"${req.method.toString.toUpperCase} ${pathOf(req.url)}"

  /**
   * `name` names the root span. The default is the method and path; a
   * caller with a route table passes the matched TEMPLATE, which keeps the
   * label set bounded — one per route, not one per url. The answer's status
   * is on the span (`http.status`), and a 5xx marks it an error where only a
   * throw did.
   */
  def route(tracer: () => Tracer, name: Request => String = methodAndPath)(r: Tracer ?=> Route): Route =
    // definedness must not depend on a live tracer: probe with one
    // that never runs a body (routes match on the request alone)
    lazy val probe: Route = r(using tracer())
    new PartialFunction[Request, Response ! Async]:
      def isDefinedAt(req: Request): Boolean = probe.isDefinedAt(req)
      def apply(req: Request): Response ! Async =
        val t = tracer()
        def h(name: String) = req.headers.collectFirst {
          case (k, v) if k.equalsIgnoreCase(name) => v }
        async {
          // the sync root keeps its finally (a throwing route still
          // closes its span); Loom runs the answer to readiness inside
          t.root(name(req), h("traceparent"), h("tracestate")) {
            t.context.foreach(here.set)
            try
              val res = okay.!.run(Async.run[Response, Nothing](r(using t)(req)))
              t.annotate(Attr("http.status", res.status.toString))
              if res.status >= 500 then t.fail()
              res
            finally here.remove()
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
