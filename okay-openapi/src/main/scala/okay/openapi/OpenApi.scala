package okay.openapi

import okay.codec.Json
import okay.codec.Json.*
import okay.{!, Async, pure}
import okay.http.{Method, Response, Route, Router}

/** what a document says about the service, which the router cannot know */
final case class Api(title: String, version: String,
                     servers: Vector[String] = Vector.empty,
                     description: Option[String] = None)

/**
 * The document is a RENDERING of the router that serves it
 * (specs/openapi.md).
 *
 * Nothing is declared twice: the paths are the ones that dispatch,
 * and a request body's schema is the one the decoder was derived
 * from. A route cannot then be served and undocumented, or documented
 * and unserved — the property the DESCRIBE interpreter was built for.
 *
 * OpenAPI 3.1, because its schema dialect IS JSON Schema: okay-codec's
 * `JsonSchema.of` drops straight in, where 3.0 would need a
 * translation layer with a drift of its own.
 *
 * AUTHENTICATION is rendered from the declaration, since
 * specs/route-headers.md stage B: a route that says what it requires
 * gets a `security` requirement on its operation and a
 * `securityScheme` in `components`, and the very same vector is what
 * `Router.enforcing` refuses on. Before that, protection lived in a
 * wrapper around the finished table and the document showed an open
 * door where there was a lock.
 *
 * Every gap this comment used to list is closed. `Router.Entry`
 * carries `Route.Described` — the template, its parameters, its query
 * AND the headers it declares — so a path parameter is rendered with
 * the KIND its `Param` declared, query parameters are rendered at all
 * (openapi-parameters), a declared request header is rendered `in:
 * header` (route-headers stage A), and an operation says what it
 * ANSWERS when the handler's type said so (openapi-responses) — in
 * whatever media, not only JSON (openapi-media).
 *
 * The one thing here that is not derived is the SUMMARY: a sentence
 * about what an operation is for cannot be computed from a path, a
 * parameter or a type, so `Router.summarised` is where an author
 * writes it and this renders it (openapi-prose). Absent stays absent;
 * an operation id derived from method and path is what identifies an
 * operation nobody described.
 */
object OpenApi:

  /** the document for a router */
  def document(api: Api, router: Router): Json =
    val byPath = router.entries.groupBy(_.path)
    // paths in the order the router declared them, each method in its
    // own order: a diff of two documents should read as a diff of the
    // code, not of a hash map
    val order = router.entries.map(_.path).distinct
    JObj(Vector(
      "openapi" -> JStr("3.1.0"),
      "info" -> JObj(Vector(
        "title" -> JStr(api.title),
        "version" -> JStr(api.version)) ++
        api.description.map(d => "description" -> JStr(d)).toVector),
    ) ++ serversField(api) ++ Vector(
      "paths" -> JObj(order.map(p => p -> pathItem(byPath(p))))) ++
      componentsField(router))

  /**
   * The schemes the ROUTES declare, collected — not a list the author
   * keeps beside them (specs/route-headers.md, stage B).
   *
   * OpenAPI models authentication as `securitySchemes` plus a
   * `security` requirement per operation, NOT as a header parameter,
   * and the difference is not cosmetic: a generated client does a
   * bearer flow for one and puts a literal string in a box for the
   * other. So `Authorization` is never rendered as a parameter, even
   * though that is where it travels.
   */
  private def componentsField(router: Router): Vector[(String, Json)] =
    val schemes = router.entries.flatMap(_.security).map(_.scheme).distinct.sorted
    if schemes.isEmpty then Vector.empty
    else Vector("components" -> JObj(Vector(
      "securitySchemes" -> JObj(schemes.map(n =>
        n -> JObj(Vector("type" -> JStr("http"), "scheme" -> JStr(n))))))))

  /** what THIS operation requires, by scheme, with the scopes it asks
   * for — the same vector `Router.enforcing` refuses on */
  private def securityOf(e: Router.Entry): Option[(String, Json)] =
    if e.security.isEmpty then None
    else Some("security" -> JArr(e.security.map(sec =>
      JObj(Vector(sec.scheme -> JArr(sec.scopes.toVector.sorted.map(JStr(_))))))))

  private def serversField(api: Api): Vector[(String, Json)] =
    if api.servers.isEmpty then Vector.empty
    else Vector("servers" -> JArr(api.servers.map(u => JObj(Vector("url" -> JStr(u))))))

  private def pathItem(entries: Vector[Router.Entry]): Json =
    JObj(entries.map(e => e.method.name.toLowerCase -> operation(e)))

  private def operation(e: Router.Entry): Json =
    JObj(Vector("operationId" -> JStr(operationId(e))) ++
      // the author's sentence, when there is one. It comes FIRST after
      // the id because that is the order a reader wants it in, and it
      // is absent rather than empty when nobody wrote one: an empty
      // summary would be a promise of prose that is not there
      e.summary.map(t => "summary" -> JStr(t)).toVector ++
      parameters(e).toVector ++
      e.body.map(b => "requestBody" -> requestBody(b)).toVector ++
      securityOf(e).toVector ++
      Vector("responses" -> responses(e)))

  /** `GET /board/{id}` -> `getBoardById`-ish, and stable: a document
   * whose ids move when nothing moved is a document nobody diffs */
  private def operationId(e: Router.Entry): String =
    val parts = e.path.split('/').filter(_.nonEmpty).map { seg =>
      if seg.startsWith("{") then "By" + capitalise(seg.drop(1).dropRight(1))
      else capitalise(seg)
    }
    e.method.name.toLowerCase + parts.mkString

  private def capitalise(s: String): String =
    if s.isEmpty then s else s.head.toUpper.toString + s.tail

  /**
   * The route's own parameters, path then query, in declaration
   * order — not `{name}` re-parsed out of the template.
   *
   * The difference is the whole of `openapi-parameters`: the template
   * is a string and a string cannot say that `id` is an integer. The
   * entry carries `Route.Seg.Var` and `Route.Q`, each with the JSON
   * Schema its `Param` declared, so what the document says a caller
   * may send is what the route actually parses.
   *
   * A path parameter is always `required` — a template with a hole
   * does not match without it. A query parameter says what its
   * declaration said: `:?` is required, `opt` is not, and `all` is a
   * repeated one whose schema is an array. A header says the same, and
   * is rendered `in: header` — it is BESIDE the template rather than
   * in it, because a header is not part of a url.
   */
  private def parameters(e: Router.Entry): Option[(String, Json)] =
    val path = e.params.map(v => JObj(Vector(
      "name" -> JStr(v.name),
      "in" -> JStr("path"),
      "required" -> JBool(true),
      "schema" -> v.schema)))
    val query = e.queries.map(q => JObj(Vector(
      "name" -> JStr(q.name),
      "in" -> JStr("query"),
      "required" -> JBool(q.required),
      "schema" -> q.schema)))
    val header = e.headers.map(h => JObj(Vector(
      "name" -> JStr(h.name),
      "in" -> JStr("header"),
      "required" -> JBool(h.required),
      "schema" -> h.schema)))
    val all = path ++ query ++ header
    if all.isEmpty then None else Some("parameters" -> JArr(all))

  /** the schema the DECODER was derived from, not a second one */
  private def requestBody(schema: Json): Json =
    JObj(Vector(
      "required" -> JBool(true),
      "content" -> JObj(Vector(
        "application/json" -> JObj(Vector("schema" -> schema))))))

  /**
   * What the operation answers, when it said so.
   *
   * A handler that answers a VALUE (`Router.out`, `jsonOut`) declares
   * by its own type, and the router adds the failures it produces
   * itself — `jsonOut`'s 400 for a body that does not parse. A handler
   * that builds its own `Response` declares nothing, and this says
   * exactly that rather than inventing a 200 nobody promised.
   *
   * The media type is the entry's too (`Router.html`, `bytes`,
   * `events`, `media`): the same value the router wrote into the
   * content-type header, so what a document files an answer under is
   * what a client will actually receive.
   */
  private def responses(e: Router.Entry): Json =
    if e.answers.isEmpty then
      JObj(Vector("default" -> JObj(Vector(
        "description" -> JStr("undeclared — this operation builds its own Response")))))
    else
      // IF NOTHING SAYS WHAT SUCCESS LOOKS LIKE, SAY SO. A secured
      // route declares 401 and 403 without the author writing them, so
      // `answers` is non-empty while the 200 is still undeclared —
      // and a document listing only the failures reads as an
      // operation that cannot succeed. Found by the first real
      // document (demo-admin-declared), not by a synthetic test.
      val undeclared =
        if e.answers.exists(a => a.status >= 200 && a.status < 300) then Vector.empty
        else Vector("default" -> JObj(Vector(
          "description" -> JStr("undeclared — this operation builds its own Response"))))
      JObj(e.answers.sortBy(_.status).map(a =>
        a.status.toString -> JObj(
          Vector("description" -> JStr(a.description)) ++
          answerHeaders(a) ++
          // an answer with NO media has no body at all, and leaving
          // `content` out is how OpenAPI says that; an empty object
          // under a media type would describe a body nobody sends
          (if a.media.isEmpty then Vector.empty
           else Vector("content" -> JObj(Vector(
             a.media -> JObj(a.schema.map(sch => "schema" -> sch).toVector))))))) ++
        undeclared)

  /** what an answer carries beside its body (specs/route-headers.md,
   * stage C). A secured route's `WWW-Authenticate` is here because the
   * router writes it from the same value; anything else is what the
   * author said they send. */
  private def answerHeaders(a: Router.Answer): Vector[(String, Json)] =
    if a.headers.isEmpty then Vector.empty
    else Vector("headers" -> JObj(a.headers.map(h =>
      h.name -> JObj(Vector(
        "description" -> JStr(if h.required then "always sent" else "sent when it applies"),
        "schema" -> h.schema)))))

  /** the document as text, for a file or a handler */
  def print(api: Api, router: Router): String = Json.print(document(api, router))

  /**
   * The two routes that serve it: the document, and a page for a
   * person (specs/openapi.md stage 2).
   *
   * The page is RENDERED ON THE SERVER — no JavaScript and no CDN.
   * The usual `<script src="…swagger-ui…">` makes a page that only
   * works where the network does, and this repository renders
   * deployments for machines that have none; a page that cannot
   * explain the API in an air-gapped cluster is not documentation
   * there. The cost is that it does not have a "try it" button, which
   * a curl command in the page covers.
   *
   * Serve it beside the router it describes, the way every surface in
   * this stack is joined:
   * {{{
   *   val all = app.routes orElse OpenApi.routes(api, app).routes
   * }}}
   * The document then describes the APPLICATION and not itself: the
   * two extra routes are not in `router`, which is the right default —
   * pass the joined router instead if you want them in.
   */
  def routes(api: Api, router: Router,
             at: String = "/openapi.json", ui: String = "/openapi"): Router =
    val doc = document(api, router)
    Router.empty
      .on(Method.Get, literal(at))(_ => pure(json(Json.print(doc))))
      .on(Method.Get, literal(ui))(_ => pure(html(page(api, router, at))))

  /** a path of literal segments, as a route */
  private def literal(path: String): Route[EmptyTuple] =
    path.split('/').filter(_.nonEmpty).foldLeft(Route.root)((r, seg) => r / seg)

  private def json(text: String): Response =
    Response(200, Seq("content-type" -> "application/json"),
      okay.http.Http.one(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  private def html(text: String): Response =
    Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
      okay.http.Http.one(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  /** the page a person reads: every operation, its parameters, its
   * body and what it answers — as HTML, computed here */
  def page(api: Api, router: Router, at: String = "/openapi.json"): String =
    val ops = router.entries.map(operationHtml)
    s"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>${esc(api.title)} ${esc(api.version)}</title>
<style>
 body{font:15px/1.5 system-ui,sans-serif;margin:2rem auto;max-width:52rem;padding:0 1rem}
 h1{margin-bottom:0} .sub{color:#666;margin-top:.2rem}
 .op{border:1px solid #ddd;border-radius:.4rem;margin:1rem 0;padding:.8rem 1rem}
 .m{font-weight:700;font-family:ui-monospace,monospace}
 .p{font-family:ui-monospace,monospace}
 table{border-collapse:collapse;margin:.5rem 0;width:100%}
 td,th{border-bottom:1px solid #eee;text-align:left;padding:.25rem .5rem;font-size:.9em}
 pre{background:#f6f6f6;padding:.5rem;border-radius:.3rem;overflow:auto;font-size:.85em}
 .none{color:#999}
 .says{margin:.3rem 0 .4rem;font-size:.95em}
</style></head><body>
<h1>${esc(api.title)}</h1>
<div class="sub">version ${esc(api.version)}${api.servers.headOption.fold("")(u => " &middot; " + esc(u))}</div>
<p class="sub">This page is rendered by the service itself from the router that answers these
paths, so it cannot describe a route that is not served. The machine-readable document is
at <a href="${esc(at)}">${esc(at)}</a>.</p>
${ops.mkString("\n")}
</body></html>"""

  private def operationHtml(e: Router.Entry): String =
    val params = e.path.split('/').filter(s => s.startsWith("{") && s.endsWith("}"))
      .map(_.drop(1).dropRight(1)).toVector
    val paramRows =
      if params.isEmpty then ""
      else "<table><tr><th>path parameter</th><th>type</th></tr>" +
        params.map(n => s"<tr><td>${esc(n)}</td><td>string</td></tr>").mkString + "</table>"
    val bodyBlock = e.body.fold("")(b =>
      s"<div>request body</div><pre>${esc(Json.print(b))}</pre>")
    val answers =
      if e.answers.isEmpty then
        """<div class="none">answers: undeclared — this handler builds its own Response</div>"""
      else e.answers.sortBy(_.status).map(a =>
        // the media type is on the page too: a reader deciding whether
        // to point a JSON client at an operation should not have to
        // open the document to find out that it answers a stream
        s"<div>answers <b>${a.status}</b> <code>${esc(a.media)}</code> — ${esc(a.description)}</div>" +
          a.schema.fold("")(sch => s"<pre>${esc(Json.print(sch))}</pre>")).mkString
    val says = e.summary.fold("")(t => s"""<div class="says">${esc(t)}</div>""")
    s"""<div class="op"><span class="m">${e.method.name}</span> <span class="p">${esc(e.path)}</span>
$says$paramRows$bodyBlock$answers</div>"""

  private def esc(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
