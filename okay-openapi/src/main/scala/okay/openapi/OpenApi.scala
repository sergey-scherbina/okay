package okay.openapi

import okay.codec.Json
import okay.codec.Json.*
import okay.http.Router

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
 * WHAT THIS STAGE CANNOT SAY, stated rather than faked: headers. A
 * route declares a path, its parameters, its query and its body, and
 * a handler that reads a header reads it from the `Request` with
 * nothing declared — so this renderer says nothing about headers
 * rather than guessing at them.
 *
 * The three gaps this comment used to list are closed.
 * `Router.Entry` now carries `Route.Described` — the template AND its
 * parameters — so a path parameter is rendered with the KIND its
 * `Param` declared and query parameters are rendered at all
 * (openapi-parameters); and an operation says what it ANSWERS when
 * the handler's type said so (openapi-responses).
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
      "paths" -> JObj(order.map(p => p -> pathItem(byPath(p))))))

  private def serversField(api: Api): Vector[(String, Json)] =
    if api.servers.isEmpty then Vector.empty
    else Vector("servers" -> JArr(api.servers.map(u => JObj(Vector("url" -> JStr(u))))))

  private def pathItem(entries: Vector[Router.Entry]): Json =
    JObj(entries.map(e => e.method.name.toLowerCase -> operation(e)))

  private def operation(e: Router.Entry): Json =
    JObj(Vector("operationId" -> JStr(operationId(e))) ++
      parameters(e).toVector ++
      e.body.map(b => "requestBody" -> requestBody(b)).toVector ++
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
   * repeated one whose schema is an array.
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
    val all = path ++ query
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
   */
  private def responses(e: Router.Entry): Json =
    if e.answers.isEmpty then
      JObj(Vector("default" -> JObj(Vector(
        "description" -> JStr("undeclared — this operation builds its own Response")))))
    else JObj(e.answers.sortBy(_.status).map(a =>
      a.status.toString -> JObj(
        Vector("description" -> JStr(a.description)) ++
        a.schema.map(sch => "content" -> JObj(Vector(
          "application/json" -> JObj(Vector("schema" -> sch))))).toVector)))

  /** the document as text, for a file or a handler */
  def print(api: Api, router: Router): String = Json.print(document(api, router))
