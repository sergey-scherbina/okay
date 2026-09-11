package okay.openapi

import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Method, Router}

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
 * WHAT THIS STAGE CANNOT SAY, stated rather than faked. `Router.Entry`
 * carries the method, the path TEMPLATE and the request body's
 * schema, and that is all it carries: the path parameters' KINDS live
 * on `Route.params` and never reach the entry, so every path
 * parameter is declared a string here; query parameters are not on
 * the entry either; and nothing in the tree declares what an
 * operation ANSWERS. The first two are small; the third decides
 * whether a document is worth publishing, and all three are
 * declarations in okay-http (BACKLOG "openapi").
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
      parameters(e.path).toVector ++
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

  /** the `{name}` segments of the template. Their KIND is not on the
   * entry (see the class comment), so they are strings until okay-http
   * carries it */
  private def parameters(path: String): Option[(String, Json)] =
    val names = path.split('/').filter(s => s.startsWith("{") && s.endsWith("}"))
      .map(_.drop(1).dropRight(1)).toVector
    if names.isEmpty then None
    else Some("parameters" -> JArr(names.map(n => JObj(Vector(
      "name" -> JStr(n),
      "in" -> JStr("path"),
      "required" -> JBool(true),
      "schema" -> JObj(Vector("type" -> JStr("string"))))))))

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
