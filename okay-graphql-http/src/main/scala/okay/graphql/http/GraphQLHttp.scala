package okay.graphql.http

import okay.Async
import okay.codec.Json
import okay.codec.Json.*
import okay.graphql.*
import okay.http.{Http, Method, Request, Response, Route, Router}
import okay.one.!
import okay.one.Cont.pure
import okay.one.row.*
import java.nio.charset.StandardCharsets.UTF_8

/** GRAPHQL OVER HTTP (the GraphQL-over-HTTP draft): `POST` with `application/json` `{query, variables,
 * operationName}`, `GET` with the same as query parameters — a mutation over `GET` refused (405). The response is
 * JSON: `application/json` with 200 for any response it could execute or refuse (the legacy, every client reads
 * it), or `application/graphql-response+json` when the client accepts it — then a request refused before execution
 * is a 400. A body that is not JSON or names no query is a 400 either way. Executed by `executeAsync`, so async
 * sources work; the route is okay-http's, on every backend it has */
object GraphQLHttp:
  private val GraphQLResponse = "application/graphql-response+json"

  /** `POST` and `GET` of `path` answered by `schema` */
  def router(schema: Schema, path: String = "graphql", limits: Limits = Limits())(using GraphQLParser): Router =
    Router.empty
      .at(Method.Post, Route / path)((_, r) => okay.Program.fromKernel(answer(schema, r, limits)))
      .at(Method.Get, Route / path)((_, r) => okay.Program.fromKernel(answer(schema, r, limits)))

  /** a request answered: what the route runs, for a server of another shape too */
  def answer(schema: Schema, r: Request, limits: Limits = Limits())(using GraphQLParser)
      : Response ! (Async +: Pure) =
    val graphQLResponse = r.headers.exists((k, v) => k.equalsIgnoreCase("accept") && v.contains(GraphQLResponse))
    def respond(status: Int, j: Json, extra: Seq[(String, String)] = Nil): Response =
      val ct = if graphQLResponse then GraphQLResponse else "application/json"
      Response(status, (("content-type", s"$ct; charset=utf-8") +: extra),
        Http.one(Json.print(j).getBytes(UTF_8)))
    def refused(status: Int, message: String, extra: Seq[(String, String)] = Nil): Response ! (Async +: Pure) =
      pure(respond(status, JObj(Vector("errors" -> JArr(Vector(JObj(Vector("message" -> JStr(message))))))), extra))
    params(r) match
      case Left(why) => refused(400, why)
      case Right((query, variables, operationName)) =>
        Executor.prepare(schema, query, operationName, limits) match
          case Left(errors) => pure(respond(if graphQLResponse then 400 else 200, errors))
          case Right(p) if r.method == Method.Get && p.operation.kind != OperationKind.Query =>
            refused(405, s"a ${p.operation.kind.toString.toLowerCase} cannot be sent by GET: use POST", Seq("allow" -> "POST"))
          case Right(p) if p.operation.kind == OperationKind.Subscription =>
            refused(400, "a subscription is not served over HTTP here: use the WebSocket (graphql-transport-ws)")
          case Right(p) => p.executeAsync(variables).map(respond(200, _))

  /** the query, the variables, the operation name: from the JSON body of a POST, from the query string of a GET */
  private def params(r: Request): Either[String, (String, Json, Option[String])] =
    val fields: Either[String, Map[String, Json]] = r.method match
      case Method.Get =>
        val qs = r.url.indexOf('?') match
          case -1 => ""
          case i => r.url.substring(i + 1)
        val ps = qs.split('&').iterator.filter(_.nonEmpty).map { kv =>
          kv.indexOf('=') match
            case -1 => decode(kv) -> ""
            case i => decode(kv.substring(0, i)) -> decode(kv.substring(i + 1))
        }.toMap
        val vars = ps.get("variables").filter(_.nonEmpty).map(Json.parse)
        if vars.exists(!isObject(_)) then Left("variables must be a JSON object")
        else Right(ps.view.filterKeys(k => k == "query" || k == "operationName").mapValues(JStr(_)).toMap ++
          vars.map("variables" -> _))
      case _ =>
        val ct = r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }.getOrElse("")
        if !ct.toLowerCase.startsWith("application/json") then Left(s"the body must be application/json, not '$ct'")
        else Json.parse(String(r.body.bytes, UTF_8)) match
          case JObj(fs) => Right(fs.toMap)
          case _ => Left("the body must be a JSON object")
    fields.flatMap { m =>
      m.get("query") match
        case Some(JStr(q)) =>
          val vars = m.getOrElse("variables", JObj(Vector.empty)) match
            case JNull => Right(JObj(Vector.empty))
            case o: JObj => Right(o)
            case _ => Left("variables must be a JSON object")
          val op = m.get("operationName") match
            case Some(JStr(n)) if n.nonEmpty => Right(Some(n))
            case None | Some(JNull) | Some(JStr(_)) => Right(None)
            case _ => Left("operationName must be a string")
          for v <- vars; o <- op yield (q, v, o)
        case _ => Left("the request names no query")
    }

  private def isObject(j: Json) = j match
    case _: JObj => true
    case _ => false

  /** a query string's percent-encoding read: `+` a space, `%XX` a byte, the bytes UTF-8 */
  private def decode(s: String): String =
    val out = java.io.ByteArrayOutputStream()
    var i = 0
    while i < s.length do
      s.charAt(i) match
        case '+' => out.write(' '); i += 1
        case '%' if i + 3 <= s.length =>
          val hex = s.substring(i + 1, i + 3)
          if hex.forall(c => Character.digit(c, 16) >= 0) then { out.write(Integer.parseInt(hex, 16)); i += 3 }
          else { out.write('%'); i += 1 }
        case c => out.write(String.valueOf(c).getBytes(UTF_8)); i += 1
    String(out.toByteArray, UTF_8)
