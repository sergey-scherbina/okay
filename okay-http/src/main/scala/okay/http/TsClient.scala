package okay.http

import okay.codec.{Schema, Stubs, TsTypes}

/**
 * A typed TypeScript client for an okay-http `Router` (typescript-types T5,
 * specs/typescript-types.md): the Scala backend's routes, called from a
 * TypeScript frontend as ordinary typed functions.
 *
 * `model(router)` is the declarations of every type a route takes or
 * answers (`Stubs.typescript`, the JSON codec's shape — what the router
 * really sends); `client(router)` is one async function per route:
 *
 * {{{
 * export async function getTasksById(o: ClientOptions, id: number): Promise<Task>
 * export async function postTasks(o: ClientOptions, body: NewTask): Promise<Task>
 * }}}
 *
 * The path's parameters, the query and the body are the function's
 * parameters, typed from the route's own declaration; the answer's type is
 * the Schema the router ENCODES the answer with, so the client cannot
 * promise what the server does not send. A route answering something that
 * is not JSON (a page, a stream, bytes) is a function answering the
 * `Response`. A non-2xx answer throws `OkayHttpError` with its status and
 * body. A function's name comes from the method and the path
 * (`GET /tasks/{id}` is `getTasksById`); a clash gets a number.
 */
object TsClient:

  /** the declarations of every type the router's routes take or answer */
  def model(router: Router): String = Stubs.typescript(schemas(router)*)

  private def schemas(router: Router): Vector[Schema[?]] =
    router.entries.flatMap(e => e.bodyType.toVector ++ e.answers.flatMap(_.tpe))

  private def cap(s: String): String = if s.isEmpty then s else s.head.toUpper.toString + s.tail

  private def ident(s: String): String =
    val parts = s.split("[^A-Za-z0-9]+").filter(_.nonEmpty)
    if parts.isEmpty then "_" else (parts.head +: parts.tail.map(cap)).mkString

  private val tsKeywords = Set("break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function", "if", "import",
    "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof",
    "var", "void", "while", "with", "let", "static", "yield", "await")

  private def param(n: String): String = { val i = ident(n); if tsKeywords(i) then s"${i}_" else i }

  private def kindType(kind: String): String = kind match
    case "int" | "long" => "number"
    case "boolean" => "boolean"
    case _ => "string"

  /** `GET /tasks/{id}` -> `getTasksById` */
  private def nameOf(e: Router.Entry): String =
    val segs = e.described.path.split("/").toVector.filter(_.nonEmpty).map { seg =>
      if seg.startsWith("{") && seg.endsWith("}") then "By" + cap(ident(seg.drop(1).dropRight(1)))
      else cap(ident(seg))
    }
    e.method.toString.toLowerCase + (if segs.isEmpty then "Root" else segs.mkString)

  /** `/tasks/{id}` -> `/tasks/${encodeURIComponent(String(id))}`; a scan,
   * not a look-behind regex, which Scala.js refuses below ES2018 */
  private def templated(path: String): String =
    val out = StringBuilder()
    var i = 0
    while i < path.length do
      val open = path.indexOf('{', i)
      val close = if open < 0 then -1 else path.indexOf('}', open)
      if close < 0 then
        out ++= path.substring(i)
        i = path.length
      else
        out ++= path.substring(i, open)
        out ++= "${encodeURIComponent(String(" + param(path.substring(open + 1, close)) + "))}"
        i = close + 1
    out.toString

  /** the client's source; its types are imported from `modelPath` */
  def client(router: Router, modelPath: String = "./model.ts"): String =
    val names = TsTypes.parse(model(router)).toOption.getOrElse(Vector.empty).map {
      case TsTypes.Decl.Interface(n, _) => n
      case TsTypes.Decl.Alias(n, _) => n
    }
    val used = scala.collection.mutable.Map.empty[String, Int]
    val fns = router.entries.map { e =>
      val base = nameOf(e)
      val k = used.updateWith(base)(n => Some(n.fold(1)(_ + 1))).getOrElse(1)
      val name = if k == 1 then base else s"$base$k"
      val pathParams = e.described.params.map(v => (param(v.name), kindType(v.kind), v.name))
      val queries = e.described.queries
      val bodyT = e.bodyType.map(Stubs.typescriptType)
      val ok = e.answers.find(a => a.status >= 200 && a.status < 300)
      val answerT = ok.flatMap(_.tpe).map(Stubs.typescriptType)
      val args = Vector("o: ClientOptions") ++
        pathParams.map((p, t, _) => s"$p: $t") ++
        bodyT.map(t => s"body: $t").toVector ++
        (if queries.isEmpty then Vector.empty
         else
           val fields = queries.map { q =>
             val t = kindType(q.kind)
             // the WIRE name, as the route declares it: quoted when it is no identifier
             val key = if q.name.matches("[A-Za-z_$][A-Za-z0-9_$]*") then q.name else s"\"${q.name}\""
             s"$key${if q.required then "" else "?"}: ${if q.repeated then s"$t[]" else t}"
           }
           Vector(s"query${if queries.forall(!_.required) then "?" else ""}: { ${fields.mkString("; ")} }"))
      val path = "`" + templated(e.described.path) + "`"
      val q = if queries.isEmpty then "undefined" else "query"
      val b = if bodyT.isDefined then "body" else "undefined"
      val send = s"""send(o, "${e.method.toString.toUpperCase}", $path, $q, $b)"""
      val doc = e.summary.fold("")(s => s"/** $s */\n")
      answerT match
        case Some(t) =>
          s"""${doc}export async function $name(${args.mkString(", ")}): Promise<$t> {
             |  return (await (await $send).json()) as $t;
             |}""".stripMargin
        case None =>
          s"""${doc}export async function $name(${args.mkString(", ")}): Promise<Response> {
             |  return await $send;
             |}""".stripMargin
    }
    val header =
      s"""// Generated by okay.http.TsClient from the Scala routes: regenerate it, do not edit it.
         |${if names.isEmpty then "" else s"import type { ${names.mkString(", ")} } from \"$modelPath\";\n"}
         |/** a non-2xx answer: its status and its body, as the server sent them */
         |export class OkayHttpError extends Error {
         |  readonly status: number;
         |  readonly body: string;
         |  constructor(status: number, body: string) {
         |    super(`HTTP $${status}: $${body}`);
         |    this.name = "OkayHttpError";
         |    this.status = status;
         |    this.body = body;
         |  }
         |}
         |
         |/** where the server is; a fetch of your own (a test's, a proxy's) and headers */
         |export interface ClientOptions {
         |  base: string;
         |  fetch?: typeof fetch;
         |  headers?: Record<string, string>;
         |}
         |
         |async function send(o: ClientOptions, method: string, path: string,
         |                    query: Record<string, unknown> | undefined, body: unknown): Promise<Response> {
         |  const pairs: [string, string][] = [];
         |  for (const [k, v] of Object.entries(query ?? {})) {
         |    if (v === undefined) continue;
         |    for (const x of Array.isArray(v) ? v : [v]) pairs.push([k, String(x)]);
         |  }
         |  const url = o.base + path + (pairs.length ? "?" + new URLSearchParams(pairs).toString() : "");
         |  const headers: Record<string, string> = { ...(o.headers ?? {}) };
         |  if (body !== undefined) headers["content-type"] = "application/json";
         |  const r = await (o.fetch ?? fetch)(url, { method, headers, body: body === undefined ? undefined : JSON.stringify(body) });
         |  if (!r.ok) throw new OkayHttpError(r.status, await r.text());
         |  return r;
         |}
         |""".stripMargin
    header + fns.map("\n" + _ + "\n").mkString
