package okay.script.api

/** The page API -- what a `.md` page sees of the request it answers,
 * the response it shapes, and the container around it. See
 * specs/okay-script.md "Site — the container".
 *
 * THIS PACKAGE IS SHARED between the host and every script
 * classloader: `okay.script.ScalaScript`'s loader delegates
 * `okay.script.api.*` -- and only it -- to the classloader that
 * loaded okay-script itself, the way a servlet container shares the
 * servlet API with an otherwise-isolated webapp. So a `Response` the
 * container creates IS the `Response` the page writes into: one
 * class identity on both sides, no encoding across the boundary.
 * Everything in `okay.script` proper stays isolated (a script gets its
 * own copy of `Meta`, set from inside the script by synthesized code).
 *
 * Per-request state lives in a `ThreadLocal`: a server answers many
 * requests at once on many threads, the container sets `Web`/
 * `Response`/`Session` on the request's thread before invoking the
 * page, and the page reads them on that same thread. `include` runs
 * the included page on the same thread and so sees the same three --
 * which is what `<jsp:include>` means.
 */
final case class Web(
  method: String,
  path: String,
  query: Map[String, String] = Map.empty,
  headers: Map[String, String] = Map.empty,
  form: Map[String, String] = Map.empty,
  cookies: Map[String, String] = Map.empty,
  body: String = "",
  params: Map[String, String] = Map.empty,
  /** a multipart/form-data request's parts, in wire order; empty
   * otherwise -- its non-file fields are ALSO in `form` */
  parts: Vector[Part] = Vector.empty,
):
  /** a header by name, case-insensitively -- `headers` keeps the
   * wire's own spelling */
  def header(name: String): Option[String] =
    headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  /** the first uploaded FILE under that field name */
  def file(name: String): Option[Part] = parts.find(p => p.name == name && p.isFile)

  /** a query or form field, whichever carries it (query wins) */
  def param(name: String): Option[String] =
    query.get(name).orElse(form.get(name)).orElse(params.get(name))

/** One part of a multipart/form-data body -- a field or an uploaded
 * file (`filename` present). See specs/okay-script.md "Uploads". */
final case class Part(name: String, filename: Option[String], contentType: Option[String], bytes: Array[Byte]):
  def text: String = new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
  def isFile: Boolean = filename.isDefined

object Web:
  val empty: Web = Web("GET", "/")

  private val local: ThreadLocal[Web] = ThreadLocal.withInitial(() => empty)

  def current: Web = local.get()

  def setCurrent(w: Web): Unit = local.set(w)

/** The response a page shapes while its stdout becomes the body.
 * Output is buffered (the page's whole stdout is captured before
 * anything is written), so status and headers can be set at any
 * point -- "headers already sent" cannot happen.
 */
final class Response:
  var status: Int = 200
  private val hs = scala.collection.mutable.ArrayBuffer.empty[(String, String)]
  private var redirectTo: Option[String] = None

  def headers: Vector[(String, String)] = hs.toVector

  /** adds a header (a second call with the same name adds a second
   * header -- `Set-Cookie` needs that) */
  def header(name: String, value: String): Unit = hs += (name -> value): Unit

  /** replaces the content type */
  def contentType(ct: String): Unit =
    hs.filterInPlace((k, _) => !k.equalsIgnoreCase("content-type"))
    hs += ("Content-Type" -> ct): Unit

  def contentTypeValue: Option[String] =
    hs.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }

  /** answers `status` (302 by default) with `Location`; the page's
   * output is discarded */
  def redirect(to: String, status: Int = 302): Unit =
    this.status = status
    redirectTo = Some(to)
    hs.filterInPlace((k, _) => !k.equalsIgnoreCase("location"))
    hs += ("Location" -> to): Unit

  def redirected: Option[String] = redirectTo

  /** one `Set-Cookie`; `maxAge` in seconds, `Some(0)` expires it */
  def cookie(name: String, value: String, maxAge: Option[Int] = None,
             path: String = "/", httpOnly: Boolean = false): Unit =
    val parts = Vector(s"$name=$value", s"Path=$path") ++
      maxAge.map(a => s"Max-Age=$a").toVector ++
      (if httpOnly then Vector("HttpOnly") else Vector.empty)
    header("Set-Cookie", parts.mkString("; "))

object Response:
  private val local: ThreadLocal[Response] = ThreadLocal.withInitial(() => new Response)

  def current: Response = local.get()

  def setCurrent(r: Response): Unit = local.set(r)

/** A session -- attributes that survive across requests from one
 * client. Created lazily on the first `set`: a request that never
 * touches its session creates none and gets no cookie.
 */
trait Session:
  def id: String
  def get(key: String): Option[String]
  def set(key: String, value: String): Unit
  def remove(key: String): Unit
  def attributes: Map[String, String]
  /** drops the session; the container expires the cookie */
  def invalidate(): Unit

object Session:
  /** outside a `Site` (a bare `render`) the session is a throwaway
   * per-thread map -- pages that read it still run */
  def detached: Session = new Session:
    private val m = scala.collection.mutable.LinkedHashMap.empty[String, String]
    def id: String = "detached"
    def get(key: String): Option[String] = m.get(key)
    def set(key: String, value: String): Unit = m(key) = value
    def remove(key: String): Unit = m.remove(key): Unit
    def attributes: Map[String, String] = m.toMap
    def invalidate(): Unit = m.clear()

  private val local: ThreadLocal[Session] = ThreadLocal.withInitial(() => detached)

  def current: Session = local.get()

  def setCurrent(s: Session): Unit = local.set(s)

/** What `forward` throws -- a control exception the container
 * dispatches on. Stackless: it is not an error. */
final case class Forwarded(path: String)
  extends RuntimeException(s"forward to $path", null, false, false)

/** What an error page reads: why the original page failed. */
final case class Error(message: String, errors: Vector[String], thrown: Option[Throwable])

object Error:
  private val local: ThreadLocal[Option[Error]] = ThreadLocal.withInitial(() => None)

  def current: Option[Error] = local.get()

  def setCurrent(e: Option[Error]): Unit = local.set(e)

/** The container's hooks, set by `okay.script.Site` on the request
 * thread. A page never touches this directly -- it calls `include`/
 * `forward` below. */
object Container:
  private val local: ThreadLocal[Option[String => String]] = ThreadLocal.withInitial(() => None)

  def includer: Option[String => String] = local.get()

  def setIncluder(f: Option[String => String]): Unit = local.set(f)

/** Renders another page -- relative to the including page's
 * directory, or from the site root with a leading `/` -- with the same
 * `Web`/`Response`/`Session`, and prints its output here, at the point
 * of the call. `<jsp:include>`. */
def include(page: String): Unit =
  Container.includer match
    case Some(f) => System.out.print(f(page))
    case None => throw new IllegalStateException(s"include(\"$page\"): no Site is serving this page")

/** Abandons this page's output and answers with the target page
 * instead. `<jsp:forward>`. */
def forward(path: String): Nothing = throw Forwarded(path)
