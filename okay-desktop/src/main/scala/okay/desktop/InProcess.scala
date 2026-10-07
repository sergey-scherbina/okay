package okay.desktop


import okay.{Async}
import okay.freer.*
import okay.std.*
import okay.given
import okay.freer.given
import okay.http.{Body, Http, Method, Request, Response}
import java.io.{ByteArrayInputStream, InputStream}
import java.net.{URL, URLConnection, URLStreamHandler}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/** a service's routes: what the window reaches, in its own process */
type Routes = PartialFunction[Request, Response ! Async]

/** an answer as the window takes it: the final status, headers and
 * bytes, and the URL they came from (after redirects) */
final case class Answer(status: Int, headers: Seq[(String, String)], body: Array[Byte], url: String):
  def header(name: String): Option[String] = headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }
  def contentType: String = header("content-type").getOrElse("application/octet-stream")
  def text: String = new String(body, UTF_8)

/**
 * THE WINDOW'S SERVICE, IN THE PROCESS (specs/app-in-process.md): no
 * socket and no port — a route table is a function, and this calls it.
 *
 * Reads come through the `app://` scheme (`InProcess.install`); writes
 * through the window's bridge (`Window.Bridge.send`), because the
 * embedded WebKit will not POST a form or `fetch` to a scheme of ours
 * (measured 2026-09-29, JavaFX 26). Both end here.
 */
object InProcess:
  val Scheme = "app"

  /** the base of a service's pages: `app://<host>` */
  def base(host: String): String = s"$Scheme://$host"

  /** a host name from an app's name: letters, digits and dashes */
  def host(name: String): String =
    name.toLowerCase.map(c => if c.isLetterOrDigit then c else '-').dropWhile(_ == '-').reverse.dropWhile(_ == '-').reverse match
      case "" => "app"
      case h => h

  /**
   * One service, reached in the process: `send` runs a request through
   * its routes and follows redirects as a browser does — 301, 302 and 303
   * become a GET, 307 and 308 keep the method — keeping the cookies the
   * answers set for the next request. A request carries peer `127.0.0.1`:
   * it came from this computer.
   */
  final class Server(val host: String, routes: Routes):
    val base: String = InProcess.base(host)
    private val cookies = ConcurrentHashMap[String, String]()
    private val held = AtomicReference[Option[(String, Answer)]](None)

    /** the path and query of a URL under this base; a path as it is */
    def target(url: String): String =
      if url.startsWith(base) then url.drop(base.length) match
        case "" => "/"
        case p if p.startsWith("/") => p
        case p => "/" + p
      else if url.startsWith("/") then url
      else "/" + url

    def send(method: String, url: String, headers: Seq[(String, String)] = Nil, body: Array[Byte] = Array.empty): Answer =
      go(method, target(url), headers, body, 10)

    /** at most `hops` redirects followed — the bound on this loop */
    @scala.annotation.tailrec
    private def go(method: String, path: String, headers: Seq[(String, String)], body: Array[Byte], hops: Int): Answer =
      val m = Method.values.find(_.name.equalsIgnoreCase(method)).getOrElse(Method.Get)
      val jar = cookies.entrySet.toArray.toVector.map(_.asInstanceOf[java.util.Map.Entry[String, String]])
        .map(e => s"${e.getKey}=${e.getValue}").mkString("; ")
      val r = Request(m, path, headers.filterNot(_._1.equalsIgnoreCase("cookie")) ++
        Option.when(jar.nonEmpty)("cookie" -> jar).toSeq,
        if body.isEmpty then Body.Empty else Body.Bytes(scala.collection.immutable.ArraySeq.unsafeWrapArray(body)),
        peer = Some("127.0.0.1"))
      val a =
        if !routes.isDefinedAt(r) then Answer(404, Seq("content-type" -> "text/plain; charset=utf-8"),
          s"nothing answers $path".getBytes(UTF_8), base + path)
        else scala.util.Try {
          val res = Async.run[Response, Pure](routes(r)).runWith
          val bytes = Async.run[Chunk[Byte], Pure](Http.bytes(res)).runWith.toArray
          Answer(res.status, res.headers, bytes, base + path)
        }.recover { case e => Answer(500, Seq("content-type" -> "text/plain; charset=utf-8"),
          s"the service failed: ${e.getClass.getSimpleName}: ${e.getMessage}".getBytes(UTF_8), base + path) }.get
      a.headers.foreach { case (k, v) if k.equalsIgnoreCase("set-cookie") => keep(v); case _ => () }
      (a.status, a.header("location")) match
        case (s, Some(to)) if hops > 0 && Set(301, 302, 303, 307, 308)(s) =>
          val next = resolve(path, to)
          if next.startsWith("/") then
            if s == 307 || s == 308 then go(method, next, headers, body, hops - 1)
            else go("GET", next, headers.filterNot(_._1.equalsIgnoreCase("content-type")), Array.empty, hops - 1)
          else a.copy(url = next) // somewhere else: the window's outside-link rule takes it
        case _ => a

    /** a Location against the request's path: absolute path, our own base, or elsewhere */
    private def resolve(from: String, to: String): String =
      if to.startsWith("/") then to
      else if to.startsWith(base) then target(to)
      else if to.contains("://") then to
      else from.takeWhile(_ != '?').reverse.dropWhile(_ != '/').reverse + to

    /** a Set-Cookie kept: its name and value; `Max-Age=0` forgets it */
    private def keep(setCookie: String): Unit =
      val parts = setCookie.split(";").map(_.trim)
      parts.headOption.flatMap(nv => nv.split("=", 2) match { case Array(n, v) => Some(n.trim -> v.trim); case _ => None }).foreach { (n, v) =>
        if parts.exists(p => p.equalsIgnoreCase("max-age=0")) || v.isEmpty then cookies.remove(n): Unit
        else cookies.put(n, v): Unit
      }

    /**
     * THE HELD ANSWER: the next GET of exactly `url` is answered with it,
     * once. A POST that lands on another page becomes a navigation there,
     * answered with what the POST already got — not asked twice.
     */
    def hold(url: String, a: Answer): Unit = held.set(Some(norm(url) -> a))
    def take(url: String): Option[Answer] =
      val u = norm(url)
      // the very value read is the one swapped out: compareAndSet compares references
      val now = held.get
      now match
        case Some((k, a)) if k == u && held.compareAndSet(now, None) => Some(a)
        case _ => None
    private def norm(url: String): String = base + target(url)

  // ---- the scheme -------------------------------------------------------

  private val servers = ConcurrentHashMap[String, Server]()
  @volatile private var installed = false

  /**
   * THE `app://` SCHEME, in this JVM: a URL whose host is a server's is
   * answered by it. The JVM takes a URL handler factory once, so the
   * factory is set on the first call and dispatches by host after.
   */
  def install(s: Server): Unit = synchronized {
    servers.put(s.host, s)
    if !installed then
      URL.setURLStreamHandlerFactory(p => if p == Scheme then Handler else null)
      installed = true
  }

  private object Handler extends URLStreamHandler:
    override def openConnection(u: URL): URLConnection = Conn(u)

  /** a page that goes on to `url` — how a redirect is shown, so the
   * document's own URL is always the page's */
  def goOn(url: String): String =
    val u = url.replace("&", "&amp;").replace("\"", "&quot;")
    val js = url.replace("\\", "\\\\").replace("'", "\\'")
    s"""<!doctype html><html><head><meta charset="utf-8"><meta http-equiv="refresh" content="0;url=$u">""" +
      s"""<script>location.replace('$js')</script></head><body></body></html>"""

  /** A REDIRECT ELSEWHERE — a checkout on the maker's site, a page on the
   * web — is not a page of ours: the embedded engine does not follow a
   * 303 through a URLConnection, and nothing would happen (okay-watch's
   * BUGS go-pro-does-nothing-in-window, 2026-09-29). So it is answered as
   * a page that asks the window to open the URL in the system browser
   * (`okayApp.external`) and steps back to the page it came from — never
   * a navigation the window would have to catch and undo. The bridge is
   * set on the page once it has loaded, so the page waits for it. */
  def goOutside(url: String): String =
    val js = url.replace("\\", "\\\\").replace("'", "\\'")
    s"""<!doctype html><html><head><meta charset="utf-8"><script>(function t(){""" +
      s"""if(window.okayApp&&window.okayApp.external){window.okayApp.external('$js');history.back();}""" +
      s"""else setTimeout(t,50);})()</script></head><body></body></html>"""

  private final class Conn(u: URL) extends URLConnection(u):
    private lazy val answer: Answer =
      val url = u.toString
      Option(servers.get(u.getHost)) match
        case None => Answer(404, Seq("content-type" -> "text/html; charset=utf-8"),
          s"<!doctype html><p>Nothing here answers ${u.getHost}.</p>".getBytes(UTF_8), url)
        case Some(s) =>
          val a = s.take(url).getOrElse(s.send("GET", url))
          if s.target(a.url) == s.target(url) then a
          else if !a.url.startsWith(s.base) then
            if Set(301, 302, 303, 307, 308)(a.status) then
              Answer(200, Seq("content-type" -> "text/html; charset=utf-8"), goOutside(a.url).getBytes(UTF_8), url)
            else a
          else
            // a redirect: its answer held for the page it went to, and a page that goes there
            s.hold(a.url, a)
            Answer(200, Seq("content-type" -> "text/html; charset=utf-8"), goOn(a.url).getBytes(UTF_8), url)
    override def connect(): Unit = ()
    override def getInputStream: InputStream = ByteArrayInputStream(answer.body)
    override def getContentType: String = answer.contentType
    override def getContentLengthLong: Long = answer.body.length.toLong
    override def getContentLength: Int = answer.body.length
    override def getHeaderField(name: String): String = answer.header(name).orNull
    override def getHeaderFields: java.util.Map[String, java.util.List[String]] =
      import scala.jdk.CollectionConverters.*
      answer.headers.groupBy(_._1.toLowerCase).map((k, vs) => k -> vs.map(_._2).asJava).asJava
