package okay.script

import okay.*
import okay.http.{Frame, Http, Request, Response as HttpResponse}
import okay.security.{Decision, Policy, Verified}
import okay.ui.{Event, Protocol}

import java.net.URLDecoder
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** The container -- a directory of `.md` pages served as a site.
 * `Jetty.serve(port)(site.routes)()` is the whole server. See
 * specs/okay-script.md "Site — the container" for routing, the
 * request/response/session model, include/forward, and error pages.
 */
final class Site(
  root: Path,
  classpath: Classpath = Classpath.ambient,
  val sessions: Sessions = Sessions(),
  tempRoot: Path = ScalaScript.defaultTempRoot,
  /** the deployment's token verifier -- what a page's `secure:`
   * front-matter is checked with (okay-script-secure); `None` and a
   * secure page is a 500, not an open door */
  verify: Option[String => Verified] = None,
  realm: String = "okay",
  /** `verify`'s pair: how the container mints a token for
   * `api.signIn(subject, scopes)` -- a `SessionIssuer.issue`, say */
  issue: Option[(String, Set[String]) => String] = None,
  /** JSP's application scope, shared by every page (okay-script-
   * application); `Application.persisted(store)` survives a restart */
  val application: api.Application = api.Application.memory(),
  /** the languages this site speaks, the first the default
   * (okay-script-i18n): `page.<lang>.md` variants and `i18n/<lang>.
   * yaml` messages are looked up for the request's language */
  val languages: Vector[String] = Vector("en"),
  /** `Secure` on the cookies this container sets
   * (okay-script-cookie-flags). `None` DECIDES per request: this
   * connection was TLS, or a trusted proxy said the client's was.
   * `Some(true)` forces it on (the honest setting behind a proxy
   * whose headers you do not want to trust), `Some(false)` off (a
   * plaintext dev box). */
  secureCookies: Option[Boolean] = None,
  /** trust `X-Forwarded-Proto` from whatever connected -- ONLY where
   * a proxy you control is the only thing that can (a private
   * network, a sidecar). A header is a claim: off by default, and
   * `secureCookies = Some(true)` is the setting that needs no
   * trust at all. */
  trustForwarded: Boolean = false,
  /** `Strict-Transport-Security: max-age=<seconds>` on SECURE
   * responses (script-https-default). Off by default and deliberately:
   * HSTS on a self-signed development host pins a browser against you
   * for as long as the max-age says, and no page can take it back. */
  hsts: Option[Int] = None,
  /** answer a request the container sees as INSECURE with a 301 to
   * the same URL on https (script-https-default) -- behind a proxy
   * that is the http-to-https redirect, and with the container's own
   * TLS it is what a plaintext port is for */
  httpsOnly: Boolean = false,
):
  import Site.*

  require(languages.nonEmpty, "a Site speaks at least one language")

  private val rootAbs = root.toAbsolutePath.normalize
  private val pages = new ConcurrentHashMap[Path, Page]
  private val fronts = new ConcurrentHashMap[Path, (FileTime, Map[String, String])]
  /** the Live apps pages have mounted, by (page file, id) -- what a
   * `?__live=<id>` WebSocket on that page's path runs */
  private val lives = new ConcurrentHashMap[(Path, String), api.Live[?]]
  /** the include stack on this thread, innermost first -- what a
   * relative `include` resolves against, and the depth cap */
  private val including: ThreadLocal[List[Path]] = ThreadLocal.withInitial(() => Nil)

  /** what a path resolves to under `root`, if anything */
  enum Hit:
    case PageFile(file: Path, params: Map[String, String])
    case Static(file: Path)

  /** defined exactly when the path resolves to a page or a static
   * file; anything else falls through to the server's own 404, or to
   * the next route a caller chains with `orElse` */
  def routes: PartialFunction[Request, HttpResponse ! Async] = {
    case r if pathOf(r.url) == api.Live.JsPath || Mobile.paths(pathOf(r.url)) || resolve(pathOf(r.url)).isDefined => pure(handle(r))
  }

  /** the WebSocket side: a Live app's session on the page's own path
   * plus `?__live=<id>` -- `Jetty.serve(port)(site.routes)(site.ws)`.
   * Event lines arrive as text frames, the tree and its patches leave
   * as text frames; a Close frame ends the session. A socket that
   * arrives before the page was ever rendered (a reconnect after a
   * restart) renders it once to register the app. */
  def ws: PartialFunction[Request, Stage[Frame, Frame, Unit]] = {
    case r if liveOf(r).isDefined && socketPermitted(r) =>
      val (app, id) = liveHit(r).get
      val cookie = cookiesOf(r).get(SessionCookie)
      // the session the cookie names, if it is live: a durable app reads
      // and writes its state there. Only a BOUND handle is passed on —
      // a stale cookie must not mint a session no browser will ever
      // hear of (a socket carries no Set-Cookie)
      val bound = cookie.map(c => sessions.handle(Some(c))).filter(_.id.nonEmpty)
      liveSession(app, cookie, bound, id)
  }

  private def liveOf(r: Request): Option[api.Live[?]] = liveHit(r).map(_._1)

  /** a secure page's socket is checked the way its request is, from
   * the socket's own headers and cookie; refused is simply undefined */
  private def socketPermitted(r: Request): Boolean =
    val split = splitUrl(r.url)
    val path = split.path
    val query = split.query
    resolve(path) match
      case Some(Hit.PageFile(f, params)) =>
        val web = webOf(r, path, query, params)
        val sess = sessions.handle(web.cookies.get(SessionCookie))
        access(f, web, sess) match
          case Access.Open | Access.Granted(_) => true
          case _ => false
      case _ => true

  private def liveHit(r: Request): Option[(api.Live[?], String)] =
    val split = splitUrl(r.url)
    val path = split.path
    val query = split.query
    parseQuery(query).get("__live").flatMap { id =>
      resolve(path) match
        case Some(Hit.PageFile(base, params)) =>
          // the socket's language picks the same variant its page had
          val f = localized(base, langOf(webOf(r, path, query, params)))
          Option(lives.get((f, id))).orElse {
            handle(Request.get(r.url, r.headers)): Unit // a render registers what the page mounts
            Option(lives.get((f, id)))
          }.map(app => (app, id))
        case _ => None
    }

  /** the server's side of a Live session (script-live-push): the
   * app's own events as the lines the browser would have sent, for
   * `Jetty.serve(port)(site.routes)(site.ws, site.push)`; the transport
   * merges them into the session's input beside the client's */
  def push: PartialFunction[Request, Source[Frame]] = {
    case r if liveOf(r).isDefined => pushOf(liveOf(r).get)
  }

  private def pushOf(app: api.Live[?]): Source[Frame] =
    val eventsToFrames: Stage[Event, Frame, Unit] =
      Stage.transduce(())((_, e) => Stage.tell[Event, Frame](Frame.Text(Protocol.eventLine(e))), _ => pure(()))
    through[Event, Frame, Async, Unit, Unit](app.push)(!.widen[Unit, Take % Event + Writer % Frame, Async](eventsToFrames))

  /** `key` is the session cookie, when the socket carries one: the
   * app resumes the state that key last reached (script-live-resume);
   * `bound` the live session it names, where a durable app keeps its
   * state (script-live-durable); `name` the mount id */
  private def liveSession(app: api.Live[?], key: Option[String], bound: Option[api.Session], name: String): Stage[Frame, Frame, Unit] =
    val closed = Protocol.eventLine(Event.Closed)
    val framesToLines: Stage[Frame, String, Unit] =
      Stage.transduce(())((_, f) =>
        f match
          case Frame.Text(t) => Stage.tell[Frame, String](t)
          case Frame.Close(_, _) => Stage.tell[Frame, String](closed)
          case _ => pure(()),
        _ => pure(()))
    val linesToFrames: Stage[String, Frame, Unit] =
      Stage.transduce(())((_, l) => Stage.tell[String, Frame](Frame.Text(l)), _ => pure(()))
    through[Frame, String, Frame, Unit, Unit](
      through[Frame, String, String, Unit, Unit](framesToLines)(app.session(key, bound, name)))(linesToFrames)

  /** the whole server, one line: routes, sockets and pushes over Jetty
   * (okay-script-serve). `0` asks for any free port; `Jetty.port(s)`
   * says which. A `Resource`: releasing it stops the server. */
  def serve(port: Int, ssl: Option[javax.net.ssl.SSLContext] = None)
           (using CanBlock, Scheduler): org.eclipse.jetty.server.Server ! Resource =
    serveWith(port, ssl, ops = false)

  /** `serve` plus the ops endpoints when `ops` -- the pages win every
   * path they claim, so a page named `/stats` is still the page's */
  def serveWith(port: Int, ssl: Option[javax.net.ssl.SSLContext], ops: Boolean)
               (using CanBlock, Scheduler): org.eclipse.jetty.server.Server ! Resource =
    // the connector terminates TLS, so every request it brings IS
    // secure -- which is how a cookie gets its `Secure` without a
    // header anyone could forge (okay-script-cookie-flags)
    servedOverTls = ssl.isDefined
    okay.jetty.Jetty.serve(port)(if ops then routes orElse opsRoutes else routes)(ws, push, ssl)

  /** the synchronous core: one request in, one response out */
  def handle(r: Request): HttpResponse = counted {
    val split = splitUrl(r.url)
    val path = split.path
    val query = split.query
    if httpsOnly && !secureFor(webOf(r, path, query, Map.empty)) then toHttps(r)
    else if path == api.Live.JsPath then
      HttpResponse(200, Vector("Content-Type" -> "text/javascript; charset=utf-8"), Http.one(LiveJs.source.getBytes(UTF_8)))
    // the mobile web leg (Mobile.scala): the stylesheet, the service
    // worker, the manifest and the icon, all from the container
    else if path == Mobile.CssPath then
      HttpResponse(200, Vector("Content-Type" -> "text/css; charset=utf-8"), Http.one(Mobile.css.getBytes(UTF_8)))
    else if path == Mobile.SwPath then
      HttpResponse(200, Vector("Content-Type" -> "text/javascript; charset=utf-8", "Service-Worker-Allowed" -> "/"),
        Http.one(Mobile.serviceWorker.getBytes(UTF_8)))
    else if path == Mobile.IconPath then
      HttpResponse(200, Vector("Content-Type" -> "image/svg+xml"), Http.one(Mobile.icon.getBytes(UTF_8)))
    else if path == Mobile.ManifestPath then
      val q = parseQuery(query)
      HttpResponse(200, Vector("Content-Type" -> "application/manifest+json"),
        Http.one(Mobile.manifest(q.getOrElse("name", "okay"), q.getOrElse("start", "/")).getBytes(UTF_8)))
    else resolve(path) match
      case None => plain(404, "not found")
      case Some(Hit.Static(f)) =>
        statics.increment()
        serveStatic(r, f)
      case Some(Hit.PageFile(f, params)) =>
        pageRequests.increment()
        servePage(r, path, query, params, f)
  }

  def close(): Unit =
    pages.values.forEach(_.close())
    pages.clear()

  // ---- warm and stats (okay-script-warm)

  /** Compiles every page under `root` WITHOUT invoking one, in path
   * order, and answers the ones that did not compile: (the page's
   * path, its errors). The measured reason (docs/benchmarks.md §19):
   * the first page of a process costs ~870 ms because dotc warms up
   * in it, and a broken page is otherwise found by a visitor rather
   * than by the start. Language variants and included fragments are
   * pages too and are compiled with the rest; `i18n/` holds messages,
   * not pages, and is skipped. */
  def warm(): Vector[(String, Vector[String])] =
    val out = Vector.newBuilder[(String, Vector[String])]
    val walk = Files.walk(rootAbs)
    try
      walk.iterator().asScala.toVector
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".md"))
        .filterNot(p => rootAbs.relativize(p).iterator().asScala.exists(_.toString == I18nDir))
        .sortBy(_.toString)
        .foreach { f =>
          val errs = pageFor(f).warm()
          compiles.increment()
          if errs.nonEmpty then out += ((rootAbs.relativize(f).toString, errs))
        }
    finally walk.close()
    out.result()

  private val pageRequests = new java.util.concurrent.atomic.LongAdder
  private val compiles = new java.util.concurrent.atomic.LongAdder
  private val statics = new java.util.concurrent.atomic.LongAdder
  private val validated = new java.util.concurrent.atomic.LongAdder
  private val refusals = new java.util.concurrent.atomic.LongAdder
  private val missing = new java.util.concurrent.atomic.LongAdder
  private val failures = new java.util.concurrent.atomic.LongAdder

  /** what this Site has done and what it is holding -- plain values,
   * the shape `Store.Stats` set (specs/ops.md): a counter is a
   * count, a gauge is read at the moment it is asked for */
  def stats: Site.Stats = Site.Stats(
    pageRequests = pageRequests.sum, compiles = compiles.sum, statics = statics.sum,
    notModified = validated.sum, refused = refusals.sum, notFound = missing.sum,
    failed = failures.sum, pagesHeld = pages.size, sessions = sessions.size)

  private def counted(r: HttpResponse): HttpResponse =
    r.status match
      case 304 => validated.increment()
      case 401 | 403 => refusals.increment()
      case 404 => missing.increment()
      case s if s >= 500 => failures.increment()
      case _ => ()
    r

  /** `/healthz`, `/stats` and `/metrics` for THIS site -- deliberately
   * not part of `routes`: ops endpoints are a caller's decision about
   * exposure, chained with `orElse` (or served on another port), never
   * something a page directory silently gains. */
  def opsRouter: okay.http.Router = okay.http.Router.empty
    .on(okay.http.Method.Get, Site.Ops.healthz)(_ => pure(plain(200, "live=true")))
    .on(okay.http.Method.Get, Site.Ops.stats)(_ =>
      pure(HttpResponse(200, Vector("Content-Type" -> "application/json"), Http.one(stats.json.getBytes(UTF_8)))))
    .on(okay.http.Method.Get, Site.Ops.metrics)(_ =>
      pure(HttpResponse(200, Vector("Content-Type" -> "text/plain; version=0.0.4; charset=utf-8"),
        Http.one(stats.prometheus.getBytes(UTF_8)))))

  def opsRoutes: PartialFunction[Request, HttpResponse ! Async] = opsRouter.routes

  // ---- caching (okay-script-cache)

  /** a static file always carries validators -- an ETag from its size
   * and mtime, and Last-Modified -- and answers 304 to a conditional
   * request that still holds. Nothing is rendered, and for a large
   * file nothing is even read. */
  private def serveStatic(r: Request, f: Path): HttpResponse =
    val etag = Caching.etagOf(f)
    val mtime = Files.getLastModifiedTime(f).toMillis
    val validators = Vector("ETag" -> etag, "Last-Modified" -> Caching.formatDate(mtime)) ++
      hstsHeader(secureFor(webOf(r, pathOf(r.url), "", Map.empty)))
    if fresh(r, etag, mtime) then HttpResponse(304, validators, Http.one(Array.emptyByteArray))
    else HttpResponse(200, ("Content-Type" -> contentTypeOf(f)) +: validators, Http.one(Files.readAllBytes(f)))

  /** does the request's own copy still hold? `If-None-Match` decides
   * alone when it is present (RFC 9110's precedence), else the date */
  private def fresh(r: Request, etag: String, lastModified: Long): Boolean =
    val hs = r.headers
    def header(n: String) = hs.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }
    header("If-None-Match") match
      case Some(inm) => Caching.matches(inm, etag)
      case None => header("If-Modified-Since").exists(Caching.notModifiedSince(_, lastModified))

  // ---- routing

  def resolve(path: String): Option[Hit] =
    segments(path).flatMap { segs =>
      val trailingSlash = path.endsWith("/") || segs.isEmpty
      literal(segs, trailingSlash).orElse(parametric(segs))
    }

  private def segments(path: String): Option[Vector[String]] =
    val decoded = URLDecoder.decode(path.replace("+", "%2B"), UTF_8)
    if decoded.contains('\u0000') then None
    else
      val segs = decoded.split("/").toVector.filter(_.nonEmpty)
      // `i18n/` holds the message files and is never routed
      if segs.exists(s => s == ".." || s == "." || s.startsWith("[")) || segs.headOption.contains(I18nDir) then None
      else Some(segs)

  private def under(p: Path): Boolean = p.toAbsolutePath.normalize.startsWith(rootAbs)

  private def literal(segs: Vector[String], trailingSlash: Boolean): Option[Hit] =
    val base = segs.foldLeft(rootAbs)(_.resolve(_))
    if !under(base) then None
    else
      val md = if segs.isEmpty then None else Some(base.resolveSibling(base.getFileName.toString + ".md"))
      val index = base.resolve("index.md")
      if !trailingSlash && md.exists(Files.isRegularFile(_)) then md.map(Hit.PageFile(_, Map.empty))
      else if Files.isDirectory(base) && Files.isRegularFile(index) then Some(Hit.PageFile(index, Map.empty))
      else if !trailingSlash && Files.isRegularFile(base) && !base.getFileName.toString.endsWith(".md") then Some(Hit.Static(base))
      else None

  private val paramFile = """\[([A-Za-z_][A-Za-z0-9_]*)\]\.md""".r
  private val paramDir = """\[([A-Za-z_][A-Za-z0-9_]*)\]""".r

  private def listing(dir: Path): Vector[Path] =
    if !Files.isDirectory(dir) then Vector.empty
    else
      val s = Files.list(dir)
      try s.iterator().asScala.toVector.sortBy(_.getFileName.toString) finally s.close()

  /** `[name].md` / `[name]/` segments: a literal match wins at every
   * level; a parameter binds the segment's text */
  private def parametric(segs: Vector[String]): Option[Hit] =
    def go(dir: Path, rest: Vector[String], params: Map[String, String]): Option[Hit] =
      rest.headOption match
        case None =>
          val index = dir.resolve("index.md")
          if params.nonEmpty && Files.isRegularFile(index) then Some(Hit.PageFile(index, params)) else None
        case Some(seg) =>
          val tail = rest.tail
          val viaLiteral =
            if tail.isEmpty then
              val md = dir.resolve(seg + ".md")
              if params.nonEmpty && Files.isRegularFile(md) then Some(Hit.PageFile(md, params)) else None
            else
              val sub = dir.resolve(seg)
              if Files.isDirectory(sub) then go(sub, tail, params) else None
          viaLiteral.orElse {
            val entries = listing(dir)
            if tail.isEmpty then
              entries.iterator.flatMap { p =>
                p.getFileName.toString match
                  case paramFile(name) if Files.isRegularFile(p) => Some(Hit.PageFile(p, params + (name -> seg)))
                  case paramDir(name) if Files.isDirectory(p) && Files.isRegularFile(p.resolve("index.md")) =>
                    Some(Hit.PageFile(p.resolve("index.md"), params + (name -> seg)))
                  case _ => None
              }.nextOption()
            else
              entries.iterator.flatMap { p =>
                p.getFileName.toString match
                  case paramDir(name) if Files.isDirectory(p) => go(p, tail, params + (name -> seg))
                  case _ => None
              }.nextOption()
          }
    if segs.isEmpty then None else go(rootAbs, segs, Map.empty)

  // ---- serving a page

  private def pageFor(f: Path): Page =
    pages.computeIfAbsent(f, p => Page(p, classpath, tempRoot))


  /** a page's front-matter; a language VARIANT inherits its base
   * page's keys and overrides what it sets -- so `secure:` on
   * `admin.md` holds for `admin.uk.md` whether or not the translator
   * repeated it (okay-script-i18n) */
  private def frontMatter(f: Path): Map[String, String] =
    baseOf(f).filter(Files.isRegularFile(_)).map(ownFrontMatter).getOrElse(Map.empty) ++ ownFrontMatter(f)

  /** `page.<lang>.md` → `page.md`, for a language the site speaks */
  private def baseOf(f: Path): Option[Path] =
    val name = f.getFileName.toString
    val parts = name.split('.')
    if parts.length >= 3 && parts.last == "md" && languages.exists(_.equalsIgnoreCase(parts(parts.length - 2))) then
      Some(f.resolveSibling(parts.dropRight(2).mkString(".") + ".md"))
    else None

  private def ownFrontMatter(f: Path): Map[String, String] =
    val mtime = Files.getLastModifiedTime(f)
    fronts.get(f) match
      case (t, fm) if t == mtime => fm
      case _ =>
        val fm = Meta.parse(Files.readString(f)).frontMatter
        fronts.put(f, (mtime, fm)): Unit
        fm

  private def servePage(r: Request, path: String, query: String, params: Map[String, String], base: Path): HttpResponse =
    val web = webOf(r, path, query, params)
    val resp = new api.Response
    resp.contentType(HtmlUtf8)
    val sess = sessions.handle(web.cookies.get(SessionCookie))
    val lang = langOf(web)
    val f = localized(base, lang)
    api.Lang.setCurrent(lang)
    api.Container.setTranslator(Some(translator(lang)))
    // every cookie this request sets -- the container's own and the
    // page's -- carries Secure iff this request was secure
    val secure = secureFor(web)
    api.Response.setSecureByDefault(secure)
    // a `?lang=` choice is remembered by cookie for the requests after
    if web.query.get("lang").contains(lang) && !web.cookies.get(api.Lang.Cookie).contains(lang) then
      resp.cookie(api.Lang.Cookie, lang)
    api.Web.setCurrent(web)
    api.Response.setCurrent(resp)
    api.Session.setCurrent(sess)
    api.Error.setCurrent(None)
    api.Container.setIncluder(Some(includer))
    api.Container.setLiveRegistrar(Some((id, app) =>
      lives.put((including.get().headOption.getOrElse(f), id), app): Unit))
    api.Container.setIssuer(issue)
    api.Application.setCurrent(application)
    try
      val body = dispatch(f, web, resp, 0)
      if sess.invalidated then resp.cookie(SessionCookie, "", maxAge = Some(0), httpOnly = true)
      else if sess.created then resp.cookie(SessionCookie, sess.id, httpOnly = true)
      val bytes = if resp.redirected.isDefined then Array.empty[Byte] else body.getBytes(UTF_8)
      hstsHeader(secure).foreach((k, v) => resp.header(k, v))
      cached(r, web, base, resp, bytes)
    finally
      api.Response.setSecureByDefault(false)
      api.Container.setIncluder(None)
      api.Container.setLiveRegistrar(None)
      api.Container.setIssuer(None)
      api.Container.setTranslator(None)
      api.Lang.setCurrent(languages.head)
      api.Application.setCurrent(api.Application.detached)
      api.Principal.setCurrent(None)
      api.Web.setCurrent(api.Web.empty)
      api.Response.setCurrent(new api.Response)
      api.Session.setCurrent(api.Session.detached)
      api.Error.setCurrent(None)

  /** The rendered page as a response, with the validators and the
   * directive its `cache:` earns (okay-script-cache). Opt-in per
   * page, and never at the cost of privacy: a page that is `secure:`,
   * that set a cookie, or that sits on a session is `private`, never
   * `public`; anything that redirected or failed carries no cache at
   * all. A GET whose `If-None-Match` still holds gets a 304 with the
   * validators and no body.
   */
  private def cached(r: Request, web: api.Web, base: Path, resp: api.Response, bytes: Array[Byte]): HttpResponse =
    val cacheable = resp.status == 200 && resp.redirected.isEmpty &&
      (web.method == "GET" || web.method == "HEAD")
    Caching.maxAge(frontMatter(base)).filter(_ => cacheable) match
      case None => HttpResponse(resp.status, resp.headers, Http.one(bytes))
      case Some(seconds) =>
        val etag = Caching.etagOf(bytes)
        // shared only when nothing about this response is one visitor's
        val private_ = frontMatter(base).contains("secure") ||
          resp.headers.exists((k, _) => k.equalsIgnoreCase("Set-Cookie")) ||
          web.cookies.contains(SessionCookie)
        val directive = (if private_ then "private" else "public") + ", max-age=" + seconds
        val validators = Vector("ETag" -> etag, "Cache-Control" -> directive)
        if fresh(r, etag, 0L) then HttpResponse(304, resp.headers.filterNot(isBodyHeader) ++ validators, Http.one(Array.emptyByteArray))
        else HttpResponse(resp.status, resp.headers ++ validators, Http.one(bytes))

  private def isBodyHeader(h: (String, String)): Boolean =
    h._1.equalsIgnoreCase("Content-Type")

  /** renders `f`, following `forward`s (capped) and falling back to
   * the error page on failure; returns the body text */
  private def dispatch(f: Path, web: api.Web, resp: api.Response, forwards: Int): String =
    access(f, web, api.Session.current) match
      case Access.Open => render(f, web, resp, forwards)
      case Access.Granted(p) =>
        api.Principal.setCurrent(Some(p))
        render(f, web, resp, forwards)
      case Access.Login(to) =>
        resp.redirect(to + "?next=" + java.net.URLEncoder.encode(web.path, UTF_8))
        ""
      case Access.Refused(status, error) =>
        resp.status = status
        resp.contentType(TextUtf8)
        resp.header("WWW-Authenticate", s"""Bearer realm="$realm", error="$error"""")
        error
      case Access.Misconfigured(why) =>
        resp.status = 500
        resp.contentType(TextUtf8)
        s"${rootAbs.relativize(f)}: $why"

  /** the `secure:` verdict for `f`, from the request's bearer header
   * or the session's stored token */
  private def access(f: Path, web: api.Web, sess: api.Session): Access =
    frontMatter(f).get("secure") match
      case None => Access.Open
      case Some(scope) =>
        verify match
          case None => Access.Misconfigured("secure: on a Site without a verifier (Site(verify = Some(...)))")
          case Some(check) =>
            val token = web.header("Authorization").filter(_.startsWith("Bearer ")).map(_.drop(7))
              .orElse(sess.get(api.Principal.TokenAttribute))
            val policy = if scope.trim == "any" then Policy.allowAll else Policy.scoped(scope.trim)
            token.map(check) match
              case Some(Verified.Ok(p)) =>
                policy(p, web.method, web.path) match
                  case Decision.Permit => Access.Granted(p)
                  case Decision.Deny(_) => Access.Refused(403, "insufficient_scope")
              case _ =>
                // no token and a bad token refuse alike: the WHY stays
                // server-side, as okay-security's Secure has it
                findLoginPage(f) match
                  case Some(lp) => Access.Login(urlOf(lp))
                  case None => Access.Refused(401, "invalid_token")

  private def findLoginPage(f: Path): Option[Path] =
    frontMatter(f).get("loginPage").flatMap(ref => relative(f, ref)).filter(Files.isRegularFile(_))
      .orElse(Some(rootAbs.resolve("login.md")).filter(Files.isRegularFile(_)))
      .filter(_ != f)

  /** the URL a page file answers at */
  private def urlOf(f: Path): String =
    val rel = rootAbs.relativize(f).toString.replace(java.io.File.separatorChar, '/')
    val noExt = if rel.endsWith(".md") then rel.dropRight(3) else rel
    val noIndex = if noExt == "index" then "" else if noExt.endsWith("/index") then noExt.dropRight(6) + "/" else noExt
    "/" + noIndex

  private def render(f: Path, web: api.Web, resp: api.Response, forwards: Int): String =
    frontMatter(f).get("contentType").foreach(resp.contentType)
    val result = rendering(f)(pageFor(f).render(web))
    result.thrown match
      case Some(api.Forwarded(target)) =>
        if forwards >= MaxForwards then
          errorPage(f, web, resp, Result(ok = false, stdout = "", errors = Vector(s"forward chain longer than $MaxForwards, last to $target"), thrown = None))
        else
          resolve(pathOf(target)) match
            case Some(Hit.PageFile(g, ps)) =>
              val w2 = web.copy(params = web.params ++ ps + ("forwarded" -> target))
              dispatch(localized(g, api.Lang.current), w2, resp, forwards + 1)
            case _ =>
              resp.status = 404
              resp.contentType(TextUtf8)
              s"forward target not found: $target"
      case _ if !result.ok => errorPage(f, web, resp, result)
      case _ => result.stdout

  private def errorPage(f: Path, web: api.Web, resp: api.Response, result: Result): String =
    val message =
      if result.errors.nonEmpty then result.errors.mkString("\n")
      else result.thrown.map(_.toString).getOrElse("failed")
    val err = api.Error(message, result.errors, result.thrown)
    resp.status = 500
    def plainText(extra: String): String =
      resp.contentType(TextUtf8)
      s"${rootAbs.relativize(f)}: $message$extra"
    findErrorPage(f) match
      case Some(ep) =>
        api.Error.setCurrent(Some(err))
        resp.contentType(HtmlUtf8)
        frontMatter(ep).get("contentType").foreach(resp.contentType)
        val r2 = rendering(ep)(pageFor(ep).render(web))
        if r2.ok then r2.stdout
        else
          val second =
            if r2.errors.nonEmpty then r2.errors.mkString("\n") else r2.thrown.map(_.toString).getOrElse("failed")
          plainText(s"\n\nerror page ${rootAbs.relativize(ep)} failed too: $second")
      case None => plainText("")

  private def findErrorPage(f: Path): Option[Path] =
    frontMatter(f).get("errorPage").flatMap(ref => relative(f, ref)).filter(Files.isRegularFile(_))
      .orElse(Some(rootAbs.resolve("error.md")).filter(Files.isRegularFile(_)))
      .map(localized(_, api.Lang.current))
      .filter(_ != f)

  // ---- languages (okay-script-i18n)

  /** `?lang=`, the cookie, `Accept-Language`, the first language */
  private def langOf(web: api.Web): String =
    def known(l: String): Option[String] = languages.find(_.equalsIgnoreCase(l))
    web.query.get("lang").flatMap(known)
      .orElse(web.cookies.get(api.Lang.Cookie).flatMap(known))
      .orElse(web.header("Accept-Language").flatMap(acceptLanguage(_, languages)))
      .getOrElse(languages.head)

  /** `page.<lang>.md` beside `page.md`, when it exists and `lang` is
   * one the site speaks; else the page itself */
  private def localized(f: Path, lang: String): Path =
    val name = f.getFileName.toString
    if lang == languages.head || !name.endsWith(".md") then f
    else
      val v = f.resolveSibling(name.dropRight(3) + "." + lang + ".md")
      if Files.isRegularFile(v) then v else f

  private val messages = new ConcurrentHashMap[String, (FileTime, Map[String, String])]

  /** `i18n/<lang>.yaml`, a flat mapping, cached by mtime; absent is empty */
  private def messagesOf(lang: String): Map[String, String] =
    val f = rootAbs.resolve(I18nDir).resolve(lang + ".yaml")
    if !Files.isRegularFile(f) then Map.empty
    else
      val mtime = Files.getLastModifiedTime(f)
      messages.get(lang) match
        case (t, m) if t == mtime => m
        case _ =>
          val m = Meta.parseYaml(Files.readAllLines(f).asScala.toVector) match
            case Meta.Value.Obj(fields) => fields.collect { case (k, Meta.Value.Str(v)) => k -> v }.toMap
            case _ => Map.empty[String, String]
          messages.put(lang, (mtime, m)): Unit
          m

  private def translator(lang: String): String => Option[String] =
    val own = messagesOf(lang)
    lazy val fallback = if lang == languages.head then Map.empty[String, String] else messagesOf(languages.head)
    key => own.get(key).orElse(fallback.get(key))

  /** a page reference from inside `f`: absolute from the root with a
   * leading `/`, else relative to `f`'s directory; never outside root */
  private def relative(f: Path, ref: String): Option[Path] =
    val p =
      if ref.startsWith("/") then rootAbs.resolve(ref.drop(1))
      else f.getParent.resolve(ref)
    val n = p.toAbsolutePath.normalize
    if under(n) then Some(n) else None

  private def rendering[A](f: Path)(body: => A): A =
    val prev = including.get()
    including.set(f :: prev)
    try body finally including.set(prev)

  private def includer: String => String = name =>
    val stack = including.get()
    if stack.length > MaxIncludeDepth then
      throw new IllegalStateException(s"include(\"$name\"): nesting deeper than $MaxIncludeDepth -- a page including itself?")
    val from = stack.headOption.getOrElse(rootAbs.resolve("index.md"))
    val g = relative(from, name).filter(Files.isRegularFile(_)).map(localized(_, api.Lang.current))
      .getOrElse(throw new java.io.FileNotFoundException(s"include(\"$name\"): no such page under ${rootAbs.relativize(from.getParent)}"))
    val r = rendering(g)(pageFor(g).render(api.Web.current))
    if r.ok then r.stdout
    else
      r.thrown match
        case Some(t) => throw t
        case None => throw new RuntimeException(s"include(\"$name\") failed to compile: ${r.errors.mkString("; ")}")

  // ---- the request, translated

  private def webOf(r: Request, path: String, query: String, params: Map[String, String]): api.Web =
    val headers = r.headers.toMap
    val ct = r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }.getOrElse("")
    val bodyBytes = r.body.bytes
    val bodyText = new String(bodyBytes, UTF_8)
    val parts =
      if ct.regionMatches(true, 0, "multipart/form-data", 0, 19) then
        Multipart.boundaryOf(ct).map(Multipart.parse(bodyBytes, _)).getOrElse(Vector.empty)
      else Vector.empty
    val form =
      if ct.startsWith("application/x-www-form-urlencoded") then parseQuery(bodyText)
      else parts.filter(!_.isFile).map(p => p.name -> p.text).toMap
    api.Web(r.method.name, path, parseQuery(query), headers, form, cookiesOf(r), bodyText, params, parts)

  /** was THIS request secure? `secureCookies` decides when it is
   * given; otherwise this Site's own server terminates TLS, or a
   * trusted proxy said the client's connection did
   * (okay-script-cookie-flags).
   *
   * NOT read from the request's URL: okay-http's `Request` carries a
   * path, not an absolute URL (okay-jetty builds it from
   * `getHttpURI.getPathQuery`), so there is no scheme in it to
   * inspect — the first draft of this method tried and would have
   * answered `false` for every HTTPS request. The server knows
   * instead, and says so in `serveWith`. */
  private def secureFor(web: api.Web): Boolean = secureCookies.getOrElse {
    servedOverTls ||
      (trustForwarded && web.header("X-Forwarded-Proto").exists(_.split(",").head.trim.equalsIgnoreCase("https")))
  }

  @volatile private var servedOverTls = false

  /** the same URL on https: the authority is the request's own `Host`
   * (a proxy forwards the one the CLIENT asked for), and a request
   * without one cannot be redirected anywhere honest -- it gets the
   * refusal rather than a guessed hostname */
  private def toHttps(r: Request): HttpResponse =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("host") => v } match
      case Some(host) =>
        HttpResponse(301, Vector("Location" -> s"https://$host${r.url}", "Content-Type" -> TextUtf8),
          Http.one(Array.emptyByteArray))
      case None => plain(400, "https is required, and this request carries no Host to redirect to")

  /** HSTS rides only on a response the container knows was secure --
   * announcing it over plaintext is how a site locks itself out */
  private def hstsHeader(secure: Boolean): Vector[(String, String)] =
    if secure then hsts.map(age => ("Strict-Transport-Security", s"max-age=$age")).toVector else Vector.empty

  private def cookiesOf(r: Request): Map[String, String] =
    r.headers.collect { case (k, v) if k.equalsIgnoreCase("cookie") => v }
      .flatMap(_.split(";").toVector)
      .flatMap { kv =>
        val i = kv.indexOf('=')
        if i <= 0 then None else Some(kv.substring(0, i).trim -> kv.substring(i + 1).trim)
      }.toMap

object Site:

  /**
   * The ops surface as VALUES (specs/optics-outside.md, stage 4).
   *
   * A deployment has to name a probe path and has no `Site` to ask,
   * so the paths exist apart from the router that serves them —
   * `ScriptDeploy` names its liveness and readiness from here instead
   * of writing a literal of its own. That is the same principle this
   * module already applied to SETTINGS ("derived from the value the
   * program itself reads ... a name this deployment could invent does
   * not exist"), extended at last to paths. `TestSiteOps` asserts that
   * `paths` is exactly what `opsRouter` dispatches, so the two cannot
   * drift.
   */
  object Ops:
    val healthz: okay.http.Route[EmptyTuple] = okay.http.Route / "healthz"
    val stats: okay.http.Route[EmptyTuple] = okay.http.Route / "stats"
    val metrics: okay.http.Route[EmptyTuple] = okay.http.Route / "metrics"

    /** every path a Site's ops surface serves */
    val paths: Set[String] = Vector(healthz, stats, metrics).map(_.describe).toSet


  /** what a Site has done and what it holds (okay-script-warm) --
   * plain values, `Store.Stats`' own shape: counters since the Site
   * was built, gauges read at the moment they are asked for. The two
   * renderings are PURE mappings, the move `okay.ops.Prom` makes for
   * a store: no client library, a documented string. */
  final case class Stats(pageRequests: Long, compiles: Long, statics: Long,
                         notModified: Long, refused: Long, notFound: Long,
                         failed: Long, pagesHeld: Int, sessions: Int):
    def json: String =
      s"""{"pageRequests":$pageRequests,"compiles":$compiles,"statics":$statics,""" +
        s""""notModified":$notModified,"refused":$refused,"notFound":$notFound,""" +
        s""""failed":$failed,"pagesHeld":$pagesHeld,"sessions":$sessions}"""

    def prometheus: String =
      val counters = Vector(
        ("okay_script_page_requests_total",
          "requests that resolved to a page -- a refusal and a failure are page requests too, which is why this is not called renders", pageRequests),
        ("okay_script_compiles_total", "page compiles paid for (a warm, or a file that changed)", compiles),
        ("okay_script_static_total", "static files served", statics),
        ("okay_script_not_modified_total", "conditional requests answered 304", notModified),
        ("okay_script_refused_total", "requests refused by a secure: page (401/403)", refused),
        ("okay_script_not_found_total", "requests that matched no page (404)", notFound),
        ("okay_script_failed_total", "requests that failed (5xx)", failed))
      val gauges = Vector(
        ("okay_script_pages_held", "compiled pages held in memory", pagesHeld.toLong),
        ("okay_script_sessions", "live sessions", sessions.toLong))
      val sb = new StringBuilder
      for (name, help, v) <- counters do sb ++= s"# HELP $name $help\n# TYPE $name counter\n$name $v\n"
      for (name, help, v) <- gauges do sb ++= s"# HELP $name $help\n# TYPE $name gauge\n$name $v\n"
      sb.result()

  /** the `secure:` verdict -- see specs/okay-script.md "Declarative security" */
  enum Access:
    case Open
    case Granted(principal: okay.security.Principal)
    case Login(to: String)
    case Refused(status: Int, error: String)
    case Misconfigured(why: String)

  val SessionCookie = "OKAYSESSID"
  val I18nDir = "i18n"
  val MaxForwards = 8

  /** the first of the header's languages, by q, that the site speaks
   * -- matched on the primary subtag (`uk-UA` is `uk`) */
  def acceptLanguage(header: String, languages: Vector[String]): Option[String] =
    val ranked = header.split(",").toVector.flatMap { part =>
      val ps = part.trim.split(";").toVector.map(_.trim)
      if ps.isEmpty || ps(0).isEmpty then None
      else
        val q = ps.drop(1).collectFirst { case p if p.startsWith("q=") => p.drop(2).toDoubleOption.getOrElse(0.0) }.getOrElse(1.0)
        Some(ps(0).toLowerCase -> q)
    }.sortBy(-_._2)
    ranked.iterator.flatMap { (tag, _) =>
      val primary = tag.takeWhile(_ != '-')
      languages.find(l => l.equalsIgnoreCase(tag) || l.equalsIgnoreCase(primary))
    }.nextOption()
  val MaxIncludeDepth = 16
  private val HtmlUtf8 = "text/html; charset=utf-8"
  private val TextUtf8 = "text/plain; charset=utf-8"

  /**
   * A url split at the '?', as a NAMED pair (split-url-named,
   * 2026-09-12). Both halves are `String`, so while they were
   * positional a caller could take them the wrong way round and get
   * the query string as the path, compiling. This is public API, so
   * the caller who could do that is not necessarily in this
   * repository. A positional `val (a, b) = splitUrl(u)` still works
   * and still binds by position; reading `.path` and `.query` is what
   * makes the mistake impossible.
   */
  def splitUrl(url: String): (path: String, query: String) =
    val i = url.indexOf('?')
    if i < 0 then (path = url, query = "")
    else (path = url.substring(0, i), query = url.substring(i + 1))

  def pathOf(url: String): String = splitUrl(url).path

  def parseQuery(q: String): Map[String, String] =
    q.split("&").toVector.filter(_.nonEmpty).flatMap { kv =>
      val i = kv.indexOf('=')
      val (k, v) = if i < 0 then (kv, "") else (kv.substring(0, i), kv.substring(i + 1))
      if k.isEmpty then None else Some(URLDecoder.decode(k, UTF_8) -> URLDecoder.decode(v, UTF_8))
    }.toMap

  private val contentTypes = Map(
    "html" -> "text/html; charset=utf-8",
    "htm" -> "text/html; charset=utf-8",
    "css" -> "text/css; charset=utf-8",
    "js" -> "text/javascript; charset=utf-8",
    "json" -> "application/json",
    "txt" -> "text/plain; charset=utf-8",
    "svg" -> "image/svg+xml",
    "png" -> "image/png",
    "jpg" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "gif" -> "image/gif",
    "ico" -> "image/x-icon",
    "woff2" -> "font/woff2",
    "woff" -> "font/woff",
  )

  def contentTypeOf(f: Path): String =
    val n = f.getFileName.toString
    val ext = n.lastIndexOf('.') match
      case -1 => ""
      case i => n.substring(i + 1).toLowerCase
    contentTypes.getOrElse(ext, "application/octet-stream")

  private def plain(status: Int, s: String): HttpResponse =
    HttpResponse(status, Vector("Content-Type" -> TextUtf8), Http.one(s.getBytes(UTF_8)))
