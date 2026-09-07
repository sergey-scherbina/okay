package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The half the container never had: the same pages `serve` compiles
 * per request, rendered ONCE to plain files (specs/okay-script.md,
 * "The command line").
 *
 * What comes out is a directory any web server can hold, with no JVM
 * in the deployment at all — and it is built by driving a real `Site`
 * with real requests rather than by reimplementing page resolution.
 * That is the whole design decision here: `Meta`, interpolation,
 * includes, declares, i18n variants, content types and static files
 * all come for free and cannot drift from what `serve` does, because
 * they ARE what serve does.
 *
 * A page a static site cannot hold is REFUSED by name, and the two
 * that can be detected precisely are detected precisely: a page that
 * touches its session (there is no session without a request) and a
 * page that redirects (a file cannot be a 302). The honest limit is
 * in the spec: `Web.current.form` at build time is an empty form, not
 * a refusal, because `Web` is a data value with nowhere to put one.
 */
object Build:

  /** what a static site cannot give a page, thrown by the seams that
   * can throw — the message names the call, and `Site`'s error page
   * carries it out to the builder */
  final case class NoRequest(what: String)
    extends RuntimeException(
      s"$what needs a request, and a static site has no request — " +
        "serve this page instead, or take the call out of it")

  final case class Page(path: String, out: String, lang: String)

  final case class Report(
    written: Vector[Page] = Vector.empty,
    copied: Vector[String] = Vector.empty,
    skipped: Vector[(String, String)] = Vector.empty,
    failed: Vector[(String, String)] = Vector.empty,
  ):
    def ok: Boolean = failed.isEmpty

  /**
   * A session engine that answers nothing, because there is nothing
   * to answer with — every method names itself.
   *
   * It is installed on the SITE rather than around the render: the
   * container binds a session per request on the request thread, so
   * anything set outside is overwritten before a page sees it. Found
   * by the test that expected a refusal and got a built page.
   */
  private val noSessions: Sessions = new Sessions:
    def handle(existing: Option[String], now: Long): Sessions.Bound = new Sessions.Bound:
      // the container reads these two to decide a cookie, and a build
      // has no cookie to decide: neither is a page touching a session
      def created: Boolean = false
      def invalidated: Boolean = false
      def id: String = ""
      def get(key: String): Option[String] = throw NoRequest(s"Session.get($key)")
      def set(key: String, value: String): Unit = throw NoRequest(s"Session.set($key)")
      def remove(key: String): Unit = throw NoRequest(s"Session.remove($key)")
      def attributes: Map[String, String] = throw NoRequest("Session.attributes")
      def invalidate(): Unit = throw NoRequest("Session.invalidate()")
    def size: Int = 0

  /**
   * Render every page under `root` into `out`.
   *
   * `index.md` becomes `index.html`; `shop.md` becomes `shop.html`;
   * `shop/index.md` becomes `shop/index.html`. A language variant
   * `page.uk.md` becomes `uk/page.html`, so a whole site exists once
   * per language under its own prefix. Static files are copied
   * byte-for-byte.
   */
  def run(root: Path, out: Path, languages: Vector[String] = Vector("en")): Report =
    val site = new Site(root, sessions = noSessions, languages = languages)
    try build(site, root.toAbsolutePath.normalize, out, languages)
    finally site.close()

  private def build(site: Site, root: Path, out: Path, languages: Vector[String]): Report =
    var report = Report()
    val files =
      val walk = Files.walk(root)
      try walk.iterator().asScala.toVector.filter(Files.isRegularFile(_)).sortBy(_.toString)
      finally walk.close()

    for f <- files do
      val rel = root.relativize(f).toString.replace('\\', '/')
      val name = f.getFileName.toString
      if rel.startsWith(Site.I18nDir + "/") then
        // messages, not pages
        ()
      else if !name.endsWith(".md") then
        val target = out.resolve(rel)
        Option(target.getParent).foreach(Files.createDirectories(_): Unit)
        Files.copy(f, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING): Unit
        report = report.copy(copied = report.copied :+ rel)
      else if name.startsWith("[") || rel.contains("/[") then
        // a parameter has nothing to bind to without a request
        report = report.copy(skipped = report.skipped :+
          (rel, "a [param] page has no parameter to bind without a request"))
      else if variantOf(name, languages).isDefined then
        // rendered under its language's prefix, from the base path
        ()
      else
        for lang <- languages do
          val url = urlOf(rel, lang, languages)
          renderOne(site, url, lang) match
            case Right(bytes) =>
              val target = out.resolve(outputOf(rel, lang, languages))
              Option(target.getParent).foreach(Files.createDirectories(_): Unit)
              Files.write(target, bytes): Unit
              report = report.copy(written = report.written :+
                Page(rel, out.relativize(target).toString.replace('\\', '/'), lang))
            case Left(why) =>
              report = report.copy(failed = report.failed :+ (s"$rel [$lang]", why))
    report

  /** one page, through the real Site, with the seams a static site
   * cannot honour set to refuse */
  private def renderOne(site: Site, url: String, lang: String): Either[String, Array[Byte]] =
    try
      val r = site.handle(Request.get(url, Seq("Accept-Language" -> lang)))
      val body = Async.run[Array[Byte], Pure](Http.bytes(r).map(_.toArray)).runWith
      val text = String(body, UTF_8)
      if r.status == 200 then
        location(r) match
          case Some(to) => Left(s"the page redirects to $to, and a file cannot be a 302")
          case None => Right(body)
      else
        location(r) match
          case Some(to) => Left(s"the page redirects to $to, and a file cannot be a 302")
          case None if text.contains("needs a request") =>
            // the refusal above, carried out through Site's error page
            Left(text.linesIterator.find(_.contains("needs a request")).getOrElse(text).trim)
          case None => Left(s"the page answered ${r.status}: ${text.trim.take(300)}")
    catch
      case e: NoRequest => Left(e.getMessage)
      case e: Throwable => Left(Option(e.getMessage).getOrElse(e.getClass.getName))

  private def location(r: HttpResponse): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("Location") => v }

  /** `index.md` -> `/`, `shop.md` -> `/shop`, `a/index.md` -> `/a/` */
  private[script] def urlOf(rel: String, lang: String, languages: Vector[String]): String =
    val base = rel.stripSuffix(".md")
    val path =
      if base == "index" then "/"
      else if base.endsWith("/index") then "/" + base.stripSuffix("index")
      else "/" + base
    if lang == languages.head then path else path + (if path.contains("?") then "&" else "?") + s"lang=$lang"

  /** where it lands: `index.md` -> `index.html`, and a non-default
   * language gets the whole site again under its own prefix */
  private[script] def outputOf(rel: String, lang: String, languages: Vector[String]): String =
    val html = rel.stripSuffix(".md") + ".html"
    if lang == languages.head then html else s"$lang/$html"

  /** `page.uk.md` when `uk` is a language this site speaks */
  private[script] def variantOf(name: String, languages: Vector[String]): Option[String] =
    val parts = name.split('.')
    if parts.length >= 3 && parts.last == "md" then languages.find(_.equalsIgnoreCase(parts(parts.length - 2)))
    else None
