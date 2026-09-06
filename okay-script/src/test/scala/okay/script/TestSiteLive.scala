package okay.script

import okay.*
import okay.given
import okay.jetty.Jetty

import java.net.{HttpURLConnection, URI}
import java.nio.file.{Files, Path}

/** okay-script-site over a REAL Jetty port: a two-page store with a
 * session-backed cart, a redirect after POST, an included header, a
 * static stylesheet and a 404. Live-tagged like every suite that
 * binds a port. See specs/okay-script.md "Site — the container".
 */
class TestSiteLive extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def deleteAll(p: Path): Unit =
    if Files.isDirectory(p) then
      val s = Files.list(p)
      try s.forEach(deleteAll) finally s.close()
    Files.deleteIfExists(p): Unit

  private def page(root: Path, rel: String, content: String): Unit =
    val f = root.resolve(rel)
    Files.createDirectories(f.getParent)
    Files.writeString(f, content): Unit

  final case class Reply(status: Int, body: String, headers: Map[String, List[String]])

  private def call(url: String, method: String = "GET", form: Option[String] = None, cookie: Option[String] = None): Reply =
    val c = URI.create(url).toURL.openConnection() match
      case h: HttpURLConnection => h
      case other => throw new IllegalStateException(s"not http: $other")
    c.setInstanceFollowRedirects(false)
    c.setRequestMethod(method)
    cookie.foreach(v => c.setRequestProperty("Cookie", v))
    form.foreach { f =>
      c.setDoOutput(true)
      c.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
      c.getOutputStream.write(f.getBytes("UTF-8"))
    }
    val status = c.getResponseCode
    val in = if status >= 400 then c.getErrorStream else c.getInputStream
    val body = if in == null then "" else new String(in.readAllBytes(), "UTF-8")
    import scala.jdk.CollectionConverters.*
    val hs = c.getHeaderFields.asScala.collect { case (k, v) if k != null => k.toLowerCase -> v.asScala.toList }.toMap
    c.disconnect()
    Reply(status, body, hs)

  test("a store: session cart across requests, POST + redirect, include, static css, 404") {
    val root = Files.createTempDirectory("okay-script-site-live-")
    val api = "```scala\nimport okay.script.api.*\n```\n"
    page(root, "parts/header.md", api + "<nav>store | cart: ${Session.current.get(\"cart\").getOrElse(\"0\")}</nav>\n")
    page(root, "index.md", api + "```scala\ninclude(\"parts/header.md\")\n```\n<h1>Home</h1>\n")
    page(root, "cart.md", api +
      """```scala
        |if Web.current.method == "POST" then
        |  val n = Session.current.get("cart").map(_.toInt).getOrElse(0) + Web.current.form("qty").toInt
        |  Session.current.set("cart", n.toString)
        |  Response.current.redirect("/")
        |include("parts/header.md")
        |```
        |<h1>Cart</h1>
        |""".stripMargin)
    page(root, "style.css", "nav { font-weight: bold }\n")
    val site = Site(root)
    try
      Resource.run[Unit, Pure](Jetty.serve(0)(site.routes)().map { server =>
        val base = s"http://127.0.0.1:${Jetty.port(server)}"

        val home = call(s"$base/")
        assertEquals(home.status, 200)
        assert(home.body.contains("cart: 0") && home.body.contains("<h1>Home</h1>"), home.body)
        assert(home.headers.get("content-type").exists(_.exists(_.startsWith("text/html"))), home.headers.toString)
        assert(!home.headers.contains("set-cookie"), home.headers.toString)

        val add = call(s"$base/cart", method = "POST", form = Some("qty=2"))
        assertEquals(add.status, 302)
        assertEquals(add.headers.get("location").map(_.head), Some("/"))
        val cookie = add.headers.get("set-cookie").flatMap(_.find(_.startsWith(Site.SessionCookie + "=")))
          .getOrElse(fail("no session cookie: " + add.headers))
        val sid = cookie.takeWhile(_ != ';')

        val again = call(s"$base/cart", method = "POST", form = Some("qty=3"), cookie = Some(sid))
        assertEquals(again.status, 302)

        val home2 = call(s"$base/", cookie = Some(sid))
        assert(home2.body.contains("cart: 5"), home2.body)

        val css = call(s"$base/style.css")
        assertEquals(css.status, 200)
        assertEquals(css.body, "nav { font-weight: bold }\n")
        assertEquals(css.headers.get("content-type").map(_.head), Some("text/css; charset=utf-8"))

        assertEquals(call(s"$base/missing").status, 404)
      }).runWith
    finally
      site.close()
      deleteAll(root)
  }
