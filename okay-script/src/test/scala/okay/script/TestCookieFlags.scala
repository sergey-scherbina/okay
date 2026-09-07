package okay.script

import okay.*
import okay.given
import okay.http.{Request, Response as HttpResponse}

import java.nio.file.{Files, Path}

/** okay-script-cookie-flags: `Secure` and `SameSite` on the cookies a
 * Site sets. See specs/okay-script.md "Cookie flags".
 */
class TestCookieFlags extends munit.FunSuite:

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def setCookies(r: HttpResponse): Vector[String] =
    r.headers.collect { case (k, v) if k.equalsIgnoreCase("set-cookie") => v }.toVector

  private def withSite[A](build: Path => Site)(body: Site => A): A =
    val root = Files.createTempDirectory("okay-script-cookies-")
    def page(rel: String, c: String): Unit = Files.writeString(root.resolve(rel), c): Unit
    page("start.md", Api + "```scala\nSession.current.set(\"k\", \"v\")\n```\nstarted\n")
    page("own.md", Api + "```scala\nResponse.current.cookie(\"pref\", \"dark\")\nResponse.current.cookie(\"legacy\", \"1\", sameSite = \"\", secure = false)\n```\nset\n")
    page("bye.md", Api + "```scala\nSession.current.set(\"k\", \"v\")\nSession.current.invalidate()\n```\ngone\n")
    val site = build(root)
    try body(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("plaintext by default: SameSite=Lax and HttpOnly on the session cookie, no Secure") {
    withSite(Site(_)) { site =>
      val c = setCookies(site.handle(Request.get("/start"))).head
      assert(c.startsWith(Site.SessionCookie + "="), c)
      assert(c.contains("HttpOnly"), c)
      assert(c.contains("SameSite=Lax"), c)
      assert(!c.contains("Secure"), c)
    }
  }

  test("secureCookies = Some(true) forces Secure on every cookie the request sets, the container's and the page's") {
    withSite(Site(_, secureCookies = Some(true))) { site =>
      assert(setCookies(site.handle(Request.get("/start"))).head.contains("Secure"))
      val own = setCookies(site.handle(Request.get("/own")))
      assertEquals(own.length, 2)
      assert(own.exists(c => c.startsWith("pref=dark") && c.contains("Secure") && c.contains("SameSite=Lax")), own.toString)
      // a page that asks for the other answer gets it: no Secure, no SameSite
      assert(own.exists(c => c.startsWith("legacy=1") && !c.contains("Secure") && !c.contains("SameSite")), own.toString)
    }
  }

  test("a forwarded claim counts only where the Site was told to trust one") {
    val forwarded = Seq("X-Forwarded-Proto" -> "https")
    withSite(Site(_)) { site =>
      assert(!setCookies(site.handle(Request.get("/start", forwarded))).head.contains("Secure"))
    }
    withSite(Site(_, trustForwarded = true)) { site =>
      assert(setCookies(site.handle(Request.get("/start", forwarded))).head.contains("Secure"))
      // the header a proxy chain writes: the CLIENT's protocol is first
      assert(setCookies(site.handle(Request.get("/start", Seq("X-Forwarded-Proto" -> "https, http")))).head.contains("Secure"))
      assert(!setCookies(site.handle(Request.get("/start", Seq("X-Forwarded-Proto" -> "http, https")))).head.contains("Secure"))
      assert(!setCookies(site.handle(Request.get("/start"))).head.contains("Secure"))
    }
    // an explicit answer beats any header, in both directions
    withSite(Site(_, secureCookies = Some(false), trustForwarded = true)) { site =>
      assert(!setCookies(site.handle(Request.get("/start", forwarded))).head.contains("Secure"))
    }
  }

  test("the expiring cookie an invalidate sends carries the same flags -- a browser drops it only on a match") {
    withSite(Site(_, secureCookies = Some(true))) { site =>
      val c = setCookies(site.handle(Request.get("/bye"))).head
      assert(c.contains("Max-Age=0") && c.contains("Secure") && c.contains("HttpOnly") && c.contains("SameSite=Lax"), c)
    }
  }

  test("the language cookie is not HttpOnly (a page may read it) but is Secure and SameSite like the rest") {
    val root = Files.createTempDirectory("okay-script-cookies-lang-")
    Files.writeString(root.resolve("index.md"), "home\n"): Unit
    Files.writeString(root.resolve("index.uk.md"), "домівка\n"): Unit
    val site = Site(root, languages = Vector("en", "uk"), secureCookies = Some(true))
    try
      val c = setCookies(site.handle(Request.get("/?lang=uk"))).find(_.startsWith(api.Lang.Cookie)).getOrElse(fail("no lang cookie"))
      assert(c.contains("Secure") && c.contains("SameSite=Lax"), c)
      assert(!c.contains("HttpOnly"), c)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
