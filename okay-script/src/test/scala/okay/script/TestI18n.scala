package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}
import okay.security.SessionIssuer

import java.nio.file.{Files, Path}

/** okay-script-i18n: page variants and messages by the request's
 * language. See specs/okay-script.md "Languages".
 */
class TestI18n extends munit.FunSuite:

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def header(r: HttpResponse, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  private def withSite[A](body: (Site, Path) => A): A =
    val root = Files.createTempDirectory("okay-script-i18n-")
    def page(rel: String, c: String): Unit =
      val f = root.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.writeString(f, c): Unit
    page("index.md", Api + "hello (${Lang.current}) ${t(\"greet\", \"Ann\")} ${t(\"only.en\")} ${t(\"nope\")}\n")
    page("index.uk.md", Api + "привіт (${Lang.current}) ${t(\"greet\", \"Ann\")} ${t(\"only.en\")} ${t(\"nope\")}\n")
    page("plain.md", Api + "plain (${Lang.current})\n")
    page("parts/header.md", "[en header]")
    page("parts/header.uk.md", "[uk header]")
    page("wrap.md", Api + "```scala\ninclude(\"parts/header.md\")\n```\n")
    page("admin.md", "---\nsecure: admin\n---\nadmin\n")
    page("admin.uk.md", "адмін\n")
    page("i18n/en.yaml", "greet: Hello, {0}!\nonly.en: en-only\n")
    page("i18n/uk.yaml", "greet: Привіт, {0}!\n")
    val site = Site(root, languages = Vector("en", "uk"), verify = Some(SessionIssuer().verify(_)))
    try body(site, root)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("?lang= renders the variant and sets the cookie; the cookie alone selects; Accept-Language selects; unknown falls back; no variant renders the base") {
    withSite { (site, _) =>
      val en = site.handle(Request.get("/"))
      assert(text(en).contains("hello (en)"), text(en))
      val uk = site.handle(Request.get("/?lang=uk"))
      assert(text(uk).contains("привіт (uk)"), text(uk))
      assert(header(uk, "set-cookie").exists(_.startsWith("OKAYLANG=uk")), uk.headers.toString)
      assert(text(site.handle(Request.get("/", Seq("Cookie" -> "OKAYLANG=uk")))).contains("привіт (uk)"))
      assert(text(site.handle(Request.get("/", Seq("Accept-Language" -> "uk-UA,uk;q=0.9,en;q=0.8")))).contains("привіт (uk)"))
      assert(text(site.handle(Request.get("/", Seq("Accept-Language" -> "de-DE,de;q=0.9")))).contains("hello (en)"))
      assert(text(site.handle(Request.get("/?lang=de"))).contains("hello (en)"))
      assert(text(site.handle(Request.get("/plain?lang=uk"))).contains("plain (uk)"))
    }
  }

  test("an include's variant is used; a variant inherits secure: from its base") {
    withSite { (site, _) =>
      assert(text(site.handle(Request.get("/wrap"))).contains("[en header]"))
      assert(text(site.handle(Request.get("/wrap?lang=uk"))).contains("[uk header]"))
      assertEquals(site.handle(Request.get("/admin?lang=uk")).status, 401)
    }
  }

  test("t: a key in uk, a key only in en, a missing key, a placeholder; i18n/ is not routed") {
    withSite { (site, _) =>
      val uk = text(site.handle(Request.get("/?lang=uk")))
      assert(uk.contains("Привіт, Ann!") && uk.contains("en-only") && uk.contains(" nope"), uk)
      val en = text(site.handle(Request.get("/")))
      assert(en.contains("Hello, Ann!"), en)
      assert(!site.routes.isDefinedAt(Request.get("/i18n/uk.yaml")))
      assertEquals(site.handle(Request.get("/i18n/uk.yaml")).status, 404)
    }
  }

  test("acceptLanguage: q order and the primary subtag") {
    assertEquals(Site.acceptLanguage("en;q=0.5, uk", Vector("en", "uk")), Some("uk"))
    assertEquals(Site.acceptLanguage("fr, de", Vector("en", "uk")), None)
    assertEquals(Site.acceptLanguage("EN-GB", Vector("en")), Some("en"))
  }
