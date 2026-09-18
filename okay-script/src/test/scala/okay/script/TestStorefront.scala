package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request}

import java.nio.file.{Files, Path}

/**
 * specs/site-framework.md stage 4: a real slice of the storefront the
 * operator's two sites are rendered from (busi's
 * `src/v2/http/storefront.ssc`) written as okay-script pages.
 *
 * This suite is the arc's VERDICT. The pages are real files under
 * `src/test/resources/storefront` rather than string literals, so a
 * reader can open them and so the fixture is what an author would
 * actually write: five library pages under `lib/`, none of them
 * routed, and two storefronts that import them.
 */
class TestStorefront extends munit.FunSuite:

  private def fixture: Path =
    Path.of(getClass.getResource("/storefront/index.md").toURI).getParent

  private def withSite[A](f: Site => A): A =
    val src = fixture
    val root = Files.createTempDirectory("okay-script-storefront-")
    Files.walk(src).forEach { p =>
      val target = root.resolve(src.relativize(p).toString)
      if Files.isDirectory(p) then Files.createDirectories(target): Unit
      else Files.copy(p, target): Unit
    }
    // the sites speak four languages, as the real ones do
    val site = Site(root, languages = Vector("pl", "en", "uk", "ru"))
    try f(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def get(site: Site, url: String): (Int, String) =
    val r = site.handle(Request.get(url))
    (r.status, Async.run[String, Pure](Http.text(r)).runWith)

  test("the storefront renders: a domain TYPE, an i18n seam, prices and cards, all across module boundaries") {
    withSite { site =>
      val (status, body) = get(site, "/?lang=pl")
      assertEquals(status, 200, body)
      // the type crossed a module boundary, and so did its companion;
      // the title is one translated element: EVERY language on it, and
      // the request's own as its text (stage 3)
      assert(body.contains("""data-pl="Przeróbki odzieży kurierem""""), body)
      assert(body.contains("""data-uk="Переробки одягу кур'єром""""), body)
      assert(body.contains(""">Przeróbki odzieży kurierem</h1>"""), s"?lang=pl must render Polish:\n$body")
      assert(body.contains("""<a class="offer" href="/s/szykownia/offer/hem">"""), body)
      assert(body.contains("35.00 zł") && body.contains("60.00 zł"), body)
      assert(body.contains("wycena"), "a zero price is a quote, not 0.00")
      assert(body.contains("""data-pl="Skrócenie spodni""""), body)
      assert(body.contains("""data-uk="Wymiana zamka""""), body)
      assert(body.contains("--accent: #9e1042") && body.contains("--void: #dcc4b6"), body)
      assert(!body.contains("""<span class="offer-desc"></span>"""), body)
      // and the client that switches them is on the page, with the
      // same cookie the per-request road reads
      assert(body.contains("window.setLang") && body.contains("OKAYLANG"), body)
      assert(body.contains("""onclick="setLang('uk')""""), body)
    }
  }

  test("the two i18n roads compose: the request picks the TEXT, the element still carries the rest") {
    withSite { site =>
      val (_, uk) = get(site, "/?lang=uk")
      assert(uk.contains(""">Переробки одягу кур'єром</h1>"""), s"the request's language must be the rendered text:\n$uk")
      assert(uk.contains("""data-pl="Przeróbki odzieży kurierem""""), uk)
      val (_, en) = get(site, "/?lang=en")
      assert(en.contains(""">Alterations, by courier</h1>"""), en)
      // ...and the script's default follows the request, so it does
      // not undo what the server just decided
      assert(en.contains("dflt='en'"), en)
      assert(uk.contains("dflt='uk'"), uk)
    }
  }

  test("the same modules render the OTHER site: one library, two storefronts") {
    withSite { site =>
      val (status, body) = get(site, "/it")
      assertEquals(status, 200, body)
      assert(body.contains("--accent: #3b82f6") && body.contains("--void: #05070c"), body)
      assert(body.contains("""href="/s/it/offer/cicd""""), body)
      assert(body.contains("wycena"), body)
    }
  }

  test("the library answers no URL, and the site warms with every page compiling") {
    withSite { site =>
      assertEquals(get(site, "/lib/cards")._1, 404)
      assertEquals(get(site, "/lib/domain")._1, 404)
      val broken = site.warm().filter(_._2.nonEmpty)
      assert(broken.isEmpty, broken.map((p, e) => s"$p: ${e.mkString("; ")}").mkString("\n"))
    }
  }

  test("the storefront's words are CONTENT: the editor writes them and the page changes") {
    withSite { site =>
      // what ships
      assert(get(site, "/?lang=pl")._2.contains("Skrócenie spodni"), get(site, "/?lang=pl")._2)
      // the editor renders a form from the same Schema the page renders from
      val form = get(site, "/edit")._2
      assert(form.contains("""<form method="post" action="/edit">"""), form)
      assert(form.contains("""name="services[0].name""""), form)
      assert(form.contains("""value="Skrócenie spodni""""), form)
      // ...and a post writes the content the storefront then shows
      val saved = post(site, "/edit", Map(
        "id" -> "site-szykownia", "slug" -> "szykownia", "title" -> "Szykownia",
        "clothing" -> "on", "accent" -> "#123456",
        "services[0].key" -> "hem", "services[0].name" -> "Podszycie spodni",
        "services[0].description" -> "ekspres, 24h", "services[0].priceCents" -> "4200",
        "services[0].siteId" -> "site-szykownia"))
      assert(saved.contains("saved 1 services"), saved)
      val after = get(site, "/?lang=pl")._2
      assert(after.contains("Podszycie spodni") && after.contains("42.00 zł"), after)
      assert(after.contains("--accent: #123456"), after)
      assert(!after.contains("Skrócenie spodni"), "the shipped default is still showing")
      // and a reset puts back exactly what shipped
      assert(post(site, "/edit", Map("__reset" -> "1")).contains("reset to what shipped"))
      assert(get(site, "/?lang=pl")._2.contains("Skrócenie spodni"), get(site, "/?lang=pl")._2)
    }
  }

  private def post(site: Site, url: String, fields: Map[String, String]): String =
    val body = fields.map((k, v) =>
      s"${java.net.URLEncoder.encode(k, "UTF-8")}=${java.net.URLEncoder.encode(v, "UTF-8")}").mkString("&")
    val r = site.handle(Request.post(url, okay.http.Body.Text(body),
      Seq(("content-type", "application/x-www-form-urlencoded"))))
    Async.run[String, Pure](Http.text(r)).runWith
