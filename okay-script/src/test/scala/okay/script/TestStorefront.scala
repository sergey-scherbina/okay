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
    val site = Site(root)
    try f(site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def get(site: Site, url: String): (Int, String) =
    val r = site.handle(Request.get(url))
    (r.status, Async.run[String, Pure](Http.text(r)).runWith)

  test("the storefront renders: a domain TYPE, an i18n seam, prices and cards, all across module boundaries") {
    withSite { site =>
      val (status, body) = get(site, "/")
      assertEquals(status, 200, body)
      // the type crossed a module boundary, and so did its companion
      assert(body.contains("<h1>Szykownia</h1>"), body)
      assert(body.contains("""<a class="offer" href="/s/szykownia/offer/hem">"""), body)
      assert(body.contains("35.00 zł") && body.contains("60.00 zł"), body)
      assert(body.contains("wycena"), "a zero price is a quote, not 0.00")
      assert(body.contains("""data-pl="Skrócenie spodni""""), body)
      assert(body.contains("""data-uk="Wymiana zamka""""), body)
      assert(body.contains("--accent:#9e1042") && body.contains("--void:#dcc4b6"), body)
      assert(!body.contains("""<span class="offer-desc"></span>"""), body)
    }
  }

  test("the same modules render the OTHER site: one library, two storefronts") {
    withSite { site =>
      val (status, body) = get(site, "/it")
      assertEquals(status, 200, body)
      assert(body.contains("--accent:#3b82f6") && body.contains("--void:#05070c"), body)
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
