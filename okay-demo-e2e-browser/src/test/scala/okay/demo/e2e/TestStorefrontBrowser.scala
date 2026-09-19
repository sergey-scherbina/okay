package okay.demo.e2e

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.script.Site
import com.microsoft.playwright.{Browser, Page, Playwright}

import java.nio.file.{Files, Path}

/**
 * specs/site-framework.md stage 4, in a REAL browser: the storefront's
 * own JavaScript works.
 *
 * The page renders one language server-side and carries the rest on
 * the element; a visitor switching language must see the text change
 * with no round trip, and the intake form must reach the server and
 * answer. Neither is provable by asserting on a string — a `contains`
 * suite cannot tell a script that runs from one that throws on line
 * one. This drives it.
 */
class TestStorefrontBrowser extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  /** the fixture pages, copied so a test's edits cannot touch the
   * checked-in ones */
  private def withSite[A](f: (Int, Site) => A): A =
    val src = Path.of(getClass.getResource("/storefront/index.md").toURI).getParent
    val root = Files.createTempDirectory("okay-storefront-browser-")
    Files.walk(src).forEach { p =>
      val target = root.resolve(src.relativize(p).toString)
      if Files.isDirectory(p) then Files.createDirectories(target): Unit
      else Files.copy(p, target): Unit
    }
    val site = Site(root, languages = Vector("pl", "en", "uk", "ru"))
    try
      Resource.run[A, Pure](Jetty.serve(0)(site.routes)().map(s => f(Jetty.port(s), site))).runWith
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def withBrowser(f: Page => Any): Unit =
    try
      val pw = Playwright.create()
      try
        val browser = pw.chromium().launch(new com.microsoft.playwright.BrowserType.LaunchOptions().setHeadless(true))
        try
          val ctx = browser.newContext(new Browser.NewContextOptions().setViewportSize(1100, 900))
          ctx.setDefaultTimeout(20000)
          try f(ctx.newPage()): Unit finally ctx.close()
        finally browser.close()
      finally pw.close()
    // ONLY a missing browser is a skip. A timeout is a PlaywrightException
    // too, and catching the class swallowed a real failure as "not
    // installed" — 21 seconds of waiting reported as a skip.
    catch case e: com.microsoft.playwright.PlaywrightException
      if e.getMessage != null && (e.getMessage.contains("Executable doesn't exist") ||
        e.getMessage.contains("playwright install")) =>
      assume(false, s"Playwright's browser isn't installed here — skipped")

  test("the language switch RUNS: the text changes with no round trip, and the choice survives a reload") {
    withSite { (port, _) =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/?lang=pl")
        assertEquals(page.textContent("h1.headline").trim, "Przeróbki odzieży kurierem")
        // no navigation happens: the same document, the text swapped
        val before = page.evaluate("() => performance.getEntriesByType('navigation').length")
        page.click("button[data-l=uk]")
        page.waitForCondition(() => page.textContent("h1.headline").trim == "Переробки одягу кур'єром")
        assertEquals(page.evaluate("() => performance.getEntriesByType('navigation').length"), before)
        // the button marks itself, and the document says which language it is in
        assertEquals(page.getAttribute("button[data-l=uk]", "class"), "on")
        assertEquals(page.evaluate("() => document.documentElement.lang"), "uk")
        // ...and the choice is remembered, by the same cookie the server reads
        page.navigate(s"http://127.0.0.1:$port/")
        page.waitForCondition(() => page.textContent("h1.headline").trim == "Переробки одягу кур'єром")
      }
    }
  }

  test("a placeholder is swapped too, being an attribute rather than a text node") {
    withSite { (port, _) =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/?lang=pl")
        // the field lives in the sheet, which is in the document from
        // the start and merely hidden -- so the swap reaches it before
        // anyone opens it
        assertEquals(page.getAttribute("#f-contact", "placeholder"), "e-mail albo telefon")
        // the switcher is UNDER the modal once it opens (the scrim
        // takes the clicks, which is what a modal is for), so the
        // language is chosen first and the sheet opened after
        page.click("button[data-l=en]")
        page.waitForCondition(() => page.getAttribute("#f-contact", "placeholder") == "email or phone")
        page.click("#buy")
        assert(page.isVisible("#buyModal .sheet"))
        assertEquals(page.getAttribute("#f-contact", "placeholder"), "email or phone")
      }
    }
  }

  test("the intake form RUNS: the modal opens, the post reaches the server, the answer is shown") {
    withSite { (port, _) =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/?lang=pl")
        assert(!page.isVisible("#buyModal .sheet"), "the modal was open before it was asked for")
        page.click("#buy")
        assert(page.isVisible("#buyModal .sheet"), "the modal did not open")
        page.fill("#f-name", "Anna")
        page.fill("#f-contact", "anna@example.com")
        page.fill("#f-what", "skrócić spodnie o 4 cm")
        page.click("#buyForm button[type=submit]")
        // the form is replaced by the acknowledgement the SERVER's
        // reference came back in
        page.waitForCondition(() => page.isVisible("#buyDone"))
        val ref = page.textContent("#buyRef").trim
        assert(ref.startsWith("R-"), s"no reference came back: '$ref'")
        assert(!page.isVisible("#buyForm"), "the form is still showing after a send")
      }
    }
  }

  test("the typed offer screen: the card leads to it, the form refuses without consent and takes the order with it") {
    withSite { (port, _) =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/?lang=pl")
        page.click("a.offer[href='/offer/hem']")
        page.waitForCondition(() => page.url().endsWith("/offer/hem"))
        // the form is the Schema's: a Select per sum, a checkbox per flag
        assertEquals(page.locator("select[name='delivery.$case']").count(), 1)
        page.fill("textarea[name=need], input[name=need]", "wymienić zamek")
        page.fill("input[name=name]", "Anna")
        page.fill("input[name=contact]", "anna@example.com")
        // sent WITHOUT consent: the page comes back with the reason
        page.click("button[type=submit]")
        page.waitForCondition(() => page.content().contains("bez zgody"))
        assert(!page.content().contains("Dziękuję"), "an order without consent was taken")
        // now with it
        page.fill("textarea[name=need], input[name=need]", "wymienić zamek")
        page.fill("input[name=name]", "Anna")
        page.fill("input[name=contact]", "anna@example.com")
        page.check("input[name=consent]")
        page.click("button[type=submit]")
        page.waitForCondition(() => page.content().contains("Dziękuję"))
        assert(page.textContent(".done").contains("R-"), page.textContent(".done"))
      }
    }
  }

  test("the theme is really applied: the atelier is warm, the IT line is dark, from ONE library") {
    withSite { (port, _) =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/")
        val warm = page.evaluate("() => getComputedStyle(document.body).backgroundColor")
        assertEquals(String.valueOf(warm), "rgb(220, 196, 182)")
        // the serif headline and the offer rows are styled, not bare
        assert(String.valueOf(page.evaluate("() => getComputedStyle(document.querySelector('h1.headline')).fontFamily"))
          .contains("serif"), "the headline is not the serif the theme asks for")
        page.navigate(s"http://127.0.0.1:$port/it")
        assertEquals(String.valueOf(page.evaluate("() => getComputedStyle(document.body).backgroundColor")),
          "rgb(5, 7, 12)")
      }
    }
  }
}
