package okay.demo.e2e

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.script.Site
import com.microsoft.playwright.{Browser, BrowserContext, Page, Playwright}
import java.nio.file.{Files, Path}

/**
 * specs/frontend.md "Mobile", M1: a Live page as an installable,
 * mobile-first application, driven through a REAL headless browser in
 * an iPhone emulation (390x844, touch, 3x) — the tap lands, the
 * server's patch lands, the manifest and the worker are served, the
 * controls are tap-sized, and the page opens OFFLINE from the shell
 * the worker kept.
 */
class TestMobileWeb extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(90, "s")

  def withSite[A](f: (Int, Site) => A): A =
    val root = Files.createTempDirectory("okay-mobile-")
    Files.writeString(root.resolve("counter.md"),
      """```scala declare
        |import okay.ui.*
        |import okay.script.api.*
        |val counter = Live(0)(n => Ui.Column(Vector(Ui.Text(s"count: $n"), Ui.Button("+1", "inc", Role.Primary))))(
        |  (n, e) => e match { case Event.Pressed("inc") => n + 1; case _ => n })
        |```
        |<!doctype html><html><head><title>counter</title>${installable("Counter")}</head><body>
        |${mount("counter", counter)}
        |</body></html>
        |""".stripMargin): Unit
    val site = Site(root)
    try
      Resource.run[A, Pure](
        Jetty.serve(0)(site.routes)(site.ws).map(s => f(Jetty.port(s), site))).runWith
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  def withPhone(f: (BrowserContext, Page) => Any): Unit =
    try
      val pw = Playwright.create()
      try
        val browser = pw.chromium().launch(new com.microsoft.playwright.BrowserType.LaunchOptions().setHeadless(true))
        try
          val ctx = browser.newContext(new Browser.NewContextOptions()
            .setViewportSize(390, 844).setIsMobile(true).setHasTouch(true).setDeviceScaleFactor(3)
            .setUserAgent("Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"))
          ctx.setDefaultTimeout(20000)   // a hang is a failure with a message, not a 90 s silence
          try f(ctx, ctx.newPage()): Unit finally ctx.close()
        finally browser.close()
      finally pw.close()
    catch case e: com.microsoft.playwright.PlaywrightException =>
      assume(false, s"Playwright's browser isn't installed here (${e.getMessage.take(120)}) — skipped")

  test("an iPhone: the tap lands, the patch lands, the controls are tap-sized, manifest and worker served") {
    withSite { (port, _) =>
      withPhone { (_, page) =>
        page.navigate(s"http://127.0.0.1:$port/counter")
        assertEquals(page.locator("meta[name=viewport]").count(), 1)
        assert(page.locator("link[rel=manifest]").count() == 1)
        val box = page.locator("button[data-key=inc]").boundingBox()
        assert(box.height >= 44, s"a tap target of ${box.height}px")
        page.tap("button[data-key=inc]")
        page.waitForCondition(() => page.textContent("#okay-live-counter").contains("count: 1"),
          new Page.WaitForConditionOptions().setTimeout(20000))
        // the manifest names the page as its start, the worker and the icon are served
        val manifest = page.request().get(s"http://127.0.0.1:$port/__okay/manifest.webmanifest?name=Counter&start=/counter")
        assertEquals(manifest.status(), 200)
        assert(manifest.text().contains("\"start_url\": \"/counter\""), manifest.text())
        assertEquals(page.request().get(s"http://127.0.0.1:$port/__okay/sw.js").status(), 200)
        assertEquals(page.request().get(s"http://127.0.0.1:$port/__okay/icon.svg").status(), 200)
        // the worker is registered and active on this origin
        val active = page.evaluate("() => navigator.serviceWorker.ready.then(r => !!r.active)")
        assertEquals(String.valueOf(active), "true")
      }
    }
  }

  test("offline: the page opens from the shell the worker kept, whole, without a socket") {
    withSite { (port, _) =>
      withPhone { (ctx, page) =>
        page.navigate(s"http://127.0.0.1:$port/counter")
        assertEquals(String.valueOf(page.evaluate("() => navigator.serviceWorker.ready.then(r => !!r.active)")), "true")
        page.tap("button[data-key=inc]")
        page.waitForCondition(() => page.textContent("#okay-live-counter").contains("count: 1"),
          new Page.WaitForConditionOptions().setTimeout(20000))
        ctx.setOffline(true)
        page.reload(new Page.ReloadOptions().setWaitUntil(com.microsoft.playwright.options.WaitUntilState.DOMCONTENTLOADED))
        // the SSR'd tree the browser last fetched — count: 0 was the page as served
        assert(page.textContent("#okay-live-counter").contains("count:"), page.content().take(300))
        assertEquals(page.locator("button[data-key=inc]").count(), 1)
        assertEquals(String.valueOf(page.evaluate("() => !!document.querySelector('link[rel=stylesheet]')")), "true")
      }
    }
  }
}
