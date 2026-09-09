package okay.demo.e2e

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.script.Site
import com.microsoft.playwright.{Browser, Playwright}
import java.nio.file.Files

/** the mobile-web steps, one at a time, with a line after each — for
 * a box where the suite went silent: `sbt okayDemoE2eBrowser/Test/runMain okay.demo.e2e.MobileProbe` */
object MobileProbe:
  def main(args: Array[String]): Unit =
    def say(s: String): Unit = { println(s"[probe ${System.currentTimeMillis() % 100000}] $s"); System.out.flush() }
    val root = Files.createTempDirectory("okay-mobile-probe-")
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
    say("page written")
    val site = Site(root)
    Resource.run[Unit, Pure](Jetty.serve(0)(site.routes)(site.ws).map { s =>
      val port = Jetty.port(s)
      say(s"jetty on $port")
      val pw = Playwright.create()
      say("playwright created")
      val browser = pw.chromium().launch(new com.microsoft.playwright.BrowserType.LaunchOptions().setHeadless(true))
      say("chromium launched")
      val ctx = browser.newContext(new Browser.NewContextOptions().setViewportSize(390, 844).setIsMobile(true).setHasTouch(true))
      ctx.setDefaultTimeout(20000)
      val page = ctx.newPage()
      page.navigate(s"http://127.0.0.1:$port/counter")
      say("navigated: " + page.title())
      say("viewport metas: " + page.locator("meta[name=viewport]").count())
      page.tap("button[data-key=inc]")
      say("tapped")
      page.waitForCondition(() => page.textContent("#okay-live-counter").contains("count: 1"),
        new com.microsoft.playwright.Page.WaitForConditionOptions().setTimeout(15000))
      say("patched: " + page.textContent("#okay-live-counter"))
      say("sw register: " + page.evaluate("""() => Promise.race([
        navigator.serviceWorker.register('/__okay/sw.js').then(r => 'ok scope=' + r.scope + ' state=' + ((r.installing||r.waiting||r.active)||{}).state).catch(e => 'err: ' + e.name + ': ' + e.message),
        new Promise(res => setTimeout(() => res('timeout'), 8000))])"""))
      say("sw controller: " + page.evaluate("() => navigator.serviceWorker.controller ? 'yes' : 'none'"))
      say("sw.js fetch: " + page.evaluate("() => fetch('/__okay/sw.js').then(r => r.status + ' ' + r.headers.get('content-type'))"))
      say("secure context: " + page.evaluate("() => window.isSecureContext"))
      ctx.setOffline(true)
      page.reload(new com.microsoft.playwright.Page.ReloadOptions().setWaitUntil(com.microsoft.playwright.options.WaitUntilState.DOMCONTENTLOADED))
      say("offline reload: " + page.textContent("#okay-live-counter"))
      browser.close(); pw.close()
      say("done")
    }).runWith
    site.close()
    System.exit(0)
