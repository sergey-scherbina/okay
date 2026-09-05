package okay.demo.e2e

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.demo.ChatDemo
import com.microsoft.playwright.{Browser, Playwright}
import com.microsoft.playwright.options.LoadState

/**
 * specs/demo-chat.md, "A browser-level proof" (demo-e2e-browser):
 * one real chat round, driven through a REAL headless browser, over
 * the SAME server every other test boots (Jetty, scripted model, a
 * random port) — proving Main.scala's fetch+ReadableStream glue in a
 * real JS engine, not just the JVM unit test's string-splitting.
 *
 * Kept in its own module, out of the root aggregate (build.sbt) —
 * Playwright's browser download is a real cost this suite alone
 * pays. Run it explicitly: sbt "okayChatWebJS/fastLinkJS"
 * "okayDemoE2eBrowser/test".
 */
class TestChatBrowser extends munit.FunSuite {
  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  val deadWire: okay.llm.Transport = (url, _, _) =>
    throw new AssertionError(s"offline test touched the wire: $url")
  val noSecrets: okay.conf.Secrets = okay.conf.Secrets.memory(Map.empty)

  def withServer[A](f: Int => A): A =
    provide(deadWire, noSecrets, okay.demo.Board(okay.demo.Board.topicOf(okay.demo.Board.store(":memory:"))))(
      Resource.run[A, Pure](
        Jetty.serve(0)(ChatDemo.routes(okay.chat.Chat.scripted, 512))()
          .map(s => f(Jetty.port(s)))).runWith)

  /** the bundle must be linked (sbt okayChatWebJS/fastLinkJS) — a
   * missing one is not this test's failure, it's an unmet
   * precondition, the same distinction liveTest draws elsewhere */
  def withBrowser(f: Browser => Any): Unit =
    assume(okay.chat.Chat.appJs.isDefined,
      "no linked React bundle (sbt okayChatWebJS/fastLinkJS) — skipped")
    try
      val pw = Playwright.create()
      try
        val browser = pw.chromium().launch(
          new com.microsoft.playwright.BrowserType.LaunchOptions().setHeadless(true))
        try f(browser): Unit finally browser.close()
      finally pw.close()
    catch case e: Throwable =>
      assume(false, s"Playwright's browser isn't installed here (${e.getMessage}) — skipped")

  test("one chat round through a real browser: typed text sends, the scripted reply streams in") {
    withServer { port =>
      withBrowser { browser =>
        val page = browser.newPage()
        page.navigate(s"http://127.0.0.1:$port/")
        page.waitForLoadState(LoadState.NETWORKIDLE)
        page.fill("input[data-key='draft']", "hello okay")
        page.click("button[data-key='send']")
        // the user bubble appears immediately (no streaming needed)
        page.waitForFunction(
          "() => document.querySelector('[data-key=log]').textContent.includes('you: hello okay')")
        // the scripted model echoes — wait for the streamed reply
        page.waitForFunction(
          "() => document.querySelector('[data-key=log]').textContent.includes('hello') " +
            "&& document.querySelector('[data-key=log]').textContent.includes('okay')",
          null, new com.microsoft.playwright.Page.WaitForFunctionOptions().setTimeout(15000))
        val log = page.textContent("[data-key=log]")
        assert(log.contains("you: hello okay"), log)
        assert(log.toLowerCase.contains("bot:"), log)
      }
    }
  }
}
