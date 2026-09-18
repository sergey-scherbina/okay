package okay.demo.e2e

import okay.*
import okay.given
import okay.jetty.Jetty
import okay.script.Site
import com.microsoft.playwright.{Browser, BrowserType, Page, Playwright}
import java.nio.file.{Files, Path}

/**
 * ui-browser-vocab, in a REAL browser (specs/ui-product.md stage 1).
 *
 * The lane proved itself on a fake document and on rendered strings,
 * which is where the laws live. What neither can say is whether the
 * page a browser actually builds from `live.js` is a table — the
 * script is 240 lines of hand-written JavaScript that no Scala test
 * executes, and its hello is what decides whether the server sends a
 * `Table` at all. So this drives Chromium against a real `Live` page:
 * the tree arrives over the socket, the browser builds it, and a
 * patch INSIDE the table has to land in the right cell.
 */
class TestTableBrowser extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(90, "s")

  /** a Live page whose view is a Table: pressing `add` appends a row,
   * pressing `edit` changes one CELL — the narrow case */
  private def withSite[A](f: Int => A): A =
    val root = Files.createTempDirectory("okay-table-")
    Files.writeString(root.resolve("table.md"),
      """```scala declare
        |import okay.ui.*
        |import okay.script.api.*
        |val rows = Live(0)(n => Ui.Column(Vector(
        |  Ui.Table(Vector("id", "note"),
        |    (0 to n).toVector.map(i => Vector(
        |      Ui.Text(s"c-$i", Style(kind = Kind.Ident)),
        |      Ui.Text(if i == n then s"note $n" else s"note $i"))),
        |    "cases", Vector(1, 9)),
        |  Ui.Button("add", "add", Role.Primary))))(
        |  (n, e) => e match { case Event.Pressed("add") => n + 1; case _ => n })
        |```
        |<!doctype html><html><head><title>table</title></head><body>
        |${mount("table", rows)}
        |</body></html>
        |""".stripMargin): Unit
    val site = Site(root)
    try
      Resource.run[A, Pure](
        Jetty.serve(0)(site.routes)(site.ws).map(s => f(Jetty.port(s)))).runWith
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def withBrowser(f: Page => Any): Unit =
    try
      val pw = Playwright.create()
      try
        val browser = pw.chromium().launch(new BrowserType.LaunchOptions().setHeadless(true))
        try
          val ctx = browser.newContext(new Browser.NewContextOptions())
          ctx.setDefaultTimeout(20000)
          try f(ctx.newPage()): Unit finally ctx.close()
        finally browser.close()
      finally pw.close()
    catch case e: com.microsoft.playwright.PlaywrightException =>
      assume(false, s"Playwright's browser isn't installed here (${e.getMessage.take(120)}) — skipped")

  test("the browser builds a REAL table from the socket's tree, and a patch lands in a cell") {
    withSite { port =>
      withBrowser { page =>
        page.navigate(s"http://127.0.0.1:$port/table")
        // the scriptless render is already a table — the page is whole
        // before any script runs
        assertEquals(page.locator("table[data-key=cases] thead th").count(), 2)
        assertEquals(page.locator("table[data-key=cases] colgroup col").count(), 2)
        assertEquals(page.locator("table[data-key=cases] tbody tr").count(), 1)
        // a header a reader — or a screen reader — can see
        assertEquals(page.textContent("table[data-key=cases] thead th:nth-child(1)"), "id")
        // the shares are said once, in the element whose job that is
        val width = page.getAttribute("table[data-key=cases] colgroup col:nth-child(2)", "style")
        assertEquals(width, "width:90%")
        // and what a cell IS travels with the cell
        assertEquals(page.locator("td span.okay-kind-ident").count(), 1)

        // now the socket: the live client said `table` in its hello, so
        // the server sent a Table rather than boxes, and `live.js`
        // built THIS. Pressing add makes the server send a patch.
        page.click("button[data-key=add]")
        page.waitForCondition(() => page.locator("table[data-key=cases] tbody tr").count() == 2,
          new Page.WaitForConditionOptions().setTimeout(20000))
        assertEquals(page.locator("table[data-key=cases] thead th").count(), 2)
        assertEquals(page.textContent("table[data-key=cases] tbody tr:nth-child(2) td:nth-child(1)"), "c-1")
        // the first row's note was rewritten by the same press: the
        // patch reached a CELL, not just the table's shape
        assertEquals(page.textContent("table[data-key=cases] tbody tr:nth-child(1) td:nth-child(2)"), "note 0")
        assertEquals(page.textContent("table[data-key=cases] tbody tr:nth-child(2) td:nth-child(2)"), "note 1")
      }
    }
  }
}
