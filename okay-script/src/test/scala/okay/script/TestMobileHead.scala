package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}
import java.nio.file.{Files, Path}

/** the mobile head renders inside a page and the mobile files are served (ui-mobile) */
class TestMobileHead extends munit.FunSuite:
  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  test("installable() renders in a page's head; the four mobile files are served") {
    val root = Files.createTempDirectory("okay-mobile-head-")
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
      val page = site.handle(Request.get("/counter"))
      val body = text(page)
      assertEquals(page.status, 200, body.take(2000))
      assert(body.contains("name=\"viewport\""), body.take(2000))
      assert(body.contains("/__okay/manifest.webmanifest?name=Counter&start=%2Fcounter"), body.take(2000))
      assert(body.contains("okayLive(\"counter\")"), body.take(2000))
      for p <- Seq(Mobile.CssPath, Mobile.SwPath, Mobile.IconPath, Mobile.ManifestPath + "?name=x&start=/y") do
        assertEquals(site.handle(Request.get(p)).status, 200, p)
      val m = text(site.handle(Request.get(Mobile.ManifestPath + "?name=x&start=/y")))
      assert(m.contains("\"start_url\": \"/y\""), m)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
