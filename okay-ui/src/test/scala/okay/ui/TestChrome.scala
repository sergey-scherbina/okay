package okay.ui

/**
 * app-host (specs/app-host.md): the frame chosen per request — the same
 * page under the app's sidebar or the site's strip, the notices above it
 * either way, and the document around it.
 */
class TestChrome extends munit.FunSuite {

  private val shell = Shell("App", Vector(Shell.Group("", Vector(Shell.Item("Trace", "/ui/trace"), Shell.Item("Check", "/ui/check")))))
  private val notices = Vector("""<p class="update">A new version</p>""", """<p class="lease">Open elsewhere</p>""")
  private val strip = """<div class="nav"><b>App</b> <a href="/ui/trace">trace</a></div>"""

  test("the notices sit above the page under both frames; the sidebar only in the app, the strip only on the site") {
    val app = Chrome.html(Chrome(shell, app = true, notices), "/ui/trace", "<h1>Trace</h1>", strip)
    assert(app.contains("""<nav class="okay-side">""") && !app.contains("""<div class="nav">"""), app)
    assert(app.contains("""<main class="okay-main"><p class="update">A new version</p><p class="lease">Open elsewhere</p><h1>Trace</h1></main>"""), app)
    assert(app.contains("""<a class="okay-place okay-here" href="/ui/trace">"""), "the reader is here")
    val site = Chrome.html(Chrome(shell, app = false, notices), "/ui/trace", "<h1>Trace</h1>", strip)
    assertEquals(site, strip + notices.mkString + "<h1>Trace</h1>")
  }

  test("the document: a refresh fetches in the app and reloads on the site, none at 0; the face and the ways only in the app") {
    val app = Chrome.document(Chrome(shell, app = true), "/ui/trace", "<p>x</p>", "App <1>", head = "<style>.tool{}</style>", refresh = 5, script = "<script>live()</script>")
    assert(app.startsWith("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\""), app)
    assert(app.contains("<title>App &lt;1&gt;</title>"))
    assert(app.contains("""<meta name="okay-refresh" content="5">""") && !app.contains("http-equiv"))
    assert(app.contains("<style>.tool{}</style>") && app.contains(Chrome.css) && app.contains(Shell.css) && app.contains(Enhance.css))
    assert(app.contains(Enhance.script) && app.endsWith("<script>live()</script></body></html>"))
    val site = Chrome.document(Chrome(shell, app = false), "/ui/trace", "<p>x</p>", "App", refresh = 5)
    assert(site.contains("""<meta http-equiv="refresh" content="5">""") && !site.contains("okay-refresh"))
    assert(!site.contains(Enhance.script) && !site.contains(Shell.css))
    val none = Chrome.document(Chrome(shell, app = true), "/", "", "App")
    assert(!none.contains("<meta name=\"okay-refresh\"") && !none.contains("http-equiv"), "no refresh at 0")
  }

  test("every class the app's face styles is one the frame, the ways or the tree's own sheet write") {
    val styled = """\.okay-[a-z-]+""".r.findAllIn(Chrome.css).map(_.drop(1)).toSet
    val written = (Shell.html(shell, "/", "") + Enhance.script + Enhance.css + Shell.css + Html.css).linesIterator
      .flatMap(l => """okay-[a-z-]+""".r.findAllIn(l)).toSet
    val orphans = styled -- written
    assert(orphans.isEmpty, s"styled but never written: $orphans")
  }
}
