package okay.ui

/**
 * ui-app (specs/ui-app.md): an application's frame and ways for the
 * HTML host — the frame marks where the reader is and carries the page
 * untouched; the script is a typed program a JavaScript engine parses,
 * hooked to the names the frame writes.
 */
class TestAppShell extends munit.FunSuite {

  private val shell = Shell("okay-watch",
    Vector(
      Shell.Group("", Vector(Shell.Item("Trace", "/ui/trace", "↗"), Shell.Item("Check", "/ui/check", "✓"))),
      Shell.Group("Monitoring", Vector(Shell.Item("Collect", "/"), Shell.Item("<Analysis>", "/ui/cases")))),
    Vector(Shell.Item("Account", "/ui/account")))

  test("the frame marks the current place, titles a group that has one, escapes labels, carries the body verbatim") {
    val page = Shell.html(shell, "/ui/check", """<h1>Before you pay</h1><form data-x="1"></form>""")
    assert(page.contains("""<a class="okay-place okay-here" href="/ui/check">"""), page)
    assertEquals("okay-here".r.findAllIn(page).size, 1)
    assert(page.contains("""<div class="okay-group">Monitoring</div>"""))
    assertEquals("okay-group".r.findAllIn(page).size, 1, "an untitled group has no heading")
    assert(page.contains("&lt;Analysis&gt;") && !page.contains("<Analysis>"))
    assert(page.contains("""<main class="okay-main"><h1>Before you pay</h1><form data-x="1"></form></main>"""))
    assert(page.indexOf("Account") > page.indexOf("okay-grow"), "the footer sits at the bottom")
  }

  test("`/` is current only at `/`; a place is current at a page under it") {
    assert(Shell.current("/", "/") && !Shell.current("/", "/ui/trace"))
    assert(Shell.current("/ui/trace", "/ui/trace/20260924-100000-000"))
    assert(Shell.current("/ui/trace", "/ui/trace?paid=1"))
    assert(!Shell.current("/ui/trace", "/ui/traces"))
  }

  test("every class the frame and the script write has a rule") {
    val written = (Shell.html(shell.copy(brand = "App"), "/", "") + Enhance.script).linesIterator.flatMap { l =>
      """okay-[a-z-]+""".r.findAllIn(l)
    }.toSet -- Set("okay-refresh", "okay-live-", "okay-main", "okay-plain") // names the script reads, not classes it styles
    val css = Shell.css + Enhance.css
    val missing = written.filterNot(c => css.contains("." + c))
    assert(missing.isEmpty, s"no rule for: ${missing.mkString(", ")}")
  }

  test("the script is typed — no escape hatch — and hooked to the frame's names") {
    assertEquals(okay.js.Js.raws(Enhance.program), 0)
    val js = Enhance.script
    for hook <- Vector("main.okay-main", "data-hard", "okay-refresh", "okay-busy", "okay-live-", "okay-plain", "okay-pick") do
      assert(js.contains(hook), hook)
  }

  test("the script is a program a JavaScript engine parses") {
    val node = Vector("/opt/homebrew/bin/node", "/usr/local/bin/node", "/usr/bin/node")
      .find(p => java.nio.file.Files.isExecutable(java.nio.file.Path.of(p)))
    assume(node.isDefined, "no node on this machine")
    val f = java.nio.file.Files.createTempFile("okay-enhance", ".js")
    java.nio.file.Files.writeString(f, Enhance.script)
    val p = ProcessBuilder(node.get, "--check", f.toString).redirectErrorStream(true).start()
    val out = new String(p.getInputStream.readAllBytes(), "UTF-8")
    assertEquals(p.waitFor(), 0, out)
  }
}
