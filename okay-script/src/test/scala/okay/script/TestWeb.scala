package okay.script

import java.nio.file.Files

/** okay-script-web: request-object injection, the remaining half of
 * "a new JSP" -- a plain, dependency-free `Web` value, no `okay.http`
 * import anywhere in `okay-script`'s own code. See
 * specs/okay-script.md "Request context".
 */
class TestWeb extends munit.FunSuite:

  test("a script reads Web.current -- method/path/query/headers set by the caller") {
    val md =
      """```scala
        |import okay.script.Web
        |val w = Web.current
        |println("method=" + w.method)
        |println("path=" + w.path)
        |println("q=" + w.query.getOrElse("id", "?"))
        |println("h=" + w.headers.getOrElse("X-Trace", "?"))
        |```
        |""".stripMargin
    val web = Web("POST", "/orders", query = Map("id" -> "42"), headers = Map("X-Trace" -> "abc"))
    val r = ScalaScript.render(md, web = web)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("method=POST"), r.stdout)
    assert(r.stdout.contains("path=/orders"), r.stdout)
    assert(r.stdout.contains("q=42"), r.stdout)
    assert(r.stdout.contains("h=abc"), r.stdout)
  }

  test("${...} in prose can read Web.current directly, not just a ```scala block") {
    val md = "Path: ${okay.script.Web.current.path}\n"
    val r = ScalaScript.render(md, web = Web("GET", "/pricing"))
    assert(r.ok, r.errors.mkString("\n"))
    assert(r.stdout.contains("Path: /pricing"), r.stdout)
  }

  test("Page.render(webA) then Page.render(webB) on the SAME Page reflects the right Web each time") {
    val f = Files.createTempFile("okay-script-web-", ".md")
    Files.writeString(
      f,
      """```scala
        |import okay.script.Web
        |```
        |
        |Path: ${Web.current.path}
        |""".stripMargin,
    )
    val page = Page(f)
    try
      val r1 = page.render(Web("GET", "/a"))
      assert(r1.ok, r1.errors.mkString("\n"))
      assert(r1.stdout.contains("Path: /a"), r1.stdout)

      val r2 = page.render(Web("GET", "/b"))
      assert(r2.ok, r2.errors.mkString("\n"))
      assert(r2.stdout.contains("Path: /b"), r2.stdout)
      assert(!r2.stdout.contains("/a"), r2.stdout)
    finally
      page.close()
      Files.deleteIfExists(f): Unit
  }

  test("omitting web (or a script that never reads it) does not break run/render") {
    val md = "```scala\nprintln(\"no web needed\")\n```\n"
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n"))
    assert(r.stdout.contains("no web needed"), r.stdout)
  }
