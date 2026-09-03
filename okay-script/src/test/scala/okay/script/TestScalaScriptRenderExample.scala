package okay.script

/** okay-script-interpolation's worked example: render-storefront.md,
 * the "JSP but Scala+Markdown" case named directly by the operator --
 * prose with Scala values dropped straight in via `${expr}`, including
 * a NESTED real string interpolation inside one marker's own
 * expression (a realistic case, not a contrived one).
 */
class TestScalaScriptRenderExample extends munit.FunSuite:

  test("render-storefront.md renders the price list with values substituted") {
    val candidates = Vector(
      "examples/render-storefront.md",
      "okay-script/examples/render-storefront.md",
    ).map(java.nio.file.Paths.get(_))
    val file = candidates.find(java.nio.file.Files.exists(_))
      .getOrElse(fail(s"missing example file, tried: ${candidates.mkString(", ")}"))
    val md = java.nio.file.Files.readString(file)

    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("Всего услуг в прайсе: 3."), r.stdout)
    assert(r.stdout.contains("Самая дорогая: Добавление новых функций и развитие (4500.00 PLN)."), r.stdout)
    assert(r.stdout.contains("- Консультация — 350.00 PLN"), r.stdout)
  }
