package okay.script

class TestScalaScriptRender extends munit.FunSuite:

  test("render: a preceding code block's val is substituted via ${...} in prose") {
    val md =
      """```scala
        |val name = "okay"
        |```
        |
        |Hello, ${name}!
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assertEquals(r.stdout, "\nHello, okay!")
  }

  test("render: a document with no ${...} at all passes through verbatim") {
    val md =
      """# Title
        |
        |Just prose, no interpolation.
        |
        |```yaml
        |a: 1
        |```
        |
        |More prose.
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n"))
    assertEquals(r.stdout, md.stripLineEnd)
  }

  test("render: $${ escapes to a literal ${, alongside a real ${...} elsewhere in the same document") {
    val md =
      """```scala
        |val x = 42
        |```
        |
        |A literal $${marker} and a real value ${x}.
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n"))
    assert(r.stdout.contains("A literal ${marker} and a real value 42."), r.stdout)
  }

  test("render: an expr with braces (if/else) and a nested real string interpolation both render correctly") {
    val md =
      """```scala
        |val n = 3
        |case class Item(name: String)
        |val items = Vector(Item("a"), Item("b"))
        |```
        |
        |Parity: ${ if (n % 2 == 0) "even" else "odd" }
        |Names: ${items.map(it => s"<${it.name}>").mkString(", ")}
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("Parity: odd"), r.stdout)
    assert(r.stdout.contains("Names: <a>, <b>"), r.stdout)
  }

  test("render: literal text ending in a quote, and text containing a \"\"\" run, both render via the escaped fallback") {
    val md =
      "```scala\n" +
      "val v = 1\n" +
      "```\n" +
      "\n" +
      "She said \"hi\"${v}\n" +
      "A literal \"\"\" triple quote run.\n"
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("She said \"hi\"1"), r.stdout)
    assert(r.stdout.contains("A literal \"\"\" triple quote run."), r.stdout)
  }

  test("render: an undefined name in ${...} is a compile error via errors, never thrown out of render itself") {
    val md = "Value: ${thisNameDoesNotExist}\n"
    val r = ScalaScript.render(md)
    assert(!r.ok)
    assert(r.errors.nonEmpty, r.errors.mkString("\n"))
    assertEquals(r.thrown, None)
  }

  test("render: an interleaved println from a code block stays in true document order, not flushed after the rendered text") {
    val md =
      """Before.
        |
        |```scala
        |println("SIDE-EFFECT")
        |```
        |
        |After.
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(r.ok, r.errors.mkString("\n"))
    val beforeIdx = r.stdout.indexOf("Before.")
    val sideIdx = r.stdout.indexOf("SIDE-EFFECT")
    val afterIdx = r.stdout.indexOf("After.")
    assert(beforeIdx >= 0 && sideIdx > beforeIdx && afterIdx > sideIdx, r.stdout)
  }

  test("run is unaffected by render's existence: prose text is still ignored, only ```scala blocks run") {
    val md =
      """Some prose with ${notAScalaBlock} that run must ignore entirely.
        |
        |```scala
        |println("run-only")
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n"))
    assertEquals(r.stdout.trim, "run-only")
  }
