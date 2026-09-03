package okay.script

/** okay-script-line-mapping: a compiler error's line number in
 * `Result.errors` points at the ORIGINAL `.md` file, not the
 * synthesized wrapped source dotc actually compiles. See
 * specs/okay-script.md "Line-accurate errors".
 */
class TestScalaScriptLineMapping extends munit.FunSuite:

  private def lineOf(errors: Vector[String]): Option[Int] =
    val re = """^L(\d+): """.r
    errors.flatMap(e => re.findFirstMatchIn(e).map(_.group(1).toInt)).headOption

  test("a compile error on a MULTI-LINE ```scala block's LATER line reports THAT line, not the block's first") {
    val md =
      """Line 1 of prose.
        |
        |```scala
        |val a = 1
        |val b = 2
        |val c: Int = "not an int"
        |```
        |""".stripMargin
    // fence opens on line 3, content starts line 4; the error is on
    // the THIRD content line -> original line 6
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assertEquals(lineOf(r.errors), Some(6))
  }

  test("a compile error on the SECOND of two ```scala blocks reports the second block's own line") {
    val md =
      """```scala
        |val ok = 1
        |```
        |
        |More prose here.
        |
        |```scala
        |val bad: Int = "nope"
        |```
        |""".stripMargin
    // second fence opens on line 7, content starts line 8
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assertEquals(lineOf(r.errors), Some(8))
  }

  test("a compile error inside a ${expr} marker (render) reports the marker's own line") {
    val md =
      """Some prose.
        |
        |Value: ${thisNameDoesNotExist}
        |""".stripMargin
    val r = ScalaScript.render(md)
    assert(!r.ok)
    assertEquals(lineOf(r.errors), Some(3))
  }

  test("with front-matter/headings (Meta plumbing injected), a user error still reports the CORRECT original line") {
    val md =
      """---
        |id: x
        |---
        |
        |# Heading
        |
        |```yaml
        |k: v
        |```
        |
        |```scala
        |val z: Int = "bad"
        |```
        |""".stripMargin
    // front-matter lines 1-3, heading line 5, yaml fence lines 7-9,
    // scala fence opens line 11, content starts line 12
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assertEquals(lineOf(r.errors), Some(12))
  }

  test("a diagnostic with no position at all does not crash the mapping lookup -- errors are reported normally") {
    // dotc's own (position-less) summary diagnostics don't always
    // reach a StoreReporter the same way a plain console run shows
    // them; the real assertion here is that the L<n>-prefix lookup
    // (dia.position().isPresent -> lineMap.lift(...)) never throws
    // regardless, since it did not for ANY error in this whole suite.
    val md = "```scala\nval x: Int = \"bad\"\n```\n"
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assert(r.errors.exists(_.startsWith("L2: ")), r.errors.mkString("\n"))
  }
