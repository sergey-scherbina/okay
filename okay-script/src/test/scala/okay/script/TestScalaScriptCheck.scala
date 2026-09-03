package okay.script

/** okay-script-check: mdoc-style output-comparison literate testing --
 * a block's expected stdout written inline in the markdown (a
 * ```stdout fence), checked against what a real `run` actually
 * printed. See specs/okay-script.md "Output-comparison testing".
 */
class TestScalaScriptCheck extends munit.FunSuite:

  test("check: matching ```stdout content succeeds, no mismatches") {
    val md =
      """```scala
        |println("hello")
        |```
        |
        |```stdout
        |hello
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(r.ok, r.mismatches.mkString("\n"))
    assertEquals(r.mismatches, Vector.empty)
  }

  test("check: two ```stdout fences, both correct and in order, succeeds") {
    val md =
      """```scala
        |println("first")
        |```
        |
        |```stdout
        |first
        |```
        |
        |```scala
        |println("second")
        |```
        |
        |```stdout
        |second
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(r.ok, r.mismatches.mkString("\n"))
  }

  test("check: a mismatched SECOND fence fails and names it, the first fence's match does not mask it") {
    val md =
      """```scala
        |println("first")
        |println("second")
        |```
        |
        |```stdout
        |first
        |```
        |
        |```stdout
        |NOT-WHAT-WAS-PRINTED
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(!r.ok)
    assertEquals(r.mismatches.length, 1)
    assert(r.mismatches.head.contains("#2"), r.mismatches.head)
    assert(r.mismatches.head.contains("NOT-WHAT-WAS-PRINTED"), r.mismatches.head)
  }

  test("check: reports ALL mismatches, not just the first") {
    val md =
      """```scala
        |println("only-this")
        |```
        |
        |```stdout
        |wrong-one
        |```
        |
        |```stdout
        |wrong-two
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(!r.ok)
    assertEquals(r.mismatches.length, 2)
  }

  test("check: an out-of-order match (second expectation only found BEFORE the first's match) is a mismatch") {
    val md =
      """```scala
        |println("BBB AAA")
        |```
        |
        |```stdout
        |AAA
        |```
        |
        |```stdout
        |BBB
        |```
        |""".stripMargin
    // "AAA" matches at position 4; the search for "BBB" then starts
    // AFTER that match, but "BBB" only occurs BEFORE it -- no match
    val r = ScalaScript.check(md)
    assert(!r.ok)
    assert(r.mismatches.exists(_.contains("#2")), r.mismatches.mkString("\n"))
  }

  test("check: a run that fails to compile fails immediately with one mismatch, no substring search attempted") {
    val md =
      """```scala
        |val x: Int = "not an int"
        |```
        |
        |```stdout
        |anything
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(!r.ok)
    assertEquals(r.mismatches.length, 1)
    assert(r.mismatches.head.contains("run failed"), r.mismatches.head)
    assert(!r.run.ok)
  }

  test("check: leading/trailing whitespace in ```stdout is ignored, internal content differences are not") {
    val md =
      """```scala
        |println("exact")
        |```
        |
        |```stdout
        |
        |  exact
        |
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(r.ok, r.mismatches.mkString("\n"))
  }

  test("check: a ```scala/```yaml fence is not mistaken for ```stdout, and vice versa") {
    val md =
      """```yaml
        |k: v
        |```
        |
        |```scala
        |println("real output")
        |```
        |
        |```stdout
        |real output
        |```
        |""".stripMargin
    val r = ScalaScript.check(md)
    assert(r.ok, r.mismatches.mkString("\n"))
  }
