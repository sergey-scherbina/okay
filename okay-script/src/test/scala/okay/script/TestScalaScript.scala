package okay.script

class TestScalaScript extends munit.FunSuite:

  test("blocks: extracts fenced ```scala regions in order, with 1-based startLine, skipping other languages") {
    val md =
      """# Title
        |
        |```yaml
        |not: scala
        |```
        |
        |```scala
        |val x = 1
        |```
        |
        |prose
        |
        |```scala
        |val y = 2
        |```
        |""".stripMargin
    val bs = ScalaScript.blocks(md)
    assertEquals(bs.map(_.code), Vector("val x = 1", "val y = 2"))
    assertEquals(bs(0).startLine, 8)
    assertEquals(bs(1).startLine, 14)
  }

  test("run: a single block that prints, succeeds with the printed output captured") {
    val md =
      """```scala
        |println("hello okay-script")
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n"))
    assert(r.stdout.contains("hello okay-script"), r.stdout)
    assertEquals(r.errors, Vector.empty)
    assertEquals(r.thrown, None)
  }

  test("run: two blocks, the second referencing the first's val, share one compilation unit") {
    val md =
      """```scala
        |val greeting = "hi"
        |```
        |
        |```scala
        |println(greeting + " again")
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n"))
    assert(r.stdout.contains("hi again"), r.stdout)
  }

  test("run: a compile error is reported in errors, never thrown out of run") {
    val md =
      """```scala
        |val x: Int = "not an int"
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assert(r.errors.nonEmpty)
    assertEquals(r.thrown, None)
  }

  test("run: a block that compiles but throws at runtime is reported via thrown, not errors") {
    val md =
      """```scala
        |throw new RuntimeException("boom")
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(!r.ok)
    assertEquals(r.errors, Vector.empty)
    assert(r.thrown.isDefined)
    assert(r.thrown.get.getMessage.contains("boom"))
  }

  test("run: a markdown file with no scala blocks trivially succeeds") {
    val md = "# Just prose\n\nNo code here.\n"
    val r = ScalaScript.run(md)
    assert(r.ok)
    assertEquals(r.stdout, "")
  }

  test("Deps.declared: extracts `using dep` coordinates from scala blocks, deduplicated") {
    val md =
      """```scala
        |//> using dep "org.scalameta::munit:1.1.1"
        |println(1)
        |```
        |
        |```scala
        |//> using dep "org.scalameta::munit:1.1.1"
        |//> using dep "com.example::other:2.0"
        |```
        |""".stripMargin
    assertEquals(
      Deps.declared(md),
      Vector("org.scalameta::munit:1.1.1", "com.example::other:2.0"),
    )
  }

  test("run: an explicit classpath overrides ambient -- an empty one fails to find the scala runtime") {
    val md = "```scala\nprintln(1)\n```\n"
    val r = ScalaScript.run(md, classpath = Classpath(Vector.empty))
    assert(!r.ok)
    assert(r.errors.nonEmpty, r.errors.mkString("\n"))
  }

  /**
   * OUT OF THE DEFAULT GATE (operator call, 2026-09-04,
   * specs/integration-test-gate.md). This snapshots a SHARED namespace
   * — every `okay-script-*` path under the system temp directory — so
   * it fails whenever another suite in this module creates or removes
   * one between the two snapshots, which is exactly what a parallel
   * matrix does. Observed in a full run of 2199 tests as the only
   * failure, with `okay-script-web-*.md` and a second `okay-script-<n>`
   * in the diff, none of them the file under test; the suite passes
   * 9/9 alone.
   *
   * Tagged rather than deleted because the property is real and worth
   * checking; the fix is to scope the snapshot to the paths this test
   * creates, and that belongs to whoever owns okay-script
   * (script-temp-snapshot-crosstalk).
   */
  test("run: leaves no temp file/directory behind, success or failure".tag(new munit.Tag("Live"))) {
    import scala.jdk.CollectionConverters.*
    val tmp = java.nio.file.Paths.get(System.getProperty("java.io.tmpdir"))
    def snapshot(): Set[String] =
      java.nio.file.Files.list(tmp).iterator().asScala
        .filter(_.getFileName.toString.startsWith("okay-script-"))
        .map(_.toString)
        .toSet
    val before = snapshot()
    ScalaScript.run("```scala\nprintln(1)\n```\n"): Unit
    ScalaScript.run("```scala\nval x: Int = \"bad\"\n```\n"): Unit
    ScalaScript.run("```scala\nthrow new RuntimeException(\"x\")\n```\n"): Unit
    val after = snapshot()
    assertEquals(after, before)
  }
