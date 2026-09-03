package okay.script

/** `//> using dep` resolution shells out to the `cs`/`coursier` CLI and
 * hits the network on a cache miss -- tagged Live and out of the
 * default `sbt test` gate (integration-test-gate), same as every
 * other suite reaching outside the JVM.
 */
class TestScalaScriptDeps extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def toolPresent(): Boolean =
    Deps.resolve(Vector("org.scalameta:munit_3:1.1.1")) match
      case Deps.Resolved.ToolMissing => false
      case _ => true

  test("Deps.resolve: a real coordinate resolves to jars on disk") {
    assume(toolPresent(), "no cs/coursier on PATH -- skipped")
    Deps.resolve(Vector("org.scalameta:munit_3:1.1.1")) match
      case Deps.Resolved.Jars(jars) =>
        assert(jars.nonEmpty)
        assert(jars.exists(_.getFileName.toString.contains("munit")), jars.mkString("\n"))
        jars.foreach(j => assert(java.nio.file.Files.exists(j), j.toString))
      case other => fail(s"expected Jars, got $other")
  }

  // fansi is not a transitive dep of okay-script itself, so this
  // proves the resolved jar was actually added to the classpath --
  // not that the class happened to already be reachable.
  test("run: a script with `using dep` compiles and runs against the resolved jar") {
    assume(toolPresent(), "no cs/coursier on PATH -- skipped")
    val md =
      """```scala
        |//> using dep "com.lihaoyi:fansi_3:0.4.0"
        |val loaded = Class.forName("fansi.Str") != null
        |println("dep-resolved-ok:" + loaded)
        |```
        |""".stripMargin
    val r = ScalaScript.run(md)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("dep-resolved-ok:true"), r.stdout)
  }

