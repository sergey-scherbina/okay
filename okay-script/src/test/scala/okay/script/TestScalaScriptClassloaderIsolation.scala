package okay.script

/** okay-script-classloader-isolation: a script's URLClassLoader must
 * not silently resolve classes from okay-script's OWN host classpath
 * just because they happen to be reachable through the parent -- only
 * what the caller's Classpath actually lists (plus the JDK). See
 * specs/okay-script.md "Classloader isolation".
 */
class TestScalaScriptClassloaderIsolation extends munit.FunSuite:

  // just enough to compile+run trivial Scala -- deliberately NOT the
  // full ambient classpath, so munit (present on okay-script's own
  // test classpath) is absent from what the SCRIPT is given
  private def minimalScalaRuntime: Classpath =
    Classpath(Classpath.ambient.entries.filter { p =>
      val n = p.getFileName.toString
      n.startsWith("scala-library") || n.startsWith("scala3-library")
    })

  test("minimalScalaRuntime actually excludes munit (a sanity check on the test's own setup)") {
    val names = minimalScalaRuntime.entries.map(_.getFileName.toString)
    assert(names.nonEmpty, "no scala runtime jars found on ambient classpath")
    assert(!names.exists(_.contains("munit")), names.mkString(", "))
  }

  test("a script cannot reach a class on the host's classpath that is absent from its own Classpath") {
    val md =
      """```scala
        |val reachable =
        |  try { Class.forName("munit.Assertions"); true }
        |  catch { case _: ClassNotFoundException => false }
        |println("munit-reachable:" + reachable)
        |```
        |""".stripMargin
    val r = ScalaScript.run(md, classpath = minimalScalaRuntime)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("munit-reachable:false"), r.stdout)
  }

  test("the SAME script, given Classpath.ambient, DOES reach munit -- proving the isolation is about the Classpath given, not a blanket ban") {
    val md =
      """```scala
        |val reachable =
        |  try { Class.forName("munit.Assertions"); true }
        |  catch { case _: ClassNotFoundException => false }
        |println("munit-reachable:" + reachable)
        |```
        |""".stripMargin
    val r = ScalaScript.run(md, classpath = Classpath.ambient)
    assert(r.ok, r.errors.mkString("\n") + r.thrown.map(_.toString).getOrElse(""))
    assert(r.stdout.contains("munit-reachable:true"), r.stdout)
  }
