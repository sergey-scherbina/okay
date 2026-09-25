package okay

/** Which backend `okay.Scoped` actually loaded (script-scoped-state-mrjar,
 * closed by mrjar-jdk25-ci-gap) -- reflection-only, so JVM-only
 * (Scala.js/Native have no `java.lang.reflect.Method`). This suite runs
 * against okay-platform's PACKAGED jar (build.sbt `multiRelease`), so
 * the JVM it forks on picks the class per JEP 238: `ScopedValue` from
 * `META-INF/versions/25/` on JDK 25+, the `ThreadLocal` root class
 * below (`verifyJdk17`). The first test is what makes the second one
 * mean anything: a class loaded from a classes directory is never
 * versioned, so a green "ThreadLocal" on 26 would be the classpath
 * being wrong, not the variant.
 */
class TestScopedBackend extends munit.FunSuite:

  private def backend: String =
    val m = classOf[Scoped[?]].getDeclaredMethod("backend")
    m.setAccessible(true)
    m.invoke(Scoped(())).asInstanceOf[String] // declared `: String`

  test("Scoped is loaded from a jar, not a classes directory") {
    val where = classOf[Scoped[?]].getProtectionDomain.getCodeSource.getLocation.getPath
    assert(where.endsWith(".jar"), s"loaded from $where")
  }

  test("the backend is the one this JDK must have picked") {
    val expected = if Runtime.version().feature() >= 25 then "ScopedValue" else "ThreadLocal"
    // printed on purpose: a green run on the wrong JVM would look like
    // this one, and the gate log is where the JDK it ran on is read
    println(s"TestScopedBackend: JDK ${Runtime.version()} loaded the $backend backend")
    assertEquals(backend, expected, s"on JDK ${Runtime.version()}")
  }
