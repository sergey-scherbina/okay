package okay

/** Which backend `okay.Scoped` actually loaded (script-scoped-state-mrjar)
 * -- reflection-only, so JVM-only (Scala.js/Native have no
 * `java.lang.reflect.Method`). This box's own JDK is the baseline
 * (ThreadLocal); the ScopedValue swap on JDK 25+ is proven by the
 * manual probe in specs/script-scoped-state-mrjar.md, not by a test
 * here -- the gate itself only ever runs on this box's own JDK.
 */
class TestScopedBackend extends munit.FunSuite:

  test("this JVM's build loads the ThreadLocal backend") {
    val backend = classOf[Scoped[?]].getDeclaredMethod("backend")
    backend.setAccessible(true)
    assertEquals(backend.invoke(Scoped(())), "ThreadLocal")
  }
