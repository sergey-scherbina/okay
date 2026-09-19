package okay.script.api

/** `ScopedValue`'s shape, over a `ThreadLocal` with no public `set`.
 * `where` binds `value` for `body`'s extent only and restores
 * whatever was bound before — nesting resolves to the nearest
 * `where` — however `body` exits, exception included.
 *
 * THIS is the JDK-21-and-up baseline. On a JVM running JDK 25+, the
 * class actually loaded is a DIFFERENT one — `okay-script/jdk25/
 * Scoped.scala`, backed by the real `java.lang.ScopedValue` (JEP 506)
 * — packaged into this jar's `META-INF/versions/25/` by
 * `scripts/build-mrjar-jdk25.sh` and `build.sbt`'s `okayScript`
 * settings (Multi-Release JAR, JEP 238): the JVM picks the class
 * file, not a runtime branch here. Both variants keep the exact same
 * public shape (`current`, `where`, `backend`) on purpose — that
 * binary contract is what makes the swap invisible to every one of
 * this package's 13 call sites. See specs/script-scoped-state.md and
 * specs/script-scoped-state-mrjar.md.
 */
final class Scoped[A] private (default: () => A):
  private val local: ThreadLocal[A] = ThreadLocal.withInitial(() => default())

  def current: A = local.get()

  def where[B](value: A)(body: => B): B =
    val prior = local.get()
    local.set(value)
    try body finally local.set(prior)

  /** which backend is actually loaded — a diagnostic, not part of
   * the design; exists only so a test can PROVE the Multi-Release
   * swap happened rather than trust it by inspection. */
  private[script] def backend: String = "ThreadLocal"

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
