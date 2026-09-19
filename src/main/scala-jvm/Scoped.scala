package okay

/** `ScopedValue`'s shape, over a `ThreadLocal` with no public `set`.
 * `where` binds `value` for `body`'s extent only and restores
 * whatever was bound before — nesting resolves to the nearest
 * `where` — however `body` exits, exception included. JVM-only
 * (`src/main/scala-jvm`, beside `Platform.scala`): `ThreadLocal` has
 * no Scala.js/Native equivalent. Originally built for okay-script's
 * per-request state (specs/script-scoped-state.md) — nothing about
 * it is script-specific, so it moved here (scoped-to-core) once a
 * second consumer made that obvious.
 *
 * THIS is the JDK-21-and-up baseline. On a JVM running JDK 25+, the
 * class actually loaded is a DIFFERENT one — `jdk25/Scoped.scala`
 * (repo root), backed by the real `java.lang.ScopedValue` (JEP 506)
 * — packaged into this jar's `META-INF/versions/25/` by
 * `scripts/build-mrjar-jdk25.sh` and `build.sbt`'s `okay` crossProject
 * `.jvmSettings` (Multi-Release JAR, JEP 238): the JVM picks the
 * class file, not a runtime branch here. Both variants keep the exact
 * same public shape (`current`, `where`, `backend`) on purpose — that
 * binary contract is what makes the swap invisible to every caller.
 * See specs/script-scoped-state-mrjar.md.
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
  private[okay] def backend: String = "ThreadLocal"

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
