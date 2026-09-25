package okay

/** The JDK 25+ variant of `okay.Scoped` — see
 * `okay-platform/src/main/scala/Scoped.scala` for the baseline this
 * replaces, only on a JVM that has it, and
 * specs/script-scoped-state-mrjar.md for how it gets there
 * (Multi-Release JAR, JEP 238). Compiled by the build like any other
 * source, as project `okayPlatformJdk25` (build.sbt `versioned`, with
 * `-java-output-version 25`), and packaged into okay-platform's jar
 * under `META-INF/versions/25/` on every build; `TestScopedBackend`
 * proves the swap on the JDK the tests run on.
 *
 * `ScopedValue.where(key, value).call(op)` (JEP 506) IS `where`: it
 * binds for `op`'s extent, restores on every exit including an
 * exception, and nests correctly (nearest-wins) — this is not an
 * adaptation of the JDK21 shape, it is what that shape was ported
 * FROM. The one seam: raw `ScopedValue.get()` throws when unbound;
 * `current` covers that with `isBound()` so a read outside any
 * `where` still answers its default, exactly like the ThreadLocal
 * path.
 *
 * Public shape — constructor, `current`, `where`, `backend`, the
 * companion `apply` — is IDENTICAL to the baseline's, verified by
 * `javap` (specs/script-scoped-state-mrjar.md): that binary contract
 * is what makes this swap invisible to every caller, compiled
 * exactly once against the baseline.
 */
final class Scoped[A] private (default: () => A):
  private val sv: ScopedValue[A] = ScopedValue.newInstance[A]()

  def current: A = if sv.isBound() then sv.get() else default()

  def where[B](value: A)(body: => B): B =
    ScopedValue.where(sv, value).call(() => body)

  private[okay] def backend: String = "ScopedValue"

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
