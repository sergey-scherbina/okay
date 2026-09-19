package okay.script.api

/** The JDK 25+ variant of `okay.script.api.Scoped` — see
 * `okay-script/src/main/scala/okay/script/api/Scoped.scala` for the
 * JDK21-and-up baseline this replaces, only on a JVM that has it,
 * and specs/script-scoped-state-mrjar.md for how it gets there
 * (Multi-Release JAR, JEP 238). NOT compiled by okayScript's own
 * build — see scripts/build-mrjar-jdk25.sh — so this file needs no
 * JDK 25 on a machine that isn't building this variant.
 *
 * `ScopedValue.where(key, value).call(op)` (JEP 506) IS `where`: it
 * binds for `op`'s extent, restores on every exit including an
 * exception, and nests correctly (nearest-wins) — this is not an
 * adaptation of the JDK21 shape, it is what that shape was ported
 * FROM. The one seam: raw `ScopedValue.get()` throws when unbound;
 * `current` covers that with `isBound()` so a `Web.current` read
 * outside any `where` still answers its default, exactly like the
 * ThreadLocal path.
 *
 * Public shape — constructor, `current`, `where`, `backend`, the
 * companion `apply` — is IDENTICAL to the baseline's, verified by
 * `javap` (specs/script-scoped-state-mrjar.md): that binary contract
 * is what makes this swap invisible to every one of this package's
 * 13 call sites, compiled exactly once against the baseline.
 */
final class Scoped[A] private (default: () => A):
  private val sv: ScopedValue[A] = ScopedValue.newInstance[A]()

  def current: A = if sv.isBound() then sv.get() else default()

  def where[B](value: A)(body: => B): B =
    ScopedValue.where(sv, value).call(() => body)

  private[script] def backend: String = "ScopedValue"

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
