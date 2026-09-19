package okay.script.api

/** `ScopedValue`'s shape, ported to where `java.lang.ScopedValue` is
 * still preview (JDK 21 here; JEP 506 finalizes it only in JDK 25):
 * a `ThreadLocal` with no public `set`. `where` binds `value` for
 * `body`'s extent only and restores whatever was bound before —
 * nesting resolves to the nearest `where` — however `body` exits,
 * exception included. See specs/script-scoped-state.md.
 */
final class Scoped[A] private (default: () => A):
  private val local: ThreadLocal[A] = ThreadLocal.withInitial(() => default())

  def current: A = local.get()

  def where[B](value: A)(body: => B): B =
    val prior = local.get()
    local.set(value)
    try body finally local.set(prior)

object Scoped:
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
