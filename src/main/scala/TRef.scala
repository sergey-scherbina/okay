package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec


/**
 * The cell of the STM (specs/stm.md): a value with a version, in one
 * AtomicReference, plus a one-shot waiter list for `retry`. Its
 * `modify` IS the single-cell transaction — one CAS — and the path
 * every handler takes for a transaction that is one `Modify`; the
 * Channel's state lives in one of these.
 *
 * The reference holds ONE type, `Stamped[A]`: the value itself when
 * it carries its own version (a BARE cell — the Channel's State, so
 * its fast path allocates nothing beyond the state it would build
 * anyway), a `Slot` wrapping any other value (a WRAPPED cell), or an
 * `Owned` marker that a commit in flight has CAS'd over the content
 * and that mirrors the content's stamp and value. Which of bare or
 * wrapped a cell is, is decided at construction — `TRef(init)` wraps,
 * `TRef.bare(init)` needs `A <: Stamped[A]` — so nothing about a
 * value is ever guessed at runtime and nothing is cast. Owned is
 * matched only where its meaning differs: the fast path retries on
 * it, a transactional read aborts on it, another commit fails its
 * CAS on it. Nothing waits.
 */
sealed abstract class TRef[A] {
  import TRef.*

  private[okay] def ref: AtomicReference[Stamped[A]]
  private[okay] val waiters = AtomicReference[List[Waiter]](Nil)

  /** stamp and shape a value for this cell — bare or in a Slot, the
   * cell's kind */
  private[okay] def install(a: A, v: Long): Stamped[A]

  /** is the answer the content itself — "nothing changed", no CAS?
   * Only a bare cell can say yes */
  protected def unchanged(a: A, content: Stamped[A]): Boolean

  /** a plain read, outside any transaction */
  def get: A = ref.get.value

  /** the cell's version: moves by one at every install */
  def version: Long = ref.get.stamp

  /** the one-cell transaction: f is PURE and may run more than once;
   * the answer b is yours to act on after — the Channel returns its
   * callbacks this way. A bare cell whose answer IS the content skips
   * the CAS; a wrapped value always installs, an equal one included (a
   * version bump and a spurious wake-up, both harmless). Content owned
   * by a commit in progress is retried (a few instructions long, never
   * a park) */
  @tailrec final def modify[B](f: A => (A, B)): B =
    ref.get match
      case _: Owned[?] => modify(f)   // a commit is installing; spin, never park
      case s =>
        val (a2, b) = f(s.value)
        if unchanged(a2, s) then b
        else if ref.compareAndSet(s, install(a2, s.stamp + 1)) then { if waiters.get ne Nil then wake(); b }
        else modify(f)

  @tailrec private[okay] final def wake(): Unit =
    val ws = waiters.get
    if ws.nonEmpty then
      if waiters.compareAndSet(ws, Nil) then ws.reverse.foreach(_.fire())
      else wake()

  @tailrec private[okay] final def watch(w: Waiter): Unit =
    val ws = waiters.get
    if !waiters.compareAndSet(ws, w :: ws) then watch(w)
}

object TRef {
  /** a cell is its own typed token: the same cell holds the same type */
  given Same[TRef] = Same.byIdentity

  /** a cell for any value: the value travels in a Slot */
  def apply[A](init: A): TRef[A] = Wrapped(init)

  /** a cell for a value that carries its own version (`extends
   * TRef.Stamped[Self] { def value = this }`): installed bare, no
   * wrapper ever built — the Channel's kind */
  def bare[A <: Stamped[A]](init: A): TRef[A] = Bare(init)

  private final class Wrapped[A](init: A) extends TRef[A]:
    private[okay] val ref = AtomicReference[Stamped[A]](install(init, 0L))
    // `new`, not `Slot(a)`: on the JVM there is also a package-private
    // okay.Slot (Platform.scala's parking cell), and a constructor
    // proxy for the inner class shadowing it is an ERROR (E177) — one
    // that only appears when this file is recompiled, so it hides from
    // an incremental build and fails a clean one.
    private[okay] def install(a: A, v: Long): Stamped[A] = { val s = new Slot(a); s.stamp = v; s }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = false

  private final class Bare[A <: Stamped[A]](init: A) extends TRef[A]:
    private[okay] val ref = AtomicReference[Stamped[A]](install(init, 0L))
    private[okay] def install(a: A, v: Long): Stamped[A] = { a.stamp = v; a }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = a eq content

  /** what a cell holds: a value of A that carries its own version.
   * Extend it — `extends TRef.Stamped[Self] { def value = this }` —
   * and a bare cell installs your value as is; a wrapped cell puts
   * any value in a Slot, which is a Stamped too. So the cell's content
   * is always a Stamped[A], typed end to end. A bare cell STAMPS at
   * install: such a value belongs to one cell and one install — build
   * a new one for every transition (an immutable case class does,
   * through copy). A class, not a trait: the type test on the fast
   * path is a primary-supers check, not an interface scan (measured,
   * half the gap of the first cut) */
  abstract class Stamped[+A] {
    private[okay] var stamp: Long = 0L
    /** the value the cell holds: yourself, unless you are a wrapper */
    def value: A
  }

  /** the wrapper a wrapped cell puts its values in */
  final class Slot[+A](val value: A) extends Stamped[A]

  /** a commit in flight owns the content it found; it IS a Stamped[A]
   * — the content's stamp and value, seen through it — so the cell
   * has one type and only the places where ownership MEANS something
   * match on it */
  private[okay] final class Owned[+A](val inner: Stamped[A], val token: AnyRef) extends Stamped[A]:
    stamp = inner.stamp
    def value: A = inner.value

  /** fires at most once, however many cells it watches */
  final class Waiter(k: () => Unit):
    private val fired = AtomicBoolean(false)
    def fire(): Unit = if fired.compareAndSet(false, true) then k()
}
