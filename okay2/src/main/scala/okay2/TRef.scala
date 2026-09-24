package okay2

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

/**
 * A TRANSACTIONAL CELL: one value behind one compare-and-set, carrying
 * a VERSION that moves on every change, and a list of WAITERS woken
 * when it does. `modify(f)` is the whole single-cell transaction — read,
 * compute, install, retry on a lost race. It is what okay-stm's `Stm`
 * builds on (a transaction over several cells commits by installing an
 * `Owned` marker in each, which a plain `modify` spins past and never
 * parks on), and on its own it is a lock-free cell with change
 * notification.
 *
 * Two representations, chosen at construction, not tested per value:
 * `TRef(init)` wraps each value in a fresh `Slot` (a version per
 * install); `TRef.bare(init)` stores a value that IS its own stamp — a
 * `Stamped` subclass — so an install allocates nothing, and a `modify`
 * that answers the SAME object changes nothing (no version bump, no
 * wake). The Scala 3 core measured the bare road for its STM's hot
 * cells.
 */
sealed abstract class TRef[A] {
  import TRef._

  private[okay2] def ref: AtomicReference[Stamped[A]]
  private[okay2] val waiters = new AtomicReference[List[Waiter]](Nil)
  private[okay2] def install(a: A, v: Long): Stamped[A]
  protected def unchanged(a: A, content: Stamped[A]): Boolean

  /** the current value */
  def get: A = ref.get.value

  /** how many changes this cell has seen */
  def version: Long = ref.get.stamp

  /** read, compute the new value and an answer, install; retried on a
   * lost race, so `f` must be pure. A commit installing (`Owned`) is
   * spun past, never parked on. */
  @tailrec final def modify[B](f: A => (A, B)): B =
    ref.get match {
      case _: Owned[_] => modify(f)
      case s =>
        val (a2, b) = f(s.value)
        if (unchanged(a2, s)) b
        else if (ref.compareAndSet(s, install(a2, s.stamp + 1))) { if (waiters.get ne Nil) wake(); b }
        else modify(f)
    }

  /** wake every waiter registered since the last wake, each once */
  @tailrec private[okay2] final def wake(): Unit = {
    val ws = waiters.get
    if (ws.nonEmpty) {
      if (waiters.compareAndSet(ws, Nil)) ws.reverse.foreach(_.fire())
      else wake()
    }
  }

  /** register a waiter, woken by the next change */
  @tailrec private[okay2] final def watch(w: Waiter): Unit = {
    val ws = waiters.get
    if (!waiters.compareAndSet(ws, w :: ws)) watch(w)
  }

  /** wake `k` once, at the next change of this cell */
  def onChange(k: () => Unit): Unit = watch(new Waiter(k))
}

object TRef {
  implicit val same: Same[TRef] = Same.byIdentity[TRef]

  def apply[A](init: A): TRef[A] = new Wrapped(init)

  /** a cell whose values are their own stamps: installs allocate nothing */
  def bare[A <: Stamped[A]](init: A): TRef[A] = new Bare(init)

  private final class Wrapped[A](init: A) extends TRef[A] {
    private[okay2] val ref = new AtomicReference[Stamped[A]](install(init, 0L))
    private[okay2] def install(a: A, v: Long): Stamped[A] = { val s = new Slot(a); s.stamp = v; s }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = false
  }

  private final class Bare[A <: Stamped[A]](init: A) extends TRef[A] {
    private[okay2] val ref = new AtomicReference[Stamped[A]](install(init, 0L))
    private[okay2] def install(a: A, v: Long): Stamped[A] = { a.stamp = v; a }
    protected def unchanged(a: A, content: Stamped[A]): Boolean = a eq content
  }

  /** a value with its version */
  abstract class Stamped[+A] {
    private[okay2] var stamp: Long = 0L
    def value: A
  }

  final class Slot[+A](val value: A) extends Stamped[A]

  /** a commit's claim on a cell: the content it will replace, and the
   * committing transaction's token */
  private[okay2] final class Owned[+A](val inner: Stamped[A], val token: AnyRef) extends Stamped[A] {
    stamp = inner.stamp
    def value: A = inner.value
  }

  /** a one-shot wake-up */
  final class Waiter(k: () => Unit) {
    private val fired = new AtomicBoolean(false)
    def fire(): Unit = if (fired.compareAndSet(false, true)) k()
  }
}

/**
 * The heterogeneous map with TYPED KEYS: a key `K[A]` holds a value of
 * type A, keys compare by `Same[K]` (identity, or value and tag), and
 * the witness `Same` hands over is what types a lookup — no cast in the
 * map. Iteration is typed too: `foreach` sees each value at its key's
 * type.
 */
final class TMap[K[_]] private (private val stack: List[TMap.Entry[K, _]]) {
  import TMap.Entry

  def get[A](k: K[A])(implicit s: Same[K]): Option[A] = {
    def at[X](e: Entry[K, X]): Option[A] = s.same(e.key, k).map(ev => ev(e.value))
    stack.iterator.map(e => at(e)).collectFirst { case Some(v) => v }
  }

  def contains[A](k: K[A])(implicit s: Same[K]): Boolean = get(k).isDefined

  def updated[A](k: K[A], v: A)(implicit s: Same[K]): TMap[K] = {
    val e = Entry(k, v)
    def isK[X](x: Entry[K, X]): Boolean = s.same(x.key, k).isDefined
    if (contains(k)) new TMap(stack.map(x => if (isK(x)) e else x))
    else new TMap(e :: stack)
  }

  def isEmpty: Boolean = stack.isEmpty
  def nonEmpty: Boolean = stack.nonEmpty
  def size: Int = stack.length

  /** the entries, in insertion order */
  def entries: Iterator[Entry[K, _]] = stack.reverseIterator

  /** each value at its key's type, in insertion order */
  def foreach(f: TMap.Each[K]): Unit = {
    def one[A](e: Entry[K, A]): Unit = f(e.key, e.value)
    stack.reverseIterator.foreach(e => one(e))
  }

  /** each value at its key's type, in NO particular order — a commit
   * installing its writes does not care, and skips the reverse */
  def foreachUnordered(f: TMap.Each[K]): Unit = {
    def one[A](e: Entry[K, A]): Unit = f(e.key, e.value)
    var s = stack
    while (s.nonEmpty) { one(s.head); s = s.tail }
  }

  override def toString: String = {
    def show[A](e: Entry[K, A]): String = s"${e.key} -> ${e.value}"
    stack.reverseIterator.map(e => show(e)).mkString("TMap(", ", ", ")")
  }
}

object TMap {
  final case class Entry[K[_], A](key: K[A], value: A)

  /** what `foreach` calls: a function polymorphic in the entry's type */
  trait Each[K[_]] { def apply[A](k: K[A], v: A): Unit }

  def empty[K[_]]: TMap[K] = new TMap[K](Nil)
}

/** a concurrent map over one `TRef`: every operation one `modify` */
final class TDict[K, A](init: Map[K, A] = Map.empty[K, A]) {
  private val ref = TRef(init)
  def get(k: K): Option[A] = ref.get.get(k)
  def contains(k: K): Boolean = ref.get.contains(k)
  def put(k: K, v: A): Unit = ref.modify(m => (m.updated(k, v), ()))
  def remove(k: K): Unit = ref.modify(m => (m.removed(k), ()))
  def computeIfAbsent(k: K)(mk: => A): A =
    ref.modify { m =>
      m.get(k) match {
        case Some(v) => (m, v)
        case None => val v = mk; (m.updated(k, v), v)
      }
    }
  def updateAt(k: K)(f: Option[A] => A): A =
    ref.modify { m =>
      val v = f(m.get(k))
      (m.updated(k, v), v)
    }
  def snapshot: Map[K, A] = ref.get
  def size: Int = ref.get.size
  def isEmpty: Boolean = ref.get.isEmpty
  def clear(): Unit = ref.modify(_ => (Map.empty[K, A], ()))
}

object TDict {
  def empty[K, A]: TDict[K, A] = new TDict[K, A]()
}

/** a concurrent append-only list over one `TRef` */
final class TList[A](init: Vector[A] = Vector.empty[A]) {
  private val ref = TRef(init)
  def append(a: A): Unit = ref.modify(v => (v :+ a, ()))
  def snapshot: Vector[A] = ref.get
  def size: Int = ref.get.size
  def isEmpty: Boolean = ref.get.isEmpty
  def clear(): Unit = ref.modify(_ => (Vector.empty[A], ()))
}

object TList {
  def empty[A]: TList[A] = new TList[A]()
}
