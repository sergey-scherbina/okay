package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

/**
 * Sameness of TYPED tokens, with the type equality it implies: for a
 * key constructor K, `same(a: K[A], b: K[B])` answers whether a and b
 * are one and the same key — and when they are, hands over the
 * witness `A =:= B`, because one key holds one type. This is the
 * typeclass behind every heterogeneous structure keyed by tokens
 * (TMap first): the structure applies the witness and never casts.
 *
 * Scala 3's own equality is `CanEqual[L, R]` — under
 * `strictEquality`, `==` between two types compiles only with that
 * evidence, but the evidence proves nothing about the types. `Same`
 * is the proof-bearing sibling: from a `Same[K]` a `CanEqual[K[A],
 * K[B]]` follows (two keys of one constructor may always be asked
 * "the same?"), so token keys compare with `==` in strict mode too.
 */
trait Same[K[_]]:
  def same[A, B](a: K[A], b: K[B]): Option[A =:= B]

object Same:
  def apply[K[_]](using s: Same[K]): Same[K] = s

  /** the axiom for reference tokens: identity. The one place a
   * `=:=` is CLAIMED rather than derived — a typed token that IS
   * another typed token has that token's type — stated once, as a
   * witness, for every key type that opts in */
  def byIdentity[K[X] <: AnyRef]: Same[K] = new Same[K]:
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if a eq b then Some(summon[A =:= A].asInstanceOf[A =:= B]) else None

  /** the axiom for VALUE keys — a typed id over a primitive, say
   * `Id[A](n: Long)`. Equal values alone cannot witness A =:= B:
   * `Id[User](5)` and `Id[Order](5)` are equal numbers and different
   * keys. So a value key must carry a runtime TAG of its type, and
   * "the same key" is "equal value AND equal tag". The tag is a
   * ClassTag: exact for non-generic A, erased for a generic one
   * (`Id[List[Int]]` and `Id[List[String]]` share a tag) — so value
   * keys are for concrete types, stated here and in the test */
  def byValue[K[_]](equal: [A, B] => (K[A], K[B]) => Boolean,
                    tag: [A] => K[A] => scala.reflect.ClassTag[A]): Same[K] = new Same[K]:
    def same[A, B](a: K[A], b: K[B]): Option[A =:= B] =
      if equal(a, b) && tag(a) == tag(b) then Some(summon[A =:= A].asInstanceOf[A =:= B]) else None

/** the witness, if b is this key. `===` is the operator the stack
 * needs for typed tokens: not a Boolean but the PROOF — in the
 * `Some(ev)` branch the compiler knows A is B and `ev(x: A): B`
 * converts; `=!=` is the Boolean "not the same key". A plain
 * `==` stays what it is (`equals`, permitted under strictEquality by
 * the CanEqual below) for the places that want only a yes or no */
extension [K[_], A](a: K[A])(using s: Same[K])
  def sameAs[B](b: K[B]): Option[A =:= B] = s.same(a, b)
  infix def ===[B](b: K[B]): Option[A =:= B] = s.same(a, b)
  infix def =!=[B](b: K[B]): Boolean = s.same(a, b).isEmpty

/** strict equality for token keys: two keys of one constructor may
 * always be compared — `Same` decides, `==` may ask. Top-level in the
 * package (like the stack's other givens: `import okay.given`) so it
 * is in scope where keys are compared, not only where Same is named */
given sameCanEqual[K[_], A, B](using Same[K]): CanEqual[K[A], K[B]] = CanEqual.derived

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

/**
 * A heterogeneous map with TYPED keys: a key `K[A]` holds an `A`, and
 * that is the whole contract — `get(k: K[Int])` is an `Option[Int]`,
 * `updated(k: K[Int], "x")` does not compile. Keys are tokens and
 * compare by IDENTITY (`eq`), never by equals: two keys that happen
 * to be equal are two entries.
 *
 * The store is a stack of typed pairs — a cons list of `Entry[K, A]`,
 * newest first, which is the nested-pair shape `(e1, (e2, (e3, ())))`
 * with every element still typed as an entry (a runtime `Tuple` would
 * lose that and need a type test per element). The pair is a class,
 * not a `(K[?], ?)` tuple, because only a class can say "the SAME A
 * on both sides"; that is where the key/value link is established,
 * once, by the type system.
 *
 * The map itself has no cast. What it needs from a key type is a
 * PROOF that two keys are the same key — and so hold the same type:
 * `Same[K]` (above), `same(a: K[A], b: K[B]): Option[A =:= B]`.
 * The key type states that axiom once, where it belongs (for
 * reference keys, `Same.byIdentity`); TMap only ever APPLIES the
 * witness.
 */
final class TMap[K[_]] private (private val stack: List[TMap.Entry[K, ?]]) {
  import TMap.Entry

  /** the value under k, if any — typed by the key, through the key
   * type's own sameness proof */
  def get[A](k: K[A])(using Same[K]): Option[A] =
    def at[X](e: Entry[K, X]): Option[A] = (e.key === k).map(ev => ev(e.value))
    stack.iterator.map(e => at(e)).collectFirst { case Some(v) => v }

  def contains[A](k: K[A])(using Same[K]): Boolean = get(k).isDefined

  /** k now holds v; an entry for the same key is replaced in place */
  def updated[A](k: K[A], v: A)(using Same[K]): TMap[K] =
    val e = Entry(k, v)
    def isK[X](x: Entry[K, X]): Boolean = (x.key === k).isDefined
    if contains(k) then TMap(stack.map(x => if isK(x) then e else x))
    else TMap(e :: stack)

  def isEmpty: Boolean = stack.isEmpty
  def nonEmpty: Boolean = stack.nonEmpty
  def size: Int = stack.length

  /** the typed pairs, in insertion order (an abstract K cannot be
   * applied to a wildcard, so the entry is the existential) */
  def entries: Iterator[Entry[K, ?]] = stack.reverseIterator

  /** typed iteration: f sees each key with its own value's type — a
   * polymorphic function, so no element is ever cast */
  def foreach(f: [A] => (K[A], A) => Unit): Unit =
    def one[A](e: Entry[K, A]): Unit = f(e.key, e.value)
    stack.reverseIterator.foreach(e => one(e))

  /** the same typed iteration in NO promised order — a forward walk
   * of the stack, no reversed copy, no iterator. For a caller whose
   * per-entry work is independent of the others': the STM commit
   * installs and wakes cells this way, where the reverse iterator was
   * a tenth of a transaction by CPU sample (stm-log-cost). */
  def foreachUnordered(f: [A] => (K[A], A) => Unit): Unit =
    def one[A](e: Entry[K, A]): Unit = f(e.key, e.value)
    var s = stack
    while s.nonEmpty do
      one(s.head)
      s = s.tail

  override def toString: String =
    def show[A](e: Entry[K, A]): String = s"${e.key} -> ${e.value}"
    stack.reverseIterator.map(e => show(e)).mkString("TMap(", ", ", ")")
}

object TMap {
  /** the typed pair: one A on both sides, fixed at construction */
  final case class Entry[K[_], A](key: K[A], value: A)

  def empty[K[_]]: TMap[K] = new TMap[K](Nil)
}

/**
 * A concurrent, cross-platform key-value map over ONE `TRef`
 * (specs/stm.md, okay-stm-collections): every operation is a single
 * `TRef.modify` — one CAS loop, never blocks — so the API stays
 * PLAIN and SYNCHRONOUS. `Tx`/`Stm[F]` exists to coordinate MANY
 * cells in one transaction; a dict backed by one cell never needs
 * more than that cell for any of its own operations, so there is no
 * facade cost here, honest or otherwise — synchronous IS the honest
 * shape for a single-cell structure.
 *
 * Named `TDict`, not `TMap`: `okay.TMap[K[_]]` already exists (the
 * STM engine's own heterogeneous write-set bookkeeping, keyed by a
 * type CONSTRUCTOR, not a plain key type) — a different shape this
 * name would collide with.
 */
final class TDict[K, A](init: Map[K, A] = Map.empty[K, A]) {
  private val ref = TRef(init)

  def get(k: K): Option[A] = ref.get.get(k)
  def contains(k: K): Boolean = ref.get.contains(k)
  def put(k: K, v: A): Unit = ref.modify(m => (m.updated(k, v), ()))
  def remove(k: K): Unit = ref.modify(m => (m.removed(k), ()))

  /** Registry.apply's exact seam: create-if-absent, atomically —
   * every caller racing the same missing key observes the SAME
   * winning value, none lost. Stated, not hidden (found by a
   * 64-thread stress test): `mk` inherits `TRef.modify`'s own "f may
   * run more than once" rule — a CAS loser's attempt already
   * evaluated `mk` before losing the race, and that value is
   * discarded, never stored or returned. Fine for a pure `mk`
   * (`Subscription.joinedOf`'s `now`); a `mk` with a real side
   * effect or allocation (`Registry.apply`'s `Channel()`) pays for
   * every LOST attempt too, not just the winner — worth knowing
   * before reaching for this on such a key */
  def computeIfAbsent(k: K)(mk: => A): A =
    ref.modify { m =>
      m.get(k) match
        case Some(v) => (m, v)
        case None => val v = mk; (m.updated(k, v), v)
    }

  /** read-modify-write at one key, atomically — the general form
   * `computeIfAbsent` is one case of: two concurrent callers
   * updating the SAME key never lose either's contribution, unlike
   * a plain get-then-put pair (two separate, non-atomic modifies) */
  def updateAt(k: K)(f: Option[A] => A): A =
    ref.modify { m =>
      val v = f(m.get(k))
      (m.updated(k, v), v)
    }

  def snapshot: Map[K, A] = ref.get
  def size: Int = ref.get.size
  def isEmpty: Boolean = ref.get.isEmpty
  def clear(): Unit = ref.modify(_ => (Map.empty, ()))
}

object TDict:
  def empty[K, A]: TDict[K, A] = TDict()

/**
 * The `TList` shape the same spec names: append + snapshot, over
 * ONE `TRef[Vector[A]]` — the identical synchronous reasoning as
 * `TDict`.
 */
final class TList[A](init: Vector[A] = Vector.empty[A]) {
  private val ref = TRef(init)

  def append(a: A): Unit = ref.modify(v => (v :+ a, ()))
  def snapshot: Vector[A] = ref.get
  def size: Int = ref.get.size
  def isEmpty: Boolean = ref.get.isEmpty
  def clear(): Unit = ref.modify(_ => (Vector.empty, ()))
}

object TList:
  def empty[A]: TList[A] = TList()
