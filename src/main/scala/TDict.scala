package okay

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
