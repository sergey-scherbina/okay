package okay

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
 * `Same[K]` (Same.scala), `same(a: K[A], b: K[B]): Option[A =:= B]`.
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

  override def toString: String =
    def show[A](e: Entry[K, A]): String = s"${e.key} -> ${e.value}"
    stack.reverseIterator.map(e => show(e)).mkString("TMap(", ", ", ")")
}

object TMap {
  /** the typed pair: one A on both sides, fixed at construction */
  final case class Entry[K[_], A](key: K[A], value: A)

  def empty[K[_]]: TMap[K] = new TMap[K](Nil)
}
