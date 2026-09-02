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
 * One cast lives here and nowhere else: in `get`, "the entry whose
 * key IS this key holds this key's type". Identity of a typed token
 * is the definition of type equality for a heterogeneous map, and
 * the compiler cannot see through it — so the claim is made in one
 * line, in the one function that needs it, and every user of TMap
 * (the STM's write set first) is cast-free.
 */
final class TMap[K[_]] private (private val stack: List[TMap.Entry[K, ?]]) {
  import TMap.Entry

  /** the value under k, if any — typed by the key */
  def get[A](k: K[A]): Option[A] =
    stack.find(_.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef])
      .map(e => e.asInstanceOf[Entry[K, A]].value)   // THE cast: identity of a typed key is type equality

  def contains[A](k: K[A]): Boolean = get(k).isDefined

  /** k now holds v; an entry for the same key (by identity) is replaced in place */
  def updated[A](k: K[A], v: A): TMap[K] =
    val e = Entry(k, v)
    if contains(k) then TMap(stack.map(x => if x.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef] then e else x))
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
