package okay

/**
 * The STATIC heterogeneous map: the map's TYPE lists its entries, as
 * a tuple of `(key.type, Value)` pairs — `((n.type, Int), (s.type,
 * String))`, newest first — so `get` is resolved by the
 * compiler, membership is a compile-time fact (a key the map does not
 * hold does not compile), and there is no cast anywhere: lookup is a
 * typeclass `Select` derived by induction over the tuple type, whose
 * instance for the matching head returns the head's value at its own
 * type. Keys are singleton types, so two distinct keys of the same
 * K[Int] are two entries and the same key added twice is shadowed by
 * the newer one.
 *
 * The price of "real": keys must be stable identifiers (vals) known
 * at the use site, and the map's type grows with every entry. Where
 * keys are runtime values — the STM's write set, whose cells come
 * from anywhere — that is impossible by nature, and TMap (identity
 * keys, one stated cast) is the honest tool. Both exist because they
 * answer different questions: HMap "which entries does this map
 * hold?" in the type, TMap "what did this run put under this key?"
 * at runtime.
 */
opaque type HMap[K[_], T <: Tuple] = T

object HMap {
  def empty[K[_]]: HMap[K, EmptyTuple] = EmptyTuple

  /** the value of type V under the key of singleton type S, in the
   * tuple T — derived by the compiler by induction over T; V is a
   * type PARAMETER so that inference carries it out to the call site */
  trait Select[T <: Tuple, S, V]:
    def apply(t: T): V

  object Select:
    /** the head IS the key: its value, at its type */
    given head[S, V, Tl <: Tuple]: Select[(S, V) *: Tl, S, V] with
      def apply(t: (S, V) *: Tl): V = t.head._2
    /** otherwise look further down */
    given tail[S, H, Tl <: Tuple, V](using s: Select[Tl, S, V]): Select[H *: Tl, S, V] with
      def apply(t: H *: Tl): V = s(t.tail)

  extension [K[_], T <: Tuple](m: HMap[K, T])
    /** k now holds v: the entry is added to the TYPE */
    def updated[A](k: K[A], v: A): HMap[K, (k.type, A) *: T] = (k, v) *: (m: T)

    /** the value under k — a compile error if the map's type has no
     * entry for this key */
    def get[A, V](k: K[A])(using s: Select[T, k.type, V]): V = s(m: T)

    def size: Int = (m: T).size

    /** the entries as their tuple: the type is the map's type */
    def toTuple: T = m
}
