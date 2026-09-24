package okay2

/**
 * The STATIC heterogeneous map (the Scala 3 core's HMap.scala; okay2
 * spec stage 20): the map's TYPE lists its entries, newest first, so
 * `get` is resolved by the compiler, membership is a compile-time fact
 * (a key the map does not hold does not compile), and there is no cast
 * anywhere: lookup is a typeclass `Select` derived by induction over the
 * entry list, whose instance for the matching head returns the head's
 * value at its own type. Keys are singleton types, so two distinct keys
 * of the same `K[Int]` are two entries and the same key added twice is
 * shadowed by the newer one.
 *
 * Scala 3 lists the entries as a tuple of `(key.type, V)` pairs; Scala 2
 * has no `*:`, so the list is its own — `Cons[S, V, T]` ending in `Nil` —
 * and the map holds exactly that list at run time, so what the type says
 * and what is stored are one structure.
 *
 * The price of "real": keys must be stable identifiers (vals) known at
 * the use site, and the map's type grows with every entry. Where keys
 * are run-time values — the STM's write set — that is impossible by
 * nature, and `TMap` is the tool: HMap answers "which entries does this
 * map hold?" in the type, TMap "what did this run put under this key?"
 * at run time.
 */
final class HMap[K[_], T] private (private val entries: T) {
  /** k now holds v: the entry is added to the TYPE */
  def updated[A](k: K[A], v: A): HMap[K, HMap.Cons[k.type, A, T]] =
    new HMap[K, HMap.Cons[k.type, A, T]](HMap.Cons[k.type, A, T](k, v, entries))

  /** the value under k — a compile error if the map's type has no entry
   * for this key */
  def get[A, V](k: K[A])(implicit s: HMap.Select[T, k.type, V]): V = s(entries)

  def size: Int = HMap.count(entries)

  /** the entries as their list: the type is the map's type */
  def toList: T = entries
}

object HMap {
  /** one entry, and the entries after it */
  final case class Cons[S, V, T](key: S, value: V, tail: T)
  /** no entries */
  sealed abstract class Nil
  case object Nil extends Nil

  def empty[K[_]]: HMap[K, Nil] = new HMap[K, Nil](Nil)

  /** the value of type V under the key of singleton type S, in the list
   * T — derived by the compiler by induction over T; V is a type
   * PARAMETER so that inference carries it out to the call site */
  @scala.annotation.implicitNotFound("this HMap has no entry for the key ${S}: its type lists what it holds, and a key is a val's singleton type — an equal key made elsewhere is a different entry")
  trait Select[T, S, V] { def apply(t: T): V }

  /** otherwise look further down — LOWER priority than `head`, so a key
   * added twice resolves to the newer entry rather than an ambiguity
   * (Scala 3 needs no priority: its tuple match is by shape) */
  trait SelectLow {
    implicit def tail[S, H, V2, Tl, V](implicit s: Select[Tl, S, V]): Select[Cons[H, V2, Tl], S, V] =
      new Select[Cons[H, V2, Tl], S, V] { def apply(t: Cons[H, V2, Tl]): V = s(t.tail) }
  }

  object Select extends SelectLow {
    /** the head IS the key: its value, at its type */
    implicit def head[S, V, Tl]: Select[Cons[S, V, Tl], S, V] =
      new Select[Cons[S, V, Tl], S, V] { def apply(t: Cons[S, V, Tl]): V = t.value }
  }

  private def count(t: Any): Int = t match {
    case c: Cons[_, _, _] => 1 + count(c.tail)
    case _ => 0
  }
}
