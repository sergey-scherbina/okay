package okay.crdt

import okay.Uid

/**
 * A GROW-ONLY SET: union, and union is already the three laws. It is
 * here because it is the honest floor — if a program only ever adds,
 * this is the whole answer and nothing below is needed.
 */
final case class GSet[A](items: Set[A]):
  def add(a: A): GSet[A] = GSet(items + a)
  def contains(a: A): Boolean = items.contains(a)
  def value: Set[A] = items

object GSet:
  def empty[A]: GSet[A] = GSet(Set.empty)
  given [A]: Crdt[GSet[A]] with
    def merge(x: GSet[A], y: GSet[A]): GSet[A] = GSet(x.items ++ y.items)

/**
 * AN OBSERVED-REMOVE SET, which is the answer to the question a
 * grow-only set cannot answer: how do you take something out?
 *
 * The naive fix — keep a set of removals and subtract — has a bug
 * that no amount of care removes: add, remove, add again, and the
 * element is gone for ever on any replica that sees the removal last.
 * "Removed" is not a property of the ELEMENT. It is a property of a
 * particular ADDITION, and a set that cannot tell two additions of
 * the same value apart cannot express that.
 *
 * So every add carries a unique tag, and this is the arc's own `Uid`
 * doing a third job: a tag has to be unique without coordination,
 * which is exactly what a locally-issued sortable id is. Removing an
 * element tombstones the tags THIS replica has observed for it —
 * hence the name — so a concurrent add, whose tag nobody has seen
 * yet, survives. Add-wins, and it is add-wins for a reason rather
 * than by preference: the alternative loses writes that were never
 * observed by the remover, which is a lost update with extra steps.
 *
 * `merge` is a union on both halves, so it inherits the three laws
 * from set union. The cost is the tombstones, which grow for ever
 * without causal stability — the same debt `PNCounter` carries, and
 * out of scope for the same reason.
 */
final case class OrSet[A](adds: Map[A, Set[Uid]], removed: Set[Uid]):

  /** add with a fresh tag; the generator is a parameter so a test can
   * make the tags deterministic */
  def add(a: A, tag: Uid): OrSet[A] =
    OrSet(adds.updated(a, adds.getOrElse(a, Set.empty) + tag), removed)

  /** tombstone every tag for `a` that THIS replica has seen — a
   * concurrent add elsewhere carries a tag we have not observed, and
   * survives */
  def remove(a: A): OrSet[A] =
    OrSet(adds, removed ++ adds.getOrElse(a, Set.empty))

  /** present when at least one of its tags is not tombstoned */
  def contains(a: A): Boolean =
    adds.getOrElse(a, Set.empty).exists(t => !removed.contains(t))

  def value: Set[A] = adds.keysIterator.filter(contains).toSet

object OrSet:
  def empty[A]: OrSet[A] = OrSet(Map.empty, Set.empty)

  given [A]: Crdt[OrSet[A]] with
    def merge(x: OrSet[A], y: OrSet[A]): OrSet[A] =
      val keys = x.adds.keySet ++ y.adds.keySet
      val union = keys.view.map { k =>
        k -> (x.adds.getOrElse(k, Set.empty) ++ y.adds.getOrElse(k, Set.empty))
      }.toMap
      OrSet(union, x.removed ++ y.removed)
