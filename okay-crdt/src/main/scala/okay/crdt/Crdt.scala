package okay.crdt

/**
 * A CONVERGENT REPLICATED DATA TYPE, which is a shorter way of saying
 * a join-semilattice (specs/coordination-free.md stage 2).
 *
 * THE WHOLE CONTENT IS THE LAWS. `merge` must be:
 *
 *   commutative   merge(x, y) == merge(y, x)
 *   associative   merge(merge(x, y), z) == merge(x, merge(y, z))
 *   idempotent    merge(x, x) == x
 *
 * and what those three buy is the only property anyone wants from
 * this: replicas that have seen the same UPDATES agree, whatever
 * order the updates arrived in, however often they arrived, and with
 * nobody coordinating. Commutativity forgives reordering,
 * associativity forgives regrouping, idempotence forgives redelivery
 * — which are exactly the three things a network does to messages.
 *
 * A type whose merge breaks one of them is not "mostly a CRDT". It is
 * a type that silently disagrees with itself under load, and that is
 * why `Crdt.violations` ships in the library rather than in the tests:
 * anyone defining an instance can run it, and every instance here
 * does.
 *
 * WHY IT FITS THIS LIBRARY. okay-cache's `View` builds a cache from a
 * FOLD over a log and says a consumer "is never invalid, only behind".
 * A CRDT is that fold with the laws written down, so a replica and a
 * cache are the same shape — which is what stage 3 makes literal.
 */
trait Crdt[A]:
  def merge(x: A, y: A): A

object Crdt:

  def apply[A](using c: Crdt[A]): Crdt[A] = c

  extension [A](x: A)(using c: Crdt[A])
    /** the join: `x` and `y` seen together */
    infix def merge(y: A): A = c.merge(x, y)

  /**
   * Check the three laws over a sample of values and ANSWER what
   * broke, rather than throwing at the first failure — a merge is
   * usually wrong in one law and right in the others, and knowing
   * which one is the whole diagnosis.
   *
   * The samples must be values a replica could really hold. Three or
   * four are enough: associativity is the only law needing three, and
   * it is checked over every ordered triple.
   */
  def violations[A](samples: Seq[A])(using c: Crdt[A]): List[String] =
    val out = List.newBuilder[String]
    for x <- samples; y <- samples do
      if c.merge(x, y) != c.merge(y, x) then
        out += s"not commutative: merge($x, $y) = ${c.merge(x, y)} but merge($y, $x) = ${c.merge(y, x)}"
    for x <- samples do
      if c.merge(x, x) != x then
        out += s"not idempotent: merge($x, $x) = ${c.merge(x, x)}"
    for x <- samples; y <- samples; z <- samples do
      val l = c.merge(c.merge(x, y), z)
      val r = c.merge(x, c.merge(y, z))
      if l != r then
        out += s"not associative: (($x, $y), $z) = $l but ($x, ($y, $z)) = $r"
    out.result()

/**
 * Who is speaking. A replica needs a name for two reasons and both
 * matter: a counter has to know whose count to raise, and a
 * last-write-wins register has to break a tie between two writes
 * stamped in the same instant — without a deterministic tie-break,
 * two replicas can disagree for ever and the merge is not a merge.
 *
 * A plain `String` rather than a `Uid`: a node name is CONFIGURED and
 * stable across restarts, which is the opposite of what `Uid` is for.
 */
opaque type NodeId = String

object NodeId:
  def apply(s: String): NodeId = s
  extension (n: NodeId) def name: String = n
  given Ordering[NodeId] = Ordering.String
