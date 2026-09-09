package okay.crdt

/**
 * A GROW-ONLY COUNTER: the simplest thing that is genuinely a CRDT,
 * and the one that shows why the shape is what it is.
 *
 * The naive replicated counter — one number, merged by adding — is
 * wrong, and wrong in the way that matters: addition is commutative
 * and associative but NOT idempotent, so a message delivered twice
 * counts twice and two replicas that have seen the same increments
 * disagree. The fix is not a better network. It is to stop merging
 * the total and start merging WHO COUNTED WHAT: a map from node to
 * that node's own count, merged pointwise by `max`.
 *
 * `max` is idempotent, which buys redelivery for free, and each node
 * writes only its own entry, so there is nothing to race over.
 */
final case class GCounter(counts: Map[NodeId, Long]):
  /** this node counts one more */
  def inc(node: NodeId, by: Long = 1L): GCounter =
    require(by >= 0, s"a grow-only counter cannot go down: $by")
    GCounter(counts.updated(node, counts.getOrElse(node, 0L) + by))
  /** what every node has counted, together */
  def value: Long = counts.valuesIterator.sum

object GCounter:
  val empty: GCounter = GCounter(Map.empty)

  given Crdt[GCounter] with
    def merge(x: GCounter, y: GCounter): GCounter =
      // pointwise max, over the union of the keys: a node absent on
      // one side has counted zero there, and max(n, 0) is n
      val keys = x.counts.keySet ++ y.counts.keySet
      GCounter(keys.view.map { k =>
        k -> math.max(x.counts.getOrElse(k, 0L), y.counts.getOrElse(k, 0L))
      }.toMap)

/**
 * A COUNTER THAT GOES BOTH WAYS, built as two grow-only counters
 * rather than as one that can decrease.
 *
 * The reason is the same law again. A counter that merges by taking
 * the larger value cannot support subtraction — "larger" stops
 * meaning "later" the moment a decrement exists. Two monotone
 * counters, one for what was added and one for what was taken away,
 * keep every part growing; the answer is their difference, and the
 * difference of two convergent values is convergent.
 *
 * The cost is honest and worth stating: this never shrinks. A key
 * that has been incremented and decremented to zero still carries
 * both counts for ever, on every replica. Garbage collection needs
 * to know that every replica has seen a value — causal stability —
 * which is out of scope here and named in the spec.
 */
final case class PNCounter(ups: GCounter, downs: GCounter):
  def inc(node: NodeId, by: Long = 1L): PNCounter = PNCounter(ups.inc(node, by), downs)
  def dec(node: NodeId, by: Long = 1L): PNCounter = PNCounter(ups, downs.inc(node, by))
  def value: Long = ups.value - downs.value

object PNCounter:
  val empty: PNCounter = PNCounter(GCounter.empty, GCounter.empty)

  given Crdt[PNCounter] with
    def merge(x: PNCounter, y: PNCounter): PNCounter =
      val g = summon[Crdt[GCounter]]
      PNCounter(g.merge(x.ups, y.ups), g.merge(x.downs, y.downs))
