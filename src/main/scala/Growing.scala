package okay

import java.util.concurrent.atomic.AtomicBoolean

/**
 * A buffer that BECOMES partitioned when producers actually contend.
 *
 * The measured problem it answers (queue-swap, 2026-09-07): a plain
 * ring is the fastest thing a channel can have while there is one
 * producer — 122 us against a partitioned buffer's 144 — and the
 * worst thing it can have at sixteen: 2 375 against 99. A channel
 * does not know which it will be, and asking the caller to know is
 * asking them to guess.
 *
 * So this starts as the ring it was given and, when TWO DIFFERENT
 * producers have been refused by it, installs an `AdaptiveFifo` that
 * ADOPTS that ring as its part 0. No element moves, nothing is
 * copied, and a reader holding either reference reaches the same part
 * 0, so every element is still read exactly once and in its
 * producer's order.
 *
 * Two different producers, not one refusal: a bounded ring refuses
 * whenever it is FULL, which a single fast producer does to a slow
 * consumer all day long, and partitioning that channel would buy
 * nothing and cost the layer for ever. Two refused producers is
 * contention rather than backpressure. Both reads live on the refusal
 * path, which already ends in a park, so the fast path pays nothing.
 *
 * What growth does NOT do is rescue the producer that triggered it.
 * That producer keeps part 0 — the ring it filled — because its own
 * elements are in there and its order is only preserved while it
 * pushes to the same part. It waits for room exactly as it would
 * have; what changes is that every OTHER producer stops queueing
 * behind it.
 *
 * `maxParts` reports the grown shape from the start, so a channel
 * sizes its per-part waiter queues once and never resizes them.
 */
final class Growing[A](initial: Buffer[A], cap: Int, each: () => Buffer[A]) extends Buffer[A] {

  /** read ONCE per operation into a local: two reads in one operation
   * could straddle the swap and compare a ring against a part of
   * itself */
  @volatile private var inner: Buffer[A] = initial
  private val grown = AtomicBoolean(false)
  /** the first producer this buffer refused; the second one that is
   * not it is what contention means here */
  @volatile private var refusedOne: Thread | Null = null

  /** one swap, ever; losers use the winner's buffer */
  private def grow(): Buffer[A] =
    if grown.compareAndSet(false, true) then
      // part 0 is the ring, and its owner is the producer that filled it
      val partitioned = AdaptiveFifo[A](cap, each, eager = false, first = inner, firstOwner = refusedOne)
      inner = partitioned
      partitioned
    else inner

  /** on the refusal path only: the second DIFFERENT producer to be
   * refused turns this into a partitioned buffer */
  private def refused(): Buffer[A] | Null =
    if grown.get then inner
    else
      val me = Thread.currentThread()
      val first = refusedOne
      if first == null then { refusedOne = me; null }
      else if (first eq me) then null
      else grow()

  override def capacity: Int = inner.capacity
  override def size: Int = inner.size
  override def isEmpty: Boolean = inner.isEmpty
  override def hasReady: Boolean = inner.hasReady
  override def hasRoom: Boolean = inner.hasRoom
  override def hasRoomAt(route: Int): Boolean = inner.hasRoomAt(route)
  override def parts: Int = inner.parts
  override def maxParts: Int = cap
  override def route(): Int = inner.route()
  override def lastRoute: Int = inner.lastRoute
  override def pop(): A | Null = inner.pop()
  override def popMany(max: Int)(sink: A => Unit): Int = inner.popMany(max)(sink)
  override def seal(mark: A): Int = inner.seal(mark)

  override def push(a: A): Boolean =
    val b = inner
    if b.push(a) then true
    else
      val grownTo = refused()
      if grownTo == null then false          // full, and not contention: the sender parks
      else grownTo.nn.push(a)                // a part of our own, if we are not part 0's owner

  override def pushAt(route: Int, a: A): Boolean =
    val b = inner
    if b.pushAt(route, a) then true
    else
      val grownTo = refused()
      if grownTo == null then false else grownTo.nn.pushAt(route, a)

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDeciding(a, unless, orElse)

  override def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDecidingAt(route, a, unless, orElse)

  override def pushMany(n: Int)(src: Int => A): Int =
    val b = inner
    val took = b.pushMany(n)(src)
    if took > 0 || n == 0 then took
    else
      val grownTo = refused()
      if grownTo == null then 0 else grownTo.nn.pushMany(n)(src)
}
