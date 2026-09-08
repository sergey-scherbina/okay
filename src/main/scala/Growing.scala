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
 * WHAT COUNTS AS CONTENTION, corrected by measurement (2026-09-07).
 * The first cut grew on a REFUSED push, reasoning that a full ring
 * with a waiting producer is contention. The benchmark said no: at
 * sixteen producers a ring is 17x slower than a partitioned buffer
 * with ROOM TO SPARE, because what costs is sixteen threads CASing
 * one tail, not fullness — the ring was never refused and the buffer
 * never grew. Fullness is backpressure, which one producer can cause
 * on its own and which partitioning would not help.
 *
 * So the signal is DIFFERENT PRODUCERS, and it is sampled rather than
 * read: a plain counter every push, and every 64th one compares the
 * pushing thread with the last one sampled. Racy on purpose — it only
 * has to be right eventually, and a volatile read per push was
 * measured at 1.49x elsewhere in this codebase.
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
 *
 * WHOSE THREAD IS PUSHING (growing-onbehalf, 2026-09-08). The
 * identity above is read from `Thread.currentThread()`, and for a
 * push the producer makes itself that is right. It is NOT right when
 * the channel resumes a producer it had parked: `SentinelChannel`
 * runs the parked sender's continuation on whichever thread freed the
 * slot — the consumer's — so the resumed push arrives wearing the
 * consumer's identity. Read as evidence, that made ONE producer look
 * like two and grew the buffer in ten runs out of thirty. Such pushes
 * come in through `pushDecidingAtOnBehalf`, which this class
 * overrides to neither sample nor grow: a resumed push says nothing
 * about who is producing, in either direction.
 *
 * The correctness of that is a law
 * (`TestGrowing`, "one producer through a CHANNEL never grows it").
 * Its SPEED is not the point and the honest number says so: the lane
 * at one producer went from 1.53x the plain ring to 1.47x, which is
 * inside the noise. The spurious growth was worth about six points of
 * the fifty-three; the rest is this wrapper's own dispatch, and no
 * change to the growth trigger will touch it.
 *
 * WHAT IT COSTS TODAY, measured, minimum of five rounds, us per 8 000
 * elements through a channel:
 *
 * {{{
 * producers      ring    growing   adaptive
 *         1       123        158        144
 *         4       715        477        136
 *        16     3 066        446        100
 * }}}
 *
 * Read that honestly: it is 6.9x the ring where the ring is weak, and
 * it is behind BOTH of the buffers it is made of at their own shapes,
 * because after the swap every push goes through two layers — this
 * wrapper and the partitioned buffer inside it. Choose `bounded` when
 * there is one producer, `adaptive` when there are many from the
 * first message, and this when the count is genuinely unknown and you
 * would rather not be wrong at either end.
 *
 * The layer is what has to go, and the way to remove it is for the
 * CHANNEL to replace its buffer rather than wrap it — one layer on
 * each side of the swap. That is filed as `queue-swap`, its audit is
 * already landed, and this class is the piece it grows out of.
 */
final class Growing[A](initial: Buffer[A], cap: Int, each: () => Buffer[A]) extends Buffer[A] {

  /** read ONCE per operation into a local: two reads in one operation
   * could straddle the swap and compare a ring against a part of
   * itself */
  @volatile private var inner: Buffer[A] = initial
  private val grown = AtomicBoolean(false)
  /** the last producer sampled, and the sampling counter. The counter
   * is plain: a lost increment costs a later sample, nothing else */
  @volatile private var sampled: Thread | Null = null
  /**
   * The sample counter lives OFF this object, and that is worth 9 of
   * the 15 points sampling was costing (growing-wrapper-cost,
   * 2026-09-08). The producer stores to it on EVERY push while the
   * consumer loads `inner` from this object on every `popMany`, so
   * with the counter here the two share a cache line and the store
   * invalidates the load. Padding either side keeps it off the lines
   * its neighbours land on.
   *
   * Measured, alternating, five rounds each with the plain ring as
   * the in-run control: 1.200x the ring with the counter here, 1.109x
   * with it moved, 1.049x with sampling removed altogether. So this
   * recovers the sharing and leaves the branch, which is the honest
   * remainder and not worth a trick.
   */
  private final class Counter:
    var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Long = 0L
    var seen: Int = 0
    var pad7, pad8, pad9, pad10, pad11, pad12, pad13: Long = 0L
  private val counter = Counter()

  /** one swap, ever; losers use the winner's buffer */
  private def grow(): Buffer[A] =
    if grown.compareAndSet(false, true) then
      // part 0 is the ring, and its owner is the producer that filled it
      val partitioned = AdaptiveFifo[A](cap, each, eager = false, first = inner, firstOwner = sampled)
      inner = partitioned
      partitioned
    else inner

  /** every 64th push: a producer that is not the one we sampled last
   * means more than one is pushing, which is what parts are for */
  private def sample(): Unit =
    val n = counter.seen + 1
    counter.seen = n
    if (n & 63) == 0 && !grown.get then
      val me = Thread.currentThread()
      val last = sampled
      if last == null then sampled = me
      else if !(last eq me) then { val _ = grow() }

  /** the refusal path still grows it when a second producer is
   * genuinely blocked behind a full part — backpressure AND
   * contention, which is the one case where fullness means parts */
  private def refused(): Buffer[A] | Null =
    if grown.get then inner
    else
      val me = Thread.currentThread()
      val last = sampled
      if last == null then { sampled = me; null }
      else if (last eq me) then null
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
    sample()
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

  // the channel's own send path is `pushDecidingAt`, so the refusal
  // that means contention arrives HERE and not through `push` — the
  // first cut delegated these two straight through and the buffer
  // never grew under a channel at all, only under a direct caller
  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    sample()
    val b = inner
    val out = b.pushDeciding(a, unless, orElse)
    if out != null then out
    else
      val grownTo = refused()
      if grownTo == null then null else grownTo.nn.pushDeciding(a, unless, orElse)

  override def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
    sample()
    val b = inner
    val out = b.pushDecidingAt(route, a, unless, orElse)
    if out != null then out
    else
      val grownTo = refused()
      if grownTo == null then null
      else
        // the route was taken from the OLD buffer; ask the new one for
        // this producer's own part rather than reusing a stale index
        grownTo.nn.pushDecidingAt(grownTo.nn.route(), a, unless, orElse)

  /** the channel is pushing for a producer that parked here, from
   * whichever thread freed the slot — so this call says nothing about
   * WHO is producing and is not allowed to teach us anything. No
   * sample, and a refusal does not grow: it means the buffer is still
   * full, which one producer can manage on its own. */
  override def pushDecidingAtOnBehalf(route: Int, a: A,
                                      unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDecidingAtOnBehalf(route, a, unless, orElse)

  override def pushMany(n: Int)(src: Int => A): Int =
    val b = inner
    val took = b.pushMany(n)(src)
    if took > 0 || n == 0 then took
    else
      val grownTo = refused()
      if grownTo == null then 0 else grownTo.nn.pushMany(n)(src)
}
