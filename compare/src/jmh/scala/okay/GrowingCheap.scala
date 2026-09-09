package okay

import java.util.concurrent.atomic.AtomicBoolean

/**
 * DIAGNOSTIC (queue-swap, 2026-09-09): `Growing` with its COUNTING
 * sample replaced by a single identity compare, and nothing else
 * changed.
 *
 * `Growing` costs 1.109x the ring it wraps at one producer, and
 * yesterday's split of that number (growing-wrapper-cost) says the
 * layer of call is 1.02-1.05x and the rest is the sample: a plain
 * store to a counter on EVERY push, padded onto its own cache line
 * because the consumer reads `inner` from the same object.
 *
 * A store is what makes it cost. This variant has none: it loads the
 * last producer seen and compares it with the current thread. The
 * line holding `sampled` is written twice in the life of the buffer —
 * once by the first producer, once at growth — so it stays SHARED in
 * every producer's cache and the read is as close to free as a field
 * read gets. The `doneGrowing` hint needs no padding for the same
 * reason: after the one write nobody stores to it again.
 *
 * It is also STRICTLY more responsive than the counter: the second
 * producer's first push grows the buffer, rather than its 64th.
 *
 * MEASURED, and REFUTED. Two independent runs of three rounds each,
 * us per 1 024 push+pop, minimum per lane:
 *
 * {{{
 * ring            9.036   1.000x
 * forwarding      9.057   1.002x
 * noSample        9.053   1.002x
 * growing         9.913   1.097x
 * growingCheap   10.479   1.160x
 * }}}
 *
 * It is 6% WORSE than the counter it replaces. A volatile load and a
 * `Thread.currentThread()` on every push lose to a plain store to a
 * padded counter with the compare hidden behind an every-64th
 * branch — the store is to a line this thread already owns, while
 * the load orders against every other read in the push.
 *
 * Kept, not deleted: the row is what makes the counter's design a
 * measured choice rather than the first thing that was tried.
 */
final class GrowingCheap[A](initial: Buffer[A], cap: Int, each: () => Buffer[A]) extends Buffer[A] {

  @volatile private var inner: Buffer[A] = initial
  private val grown = AtomicBoolean(false)
  @volatile private var sampled: Thread | Null = null

  /** a racy hint that the one swap has happened; plain on purpose,
   * and written once ever, so it shares a line with nothing that
   * matters */
  private var doneGrowing: Boolean = false

  private def grow(): Buffer[A] =
    doneGrowing = true
    if grown.compareAndSet(false, true) then
      val partitioned = AdaptiveFifo[A](cap, each, eager = false, first = inner, firstOwner = sampled)
      inner = partitioned
      partitioned
    else inner

  /** no counter, no store: one load and one reference compare */
  private def sample(): Unit =
    if !doneGrowing then
      val me = Thread.currentThread()
      val last = sampled
      if last == null then sampled = me
      else if !(last eq me) then { val _ = grow() }

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
      if grownTo == null then false else grownTo.nn.push(a)

  override def pushAt(route: Int, a: A): Boolean =
    val b = inner
    if b.pushAt(route, a) then true
    else
      val grownTo = refused()
      if grownTo == null then false else grownTo.nn.pushAt(route, a)

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
      else grownTo.nn.pushDecidingAt(grownTo.nn.route(), a, unless, orElse)

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

/**
 * DIAGNOSTIC (queue-swap, 2026-09-09): `Growing` with NO trigger at
 * all — the volatile field and the forwarding, and nothing else.
 *
 * It exists to split the 9.4% `growing_fillDrain` costs over the ring
 * into its two halves, because the split decides `queue-swap`. The
 * entry's first step turns `SentinelChannel.ring` from a `private
 * val` into a `@volatile private var`, so if the volatile READ is
 * most of the cost, moving the swap into the channel does not remove
 * it — it moves the same read from the buffer to the channel and
 * removes only the call, which `forwarding_fillDrain` prices at zero.
 *
 * Not a candidate for shipping: without a trigger it never grows, so
 * it is a ring with an extra volatile read. Only the number matters.
 *
 * MEASURED: 9.053 against the ring's 9.036 — 1.002x, which is
 * nothing. The volatile read is FREE, and so is the forwarding layer
 * beside it (9.057, 1.002x). All 9.7% of what `Growing` costs at one
 * producer is the trigger.
 *
 * That is the number that closed `queue-swap` as filed. Its first
 * step turns `SentinelChannel.ring` into a `@volatile private var` —
 * this row — to remove the wrapper — the forwarding row. Both are
 * zero, so the plan trades nothing for nothing and leaves the 9.7%
 * exactly where it was.
 */
final class GrowingNoSample[A](initial: Buffer[A]) extends Buffer[A] {

  @volatile private var inner: Buffer[A] = initial

  /** the swap this row exists to price, spelled out so the field is a
   * genuine `var` rather than a `val` the compiler would fold: a
   * final field can be hoisted out of the push loop and a volatile
   * one cannot, and hoisting it would measure the wrong thing. The
   * benchmark never calls this. */
  def replace(b: Buffer[A]): Unit = inner = b

  override def capacity: Int = inner.capacity
  override def size: Int = inner.size
  override def isEmpty: Boolean = inner.isEmpty
  override def hasReady: Boolean = inner.hasReady
  override def hasRoom: Boolean = inner.hasRoom
  override def hasRoomAt(route: Int): Boolean = inner.hasRoomAt(route)
  override def parts: Int = inner.parts
  override def route(): Int = inner.route()
  override def lastRoute: Int = inner.lastRoute
  override def pop(): A | Null = inner.pop()
  override def popMany(max: Int)(sink: A => Unit): Int = inner.popMany(max)(sink)
  override def seal(mark: A): Int = inner.seal(mark)
  override def push(a: A): Boolean = inner.push(a)
  override def pushAt(route: Int, a: A): Boolean = inner.pushAt(route, a)
  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDeciding(a, unless, orElse)
  override def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDecidingAt(route, a, unless, orElse)
  override def pushDecidingAtOnBehalf(route: Int, a: A,
                                      unless: AtomicBoolean, orElse: A): A | Null =
    inner.pushDecidingAtOnBehalf(route, a, unless, orElse)
  override def pushMany(n: Int)(src: Int => A): Int = inner.pushMany(n)(src)
}
