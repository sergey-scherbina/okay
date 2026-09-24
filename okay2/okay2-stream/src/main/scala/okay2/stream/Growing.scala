package okay2.stream

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/**
 * A buffer that BECOMES partitioned when producers actually contend —
 * okay-stream's Growing.scala. It starts as the ring it was given and,
 * when TWO DIFFERENT producers are seen, installs an `AdaptiveFifo` that
 * ADOPTS that ring as part 0: no element moves, nothing is copied.
 *
 * The signal is DIFFERENT PRODUCERS, not fullness (fullness is
 * backpressure one producer causes alone), SAMPLED: a plain counter per
 * push and every 64th compares the pushing thread with the last one
 * sampled — racy on purpose, it only has to be right eventually. The
 * counter lives off this object with padding, so a producer's store does
 * not share a line with the consumer's load of `inner` (1.200x -> 1.109x
 * the ring in the Scala 3 core), and stops once grown.
 *
 * A push the channel makes ON BEHALF of a parked producer runs on the
 * waker's thread and teaches this nothing, in either direction
 * (`pushDecidingAtOnBehalf`): read as evidence it made one producer look
 * like two and grew the buffer in ten runs out of thirty.
 *
 * WHAT IT GIVES UP: a producer's own order in AT MOST ONE PLACE, ONCE,
 * across the swap — the laws say exactly that for this buffer. A caller
 * who needs exact order asks for `Queues.strong[A].adaptive` (no
 * adoption) or `fifo` (one ring).
 */
final class Growing[A >: Null](initial: Buffer[A], cap: Int, each: () => Buffer[A]) extends Buffer[A] {

  /**
   * THE STATE, one reference: the open ring, the grown `AdaptiveFifo`,
   * or the ring SEALED — and sealing and growing are ONE decision, a CAS
   * away from the open state (okay2-channel-close-wakeup). They were two:
   * `seal` put the end mark into whatever `inner` was, and a producer
   * already past its open check could grow the ring AFTER that — the
   * adopted ring kept its one end mark, a fresh part got none, and the
   * channel, counting `parts` end marks, parked its receiver for good
   * (TestChannelLaws law 1b, twice under load; TestGrowingSeal, always).
   * Now a seal that wins over the open ring rules growth out, and a
   * growth that won first means the seal goes to the `AdaptiveFifo`,
   * which freezes and seals every part it opened.
   */
  private val open: Growing.St[A] = new Growing.St[A](initial, grown = false)
  private val state = new AtomicReference[Growing.St[A]](open)

  /** read ONCE per operation into a local: two reads could straddle the
   * swap and compare a ring against a part of itself */
  private def inner: Buffer[A] = state.get.buf
  private def grownYet: Boolean = state.get.grown
  @volatile private var sampled: Thread = null

  private final class Counter {
    var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Long = 0L
    var seen: Int = 0
    /** a racy hint that the one swap happened; the decision re-reads `grown` */
    var doneGrowing: Boolean = false
    var pad7, pad8, pad9, pad10, pad11, pad12, pad13: Long = 0L
  }
  private val counter = new Counter

  /** one swap, ever — and never after a seal; losers use the winner's buffer */
  private def grow(): Buffer[A] = {
    counter.doneGrowing = true
    if (state.get ne open) inner
    else {
      val partitioned = new AdaptiveFifo[A](cap, each, eager = false, first = initial)
      if (state.compareAndSet(open, new Growing.St[A](partitioned, grown = true))) partitioned else inner
    }
  }

  private def sample(): Unit =
    if (!counter.doneGrowing) {
      val n = counter.seen + 1
      counter.seen = n
      if ((n & 63) == 0 && !grownYet) {
        val me = Thread.currentThread()
        val last = sampled
        if (last == null) sampled = me
        else if (!(last eq me)) { val _ = grow() }
      }
    }

  /** the refusal path grows it when a SECOND producer is blocked behind a
   * full part — backpressure AND contention */
  private def refused(): Buffer[A] =
    if (grownYet) inner
    else {
      val me = Thread.currentThread()
      val last = sampled
      if (last == null) { sampled = me; null }
      else if (last eq me) null
      else grow()
    }

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
  override def pop(): A = inner.pop()
  override def popMany(max: Int)(sink: A => Unit): Int = inner.popMany(max)(sink)
  /** the seal wins over the open ring by the same CAS a growth needs */
  override def seal(mark: A): Int =
    if (state.compareAndSet(open, new Growing.St[A](initial, grown = true))) initial.seal(mark)
    else inner.seal(mark)

  override def push(a: A): Boolean = {
    sample()
    val b = inner
    if (b.push(a)) true
    else {
      val grownTo = refused()
      if (grownTo == null) false else grownTo.push(a)
    }
  }

  /** a route taken before the swap names part 0, which after it is the
   * adopted, drain-only part; on our own thread ask again */
  private def ours(b: Buffer[A], route: Int): Int = if (grownYet) b.route() else route

  override def pushAt(route: Int, a: A): Boolean = {
    val b = inner
    if (b.pushAt(ours(b, route), a)) true
    else {
      val grownTo = refused()
      if (grownTo == null) false else grownTo.pushAt(grownTo.route(), a)
    }
  }

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A = {
    sample()
    val out = inner.pushDeciding(a, unless, orElse)
    if (out != null) out
    else {
      val grownTo = refused()
      if (grownTo == null) null else grownTo.pushDeciding(a, unless, orElse)
    }
  }

  override def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A = {
    sample()
    val b = inner
    val out = b.pushDecidingAt(ours(b, route), a, unless, orElse)
    if (out != null) out
    else {
      val grownTo = refused()
      if (grownTo == null) null
      else grownTo.pushDecidingAt(grownTo.route(), a, unless, orElse)
    }
  }

  /** pushed for a parked producer from the waker's thread: no sample, and
   * a refusal does not grow */
  override def pushDecidingAtOnBehalf(route: Int, a: A, unless: AtomicBoolean, orElse: A): A =
    inner.pushDecidingAtOnBehalf(route, a, unless, orElse)

  override def pushMany(n: Int)(src: Int => A): Int = {
    val took = inner.pushMany(n)(src)
    if (took > 0 || n == 0) took
    else {
      val grownTo = refused()
      if (grownTo == null) 0 else grownTo.pushMany(n)(src)
    }
  }
}

object Growing {
  /** the buffer in force and whether the ring's one chance to grow is
   * spent (grown, or sealed as it was) */
  private[stream] final class St[A >: Null](val buf: Buffer[A], val grown: Boolean)
}
