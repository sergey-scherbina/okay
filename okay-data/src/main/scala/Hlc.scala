package okay

import java.util.concurrent.atomic.AtomicLong

/**
 * A HYBRID LOGICAL CLOCK (specs/coordination-free.md).
 *
 * WHY IT EXISTS, and why it is one thing rather than two. A
 * last-write-wins register needs a timestamp that keeps increasing
 * when the physical clock does not. A monotonic, sortable id needs
 * exactly the same thing. Written twice they would drift apart;
 * written once, the harder half is paid for by both.
 *
 * WHAT IS HYBRID ABOUT IT. A purely logical clock (Lamport) orders
 * events but its numbers mean nothing to a human and cannot be
 * compared with a wall clock. A purely physical clock means something
 * and lies: NTP steps it, a VM suspend jumps it, and
 * `System.currentTimeMillis` is explicitly not monotonic. This keeps
 * the physical reading when it moves forward and a counter when it
 * does not, so a stamp is BOTH readable as a time and safe to order
 * by.
 *
 * THE LAYOUT: one `Long`, 48 bits of Unix milliseconds then 16 of
 * counter. Comparison is therefore a plain `Long` comparison, which
 * is the point of packing it — an `Ordering` over a pair would box on
 * every compare, and this value is compared far more often than it is
 * made. 48 bits of milliseconds runs to the year 10889; 16 bits of
 * counter is 65 536 stamps inside one millisecond, and what happens
 * beyond that is stated below rather than left to overflow.
 *
 * THE CLOCK IS A PARAMETER, deliberately. The hazard this class
 * exists to survive is time going backwards, and a design whose
 * central hazard cannot be reached from a test is not designed. `at`
 * takes the source; `Hlc.next()` is the convenience over the system
 * clock for callers who do not care.
 *
 * WHAT IT DOES NOT GIVE. Not a global physical time. Two nodes that
 * have never exchanged a stamp can order their concurrent writes
 * either way — which is what "last write wins" has always meant, and
 * why `LwwRegister` is one choice among several rather than the
 * answer.
 */
object Hlc:

  /** 48 bits of milliseconds, 16 of counter, in one Long. Opaque so
   * nobody does arithmetic on it; ordered, because ordering it is the
   * whole purpose. */
  opaque type Stamp = Long

  final val CounterBits = 16
  final val MaxCounter = (1 << CounterBits) - 1
  /** the last millisecond this layout can express: year 10889 */
  final val MaxMillis = (1L << 48) - 1

  extension (s: Stamp)
    /** the physical half, Unix milliseconds */
    def millis: Long = s >>> CounterBits
    /** the logical half: how many stamps have been issued inside
     * `millis` already */
    def counter: Int = (s & MaxCounter.toLong).toInt
    def toLong: Long = s

  /** rebuild a stamp that travelled as a number */
  def fromLong(l: Long): Stamp = l

  def apply(millis: Long, counter: Int): Stamp =
    require(millis >= 0 && millis <= MaxMillis, s"millis out of range: $millis")
    require(counter >= 0 && counter <= MaxCounter, s"counter out of range: $counter")
    (millis << CounterBits) | counter.toLong

  /** Stamps are compared as unsigned-in-practice Longs: the top 16
   * bits of a 48-bit millisecond value are zero, so a signed compare
   * is already the right one and costs nothing. */
  given Ordering[Stamp] = Ordering.Long

  /**
   * A clock instance. Holds the last stamp it issued and never
   * returns anything smaller — the single `AtomicLong` is the whole
   * state, and CAS on it is what makes `next` safe from several
   * threads without a lock.
   *
   * `AtomicLong` rather than anything richer on purpose: it is the
   * one atomic Scala Native's JDK subset implements everywhere, and
   * `Ring` in this same package already relies on it
   * (AdaptiveFifo lost a day to `AtomicIntegerArray` not existing).
   */
  final class Clock private[okay] (source: () => Long, counterBits: Int):

    require(counterBits >= 1 && counterBits <= CounterBits,
      s"counterBits must be 1..$CounterBits, got $counterBits")

    /**
     * How far the counter runs before a millisecond is borrowed. The
     * FIELD is always 16 bits wide so `Stamp` has one layout and its
     * accessors never need to know who made it; a clock may simply
     * choose to use fewer, which is what `Uid` does — its 128-bit
     * layout has room for twelve.
     */
    private val maxCounter = (1 << counterBits) - 1

    private val last = AtomicLong(0L)

    /**
     * The next stamp: at least one more than the last one issued, and
     * carrying the physical reading whenever that has moved on.
     *
     * The loop is a CAS retry, and the three cases inside it are the
     * whole algorithm:
     *   - the physical clock moved forward -> take it, counter 0
     *   - it did not (same millisecond, or it went BACKWARDS) ->
     *     keep the last millisecond and add one to the counter
     *   - the counter is full -> borrow from the next millisecond
     *
     * That last case is why this cannot fail or block. 65 536 stamps
     * in one millisecond is 65 million a second; a caller that fast
     * borrows from the future by a millisecond and keeps going, which
     * is a smaller lie than blocking or refusing, and it repairs
     * itself as soon as the physical clock catches up.
     */
    def next(): Stamp =
      var out = 0L
      var done = false
      while !done do
        val prev = last.get
        val now = source()
        val cand =
          if now > prev.millis then Hlc(now, 0)
          else if prev.counter < maxCounter then Hlc(prev.millis, prev.counter + 1)
          else Hlc(prev.millis + 1, 0)   // counter full: borrow a millisecond
        if last.compareAndSet(prev, cand) then { out = cand; done = true }
      out

    /**
     * Take account of a stamp made elsewhere. THIS is the hybrid
     * part: after observing a remote stamp, everything this clock
     * issues sorts above it, so a reply is ordered after the message
     * it answers even when the two machines' clocks disagree.
     *
     * Returns the stamp this clock now issues, so a caller that
     * wants to stamp its reply does not need a second call.
     */
    def observe(remote: Stamp): Stamp =
      var out = 0L
      var done = false
      while !done do
        val prev = last.get
        val now = source()
        val high = if remote > prev then remote else prev
        val cand =
          if now > high.millis then Hlc(now, 0)
          else if high.counter < maxCounter then Hlc(high.millis, high.counter + 1)
          else Hlc(high.millis + 1, 0)
        if last.compareAndSet(prev, cand) then { out = cand; done = true }
      out

    /** the last stamp issued, without issuing one */
    def peek: Stamp = last.get

  /** a clock over a source of milliseconds — the testable door.
   * `counterBits` narrows how many stamps one millisecond holds
   * before the next is borrowed; `Uid` asks for twelve because that
   * is what fits beside a UUIDv7's version and variant. */
  def at(source: () => Long, counterBits: Int = CounterBits): Clock =
    Clock(source, counterBits)

  /** the ambient clock over the system's wall time */
  val system: Clock = Clock(() => System.currentTimeMillis(), CounterBits)

  /** the next stamp from the ambient clock */
  def next(): Stamp = system.next()

  /** merge a remote stamp into the ambient clock */
  def observe(remote: Stamp): Stamp = system.observe(remote)
