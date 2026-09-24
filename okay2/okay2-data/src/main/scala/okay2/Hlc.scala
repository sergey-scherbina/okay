package okay2

import java.util.concurrent.atomic.AtomicLong

/**
 * A HYBRID LOGICAL CLOCK — the Scala 3 core's okay-data `Hlc`
 * (specs/coordination-free.md there). One timestamp that keeps
 * increasing when the physical clock does not: the physical reading
 * when it moves forward, a counter when it does not, so a stamp is both
 * readable as a time and safe to order by. A last-write-wins register
 * and a sortable id (`Uid`) need exactly this, so it is written once.
 *
 * THE LAYOUT: one `Long`, 48 bits of Unix milliseconds then 16 of
 * counter, so comparison is a plain `Long` comparison. 48 bits of
 * milliseconds runs to the year 10889; 16 bits of counter is 65 536
 * stamps inside one millisecond, and beyond that a millisecond is
 * borrowed rather than overflowed.
 *
 * THE CLOCK IS A PARAMETER: the hazard this exists to survive is time
 * going backwards, and only a test that can move the clock reaches it.
 * `at` takes the source; `Hlc.next()` is the convenience over the
 * system clock.
 */
object Hlc {

  /** 48 bits of milliseconds, 16 of counter, in one Long. The core's
   * opaque type is a value class here: no arithmetic on it from
   * outside, and no allocation where it is used at its own type */
  final class Stamp private[Hlc] (val toLong: Long) extends AnyVal {
    /** the physical half, Unix milliseconds */
    def millis: Long = toLong >>> CounterBits
    /** the logical half: how many stamps were issued inside `millis` already */
    def counter: Int = (toLong & MaxCounter.toLong).toInt
    override def toString: String = s"Stamp($millis, $counter)"
  }

  object Stamp {
    /** a signed compare is already the right one: the top 16 bits of a
     * 48-bit millisecond value are zero */
    implicit val ordering: Ordering[Stamp] = new Ordering[Stamp] {
      def compare(a: Stamp, b: Stamp): Int = java.lang.Long.compare(a.toLong, b.toLong)
    }
  }

  final val CounterBits = 16
  final val MaxCounter = (1 << CounterBits) - 1
  /** the last millisecond this layout can express: year 10889 */
  final val MaxMillis = (1L << 48) - 1

  /** rebuild a stamp that travelled as a number */
  def fromLong(l: Long): Stamp = new Stamp(l)

  def apply(millis: Long, counter: Int): Stamp = {
    require(millis >= 0 && millis <= MaxMillis, s"millis out of range: $millis")
    require(counter >= 0 && counter <= MaxCounter, s"counter out of range: $counter")
    new Stamp((millis << CounterBits) | counter.toLong)
  }

  /**
   * A clock instance: the last stamp it issued, in one `AtomicLong`, and
   * never anything smaller — CAS on it makes `next` safe from several
   * threads without a lock. A clock may use fewer counter bits than the
   * field holds (`Uid` uses twelve); the layout stays one.
   */
  final class Clock private[okay2] (source: () => Long, counterBits: Int) {
    require(counterBits >= 1 && counterBits <= CounterBits, s"counterBits must be 1..$CounterBits, got $counterBits")

    private val maxCounter = (1 << counterBits) - 1
    private val last = new AtomicLong(0L)

    /** the three cases are the algorithm: the physical clock moved
     * forward -> take it, counter 0; it did not (or went BACKWARDS) ->
     * keep the millisecond, counter + 1; the counter is full -> borrow
     * the next millisecond, a smaller lie than blocking or failing */
    private def after(high: Long, now: Long): Long = {
      val h = new Stamp(high)
      if (now > h.millis) Hlc(now, 0).toLong
      else if (h.counter < maxCounter) Hlc(h.millis, h.counter + 1).toLong
      else Hlc(h.millis + 1, 0).toLong
    }

    /** the next stamp: above the last one issued */
    def next(): Stamp = {
      var out = 0L
      var done = false
      while (!done) {
        val prev = last.get
        val cand = after(prev, source())
        if (last.compareAndSet(prev, cand)) { out = cand; done = true }
      }
      new Stamp(out)
    }

    /** take account of a stamp made elsewhere — THE hybrid part: from
     * now on this clock issues stamps above it, so a reply sorts after
     * the message it answers even when the machines' clocks disagree.
     * Answers the stamp issued, for the reply. */
    def observe(remote: Stamp): Stamp = {
      var out = 0L
      var done = false
      while (!done) {
        val prev = last.get
        val high = if (remote.toLong > prev) remote.toLong else prev
        val cand = after(high, source())
        if (last.compareAndSet(prev, cand)) { out = cand; done = true }
      }
      new Stamp(out)
    }

    /** the last stamp issued, without issuing one */
    def peek: Stamp = new Stamp(last.get)
  }

  /** a clock over a source of milliseconds — the testable door */
  def at(source: () => Long, counterBits: Int = CounterBits): Clock = new Clock(source, counterBits)

  /** the ambient clock over the system's wall time */
  val system: Clock = new Clock(() => System.currentTimeMillis(), CounterBits)

  /** the next stamp from the ambient clock */
  def next(): Stamp = system.next()

  /** merge a remote stamp into the ambient clock */
  def observe(remote: Stamp): Stamp = system.observe(remote)
}
