package okay2.stream

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicLongArray, AtomicReferenceArray}

/**
 * A bounded lock-free MPMC queue — okay-stream's Ring.scala: Vyukov's
 * bounded queue, which is also what ZIO's ring is. Each slot carries a
 * stamp saying whose turn it is: writable for push position `p` when
 * `stamp == p`; a push publishes `p + 1`, which is what a pop at `p`
 * waits for; a pop publishes `p + capacity`, the next lap's push. A
 * consumer never looks at the tail, only at the stamp of the slot it
 * wants, so "advance the position, then write the slot" is safe.
 *
 * No casts: the slots are an `AtomicReferenceArray[A]`, generic in Java,
 * and `A >: Null` lets an empty slot be null at its own type.
 *
 * SINGLE CONSUMER, decided at construction: the head is then that
 * consumer's private cursor and a release store moves it where `pop`
 * otherwise pays a CAS per element (35% of an elementwise consumer's
 * profile in the Scala 3 core). A promise the caller makes.
 */
final class Ring[A >: Null](requested: Int, singleConsumer: Boolean = false) extends Buffer[A] {

  /** rounded UP to a power of two (a mask, not a division), and at least
   * two: at one the stamp difference of an unpopped slot against the
   * next lap is zero — wrongly writable (found by the boundary test) */
  override val capacity: Int =
    if (requested <= 2) 2
    else Integer.highestOneBit(requested - 1) << 1

  private val mask = capacity - 1
  private val slots = new AtomicReferenceArray[A](capacity)
  private val stamp = new AtomicLongArray(capacity)
  private val head = new AtomicLong(0L)
  private val tail = new AtomicLong(0L)

  locally {
    var i = 0
    while (i < capacity) { stamp.set(i, i.toLong); i += 1 }
  }

  override def size: Int = {
    val n = tail.get - head.get
    if (n < 0) 0 else if (n > capacity) capacity else n.toInt
  }

  override def isEmpty: Boolean = head.get >= tail.get

  override def hasRoom: Boolean = {
    val pos = tail.get
    stamp.get((pos & mask).toInt) - pos == 0
  }

  override def hasReady: Boolean = {
    val pos = head.get
    stamp.get((pos & mask).toInt) - (pos + 1) == 0
  }

  override def push(a: A): Boolean = {
    var done = false
    var full = false
    while (!done && !full) {
      val pos = tail.get
      val i = (pos & mask).toInt
      val d = stamp.get(i) - pos
      if (d == 0) {
        if (tail.compareAndSet(pos, pos + 1)) {
          slots.set(i, a)
          stamp.set(i, pos + 1)
          done = true
        }
      } else if (d < 0) full = true
    }
    done
  }

  /** the test runs AFTER the position is won, so "accepted" and "ordered"
   * are one instant and a closing channel has no window between them */
  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A = {
    var out: A = null
    var full = false
    while (out == null && !full) {
      val pos = tail.get
      val i = (pos & mask).toInt
      val d = stamp.get(i) - pos
      if (d == 0) {
        if (tail.compareAndSet(pos, pos + 1)) {
          val v = if (unless.get) orElse else a
          slots.set(i, v)
          stamp.set(i, pos + 1)
          out = v
        }
      } else if (d < 0) full = true
    }
    out
  }

  /** a run of writable slots claimed by ONE move of the tail */
  override def pushMany(n: Int)(src: Int => A): Int = {
    val limit = if (n < capacity) n else capacity
    var took = 0
    var pos = 0L
    var claimed = false
    while (!claimed) {
      pos = tail.get
      var k = 0
      var scanning = true
      while (scanning && k < limit) {
        val i = ((pos + k) & mask).toInt
        if (stamp.get(i) - (pos + k) == 0) k += 1 else scanning = false
      }
      if (k == 0) { took = 0; claimed = true }
      else if (tail.compareAndSet(pos, pos + k)) { took = k; claimed = true }
    }
    var j = 0
    while (j < took) {
      val i = ((pos + j) & mask).toInt
      slots.set(i, src(j))
      stamp.set(i, pos + j + 1)
      j += 1
    }
    took
  }

  override def pop(): A = {
    var out: A = null
    var empty = false
    while (out == null && !empty) {
      val pos = head.get
      val i = (pos & mask).toInt
      val d = stamp.get(i) - (pos + 1)
      if (d == 0) {
        val won =
          if (singleConsumer) { head.lazySet(pos + 1); true }
          else head.compareAndSet(pos, pos + 1)
        if (won) {
          val a = slots.get(i)
          slots.set(i, null)
          stamp.set(i, pos + capacity)
          out = a
        }
      } else if (d < 0) empty = true
    }
    out
  }

  /** a run of published slots claimed by ONE move of the head */
  override def popMany(max: Int)(sink: A => Unit): Int = {
    val limit = if (max < capacity) max else capacity
    var n = 0
    var pos = 0L
    var claimed = false
    while (!claimed) {
      pos = head.get
      var k = 0
      var scanning = true
      while (scanning && k < limit) {
        val i = ((pos + k) & mask).toInt
        if (stamp.get(i) - (pos + k + 1) == 0) k += 1 else scanning = false
      }
      if (k == 0) { n = 0; claimed = true }
      else if (singleConsumer) { head.lazySet(pos + k); n = k; claimed = true }
      else if (head.compareAndSet(pos, pos + k)) { n = k; claimed = true }
    }
    var j = 0
    while (j < n) {
      val i = ((pos + j) & mask).toInt
      val a = slots.get(i)
      slots.set(i, null)
      stamp.set(i, pos + j + capacity)
      sink(a)
      j += 1
    }
    n
  }
}
