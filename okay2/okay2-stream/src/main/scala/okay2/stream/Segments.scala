package okay2.stream

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicLongArray, AtomicReference, AtomicReferenceArray}

/**
 * An UNBOUNDED buffer with a ring's arithmetic — okay-stream's
 * Segments.scala: one linked list of fixed arrays behind one pair of
 * position counters. `Int.MaxValue` is not an array, so the unbounded
 * channel needs this rather than a `Ring`.
 *
 * NO RECLAMATION, by design: a segment is never reused or freed by hand,
 * a thread reaches one by holding a reference, and a segment nobody
 * holds is garbage — the collector reclaims and the use-after-free
 * hazard of a segmented queue has nowhere to live.
 *
 * Every position is used exactly once, so a slot's stamp needs one bit:
 * `stamp(i) == p + 1` is "published", a fresh segment's zeros "not yet".
 */
final class Segments[A >: Null](segShift: Int = 8) extends Buffer[A] {

  private val SegSize = 1 << segShift
  private val SegMask = (SegSize - 1).toLong

  private final class Segment(val id: Long) {
    val slots = new AtomicReferenceArray[A](SegSize)
    val stamp = new AtomicLongArray(SegSize)
    val next = new AtomicReference[Segment](null)
  }

  private val first = new Segment(0L)
  private val head = new AtomicLong(0L)
  private val tail = new AtomicLong(0L)
  /** where each side last looked — hints: a stale one costs a walk */
  private val headSeg = new AtomicReference[Segment](first)
  private val tailSeg = new AtomicReference[Segment](first)

  override val capacity: Int = Int.MaxValue

  /**
   * Walk (extending as needed) to the segment holding `id`. A hint can be
   * AHEAD — producers claim with one `getAndIncrement` and publish out of
   * order — and there is no backward link, so a walk from past its target
   * would write into another position's slot. The head is the anchor
   * that is never ahead of a position still being published.
   */
  private def segmentFor(hint: AtomicReference[Segment], id: Long): Segment = {
    var s = hint.get
    if (s.id > id) s = headSeg.get
    while (s.id < id) {
      val n = s.next.get
      if (n != null) s = n
      else {
        val fresh = new Segment(s.id + 1)
        if (s.next.compareAndSet(null, fresh)) s = fresh
        else s = s.next.get
      }
    }
    // forward only, so a slower thread cannot drag the hint back
    val cur = hint.get
    if (s.id > cur.id) { val _ = hint.compareAndSet(cur, s) }
    s
  }

  private def publish(p: Long, v: A): Unit = {
    val seg = segmentFor(tailSeg, p >> segShift)
    val i = (p & SegMask).toInt
    seg.slots.set(i, v)
    seg.stamp.set(i, p + 1)
  }

  override def push(a: A): Boolean = { publish(tail.getAndIncrement(), a); true }

  /** one atomic wins the position — unbounded, so it can never be refused */
  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A = {
    val p = tail.getAndIncrement()
    val v = if (unless.get) orElse else a
    publish(p, v)
    v
  }

  override def pushMany(n: Int)(src: Int => A): Int =
    if (n <= 0) 0
    else {
      val base = tail.getAndAdd(n.toLong)
      var i = 0
      while (i < n) { publish(base + i, src(i)); i += 1 }
      n
    }

  override def pop(): A = {
    var out: A = null
    var empty = false
    while (out == null && !empty) {
      val p = head.get
      val seg = segmentFor(headSeg, p >> segShift)
      val i = (p & SegMask).toInt
      if (seg.stamp.get(i) == p + 1) {
        if (head.compareAndSet(p, p + 1)) {
          out = seg.slots.get(i)
          seg.slots.set(i, null)
        }
      } else empty = true   // unclaimed, or claimed and unpublished: nothing ready
    }
    out
  }

  /** the segment the SCAN used is kept, not re-derived after the CAS:
   * a second consumer can advance `headSeg` past this run in between
   * (the Scala 3 core's defect, a consumer killed by a null slot) */
  override def popMany(max: Int)(sink: A => Unit): Int = {
    var n = 0
    var pos = 0L
    var start: Segment = first
    var claimed = false
    while (!claimed) {
      pos = head.get
      var seg = segmentFor(headSeg, pos >> segShift)
      start = seg
      var k = 0
      var scanning = true
      while (scanning && k < max) {
        val p = pos + k
        val id = p >> segShift
        if (id != seg.id) {
          val nxt = seg.next.get
          if (nxt == null) scanning = false else seg = nxt
        }
        if (scanning && id == seg.id) {
          if (seg.stamp.get((p & SegMask).toInt) == p + 1) k += 1
          else scanning = false
        }
      }
      if (k == 0) { n = 0; claimed = true }
      else if (head.compareAndSet(pos, pos + k)) { n = k; claimed = true }
    }
    var j = 0
    var seg = start
    while (j < n) {
      val p = pos + j
      if ((p >> segShift) != seg.id) seg = seg.next.get
      val i = (p & SegMask).toInt
      val a = seg.slots.get(i)
      seg.slots.set(i, null)
      sink(a)
      j += 1
    }
    n
  }

  override def size: Int = {
    val n = tail.get - head.get
    if (n < 0) 0 else if (n > Int.MaxValue) Int.MaxValue else n.toInt
  }

  override def isEmpty: Boolean = head.get >= tail.get

  override def hasRoom: Boolean = true

  override def hasReady: Boolean = {
    val p = head.get
    val seg = segmentFor(headSeg, p >> segShift)
    seg.stamp.get((p & SegMask).toInt) == p + 1
  }
}
