package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicLongArray, AtomicReference, AtomicReferenceArray}

/**
 * An UNBOUNDED buffer with a ring's arithmetic: one linked list of
 * fixed arrays behind one pair of position counters.
 *
 * WHY IT EXISTS. `Channel.apply` defaults to unbounded, and a `Ring`
 * cannot be — `Int.MaxValue` is not an array. So the unbounded case
 * stayed on `StmChannel`, and that was the last lane behind
 * `zio.Queue`: 130.6us against 122.2 at chunk granularity, 40% of its
 * leaf samples in `List.reverse`. `immutable.Queue` is two lists with
 * an amortized reverse, and the amortization does not survive a bulk
 * receive — the producer refills `in` while the consumer drains
 * `out`, so the reverses total one traversal per element. Three
 * earlier lanes tried swapping the structure inside that model and
 * every one failed, because the per-transaction rebuild is the cost
 * and not the thing being rebuilt.
 *
 * NO RECLAMATION, and that is the design, not an omission. A
 * segmented queue's classic hazard is freeing a segment while another
 * thread is still walking it — the same use-after-free shape that a
 * batched scan makes worse, since `popMany` reads a run that may
 * cross a boundary. Here a segment is never reused and never freed by
 * hand: a thread reaches one by holding a reference, and a segment
 * nobody holds is simply garbage. The collector does the reclamation
 * and the hazard has nowhere to live.
 *
 * THE STAMP IS SIMPLER THAN A RING'S. A bounded ring reuses each slot
 * every lap, so its stamp must say WHICH lap. Here every position is
 * used exactly once, so a slot needs one bit of state — published or
 * not — and `stamp(i) == p + 1` says it, with a fresh segment's zeros
 * meaning "not yet".
 */
private[okay] final class Segments[A](segShift: Int = 8) extends Buffer[A] {

  private final val SegSize = 1 << segShift
  private final val SegMask = (SegSize - 1).toLong

  private final class Segment(val id: Long):
    val slots = AtomicReferenceArray[A | Null](SegSize)
    // 0 means "no value published here yet"; a publish writes p + 1,
    // which is never 0 and is unique to this position
    val stamp = AtomicLongArray(SegSize)
    val next = AtomicReference[Segment | Null](null)

  private val first = Segment(0L)
  private val head = AtomicLong(0L)   // next position to pop
  private val tail = AtomicLong(0L)   // next position to claim
  // where each side last looked. Hints only: a stale one costs a
  // short walk, never a wrong answer
  private val headSeg = AtomicReference[Segment](first)
  private val tailSeg = AtomicReference[Segment](first)

  override val capacity: Int = Int.MaxValue

  /**
   * Walk (extending as needed) to the segment holding `id`.
   *
   * A hint can be BEHIND, which costs a short walk, and it can be
   * AHEAD, which would be silent corruption if unhandled — there is
   * no backward link, so a walk that starts past its target returns
   * the wrong segment and the caller writes an element into another
   * position's slot. It happens because producers claim positions
   * with one `getAndIncrement` and publish out of order: a thread
   * holding position 800 can find `tailSeg` already advanced to the
   * segment of 1100. Positions are monotone per THREAD, not between
   * threads — the reasoning that talked me out of this the first
   * time.
   *
   * The head is the anchor that is never ahead: `head` only advances
   * past positions whose stamp is already published, so for any
   * position still being published, `headSeg` sits at or before it.
   */
  private def segmentFor(hint: AtomicReference[Segment], id: Long): Segment =
    var s = hint.get
    if s.id > id then s = headSeg.get
    while s.id < id do
      val n = s.next.get
      if n != null then s = n.nn
      else
        val fresh = Segment(s.id + 1)
        if s.next.compareAndSet(null, fresh) then s = fresh
        else s = s.next.get.nn
    // publish the hint forward only, so a slower thread cannot drag
    // it back and make the next walk longer
    val cur = hint.get
    if s.id > cur.id then hint.compareAndSet(cur, s): Unit
    s

  private def publish(p: Long, v: A): Unit =
    val seg = segmentFor(tailSeg, p >> segShift)
    val i = (p & SegMask).toInt
    seg.slots.set(i, v)
    seg.stamp.set(i, p + 1)

  override def push(a: A): Boolean =
    publish(tail.getAndIncrement(), a)
    true

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    // one atomic wins the position -- no CAS loop, because unbounded
    // means the claim can never be refused
    val p = tail.getAndIncrement()
    val v = if unless.get then orElse else a
    publish(p, v)
    v

  override def pop(): A | Null =
    var out: A | Null = null
    var empty = false
    while out == null && !empty do
      val p = head.get
      val seg = segmentFor(headSeg, p >> segShift)
      val i = (p & SegMask).toInt
      if seg.stamp.get(i) == p + 1 then
        if head.compareAndSet(p, p + 1) then
          out = seg.slots.get(i)
          seg.slots.set(i, null)   // release the reference
      else
        // either nothing has been claimed here, or a claim is in
        // flight and not yet published: both mean "nothing ready"
        empty = true
    out

  override def popMany(max: Int)(sink: A => Unit): Int =
    var n = 0
    var pos = 0L
    // the segment the SCAN used, kept rather than re-derived. Deriving
    // it again after the CAS was the defect: a second consumer
    // advances `headSeg` past this run in between, and the
    // hint-is-ahead fallback cannot help, because the hint IS
    // `headSeg`. The read then walks a later segment and takes wrong
    // values or a null slot -- which killed the consumer with an
    // exception no one saw, inside a virtual thread. Holding the
    // reference is enough here precisely because segments are never
    // freed.
    var start: Segment = first
    var claimed = false
    while !claimed do
      pos = head.get
      var seg = segmentFor(headSeg, pos >> segShift)
      start = seg
      var k = 0
      var scanning = true
      while scanning && k < max do
        val p = pos + k
        val id = p >> segShift
        // a run may cross a boundary: step forward only when the next
        // segment already exists, since a scan must never build one
        if id != seg.id then
          val nxt = seg.next.get
          if nxt == null then scanning = false else seg = nxt.nn
        if scanning && id == seg.id then
          if seg.stamp.get((p & SegMask).toInt) == p + 1 then k += 1
          else scanning = false
      if k == 0 then { n = 0; claimed = true }
      else if head.compareAndSet(pos, pos + k) then { n = k; claimed = true }
      // else another consumer moved the head; scan again from there
    var j = 0
    var seg = start
    while j < n do
      val p = pos + j
      val id = p >> segShift
      if id != seg.id then seg = seg.next.get.nn
      val i = (p & SegMask).toInt
      val a = seg.slots.get(i)
      seg.slots.set(i, null)
      sink(a.nn)
      j += 1
    n

  override def size: Int =
    val n = tail.get - head.get
    if n < 0 then 0 else if n > Int.MaxValue then Int.MaxValue else n.toInt

  override def isEmpty: Boolean = head.get >= tail.get

  override def hasReady: Boolean =
    val p = head.get
    val seg = segmentFor(headSeg, p >> segShift)
    seg.stamp.get((p & SegMask).toInt) == p + 1
}
