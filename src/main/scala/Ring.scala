package okay

import java.util.concurrent.atomic.{AtomicLong, AtomicLongArray, AtomicReferenceArray}

/**
 * A bounded lock-free MPMC queue: the allocation-free half of a
 * channel.
 *
 * WHY THIS EXISTS. `Channel.State` is an immutable value that
 * `TRef.modify` rebuilds on every operation — a new `Queue` (cons
 * cells, an amortized `List.reverse`), a new case class, then a CAS
 * of the whole thing. Three lanes (channel-queue-reversal's `Vector`
 * and hand-rolled `Fifo`, channel-cas-contention) swapped the
 * structure INSIDE that model and every one of them failed, because
 * the rebuild is the cost and not the structure being rebuilt.
 * `zio.Queue` pays none of it: verified in its own sources
 * (`zio/internal/RingBuffer.scala`, `RingBufferPow2.scala`), it is a
 * mutable array of slots with CAS'd positions and no allocation per
 * operation, which is where its 3.6x on a buffered channel comes from
 * (idiomatic-api-compare).
 *
 * THE TWO CONSTRAINTS, and how each is met without compromise.
 *
 * No CASTS. The slots are an `AtomicReferenceArray[A]`, which is
 * generic in Java — `get(i): A`, `compareAndSet(i, A, A)` — so an
 * element is stored and read back at its own type with nothing
 * asserted. That is why it is not `Array[?]`: `ChunkBuf` can live
 * with an existential array because it never reads ONE element back
 * (it writes, then hands out the whole array through the single
 * commented cast in `wrap`); a ring must read one at a time, so the
 * existential would force a cast per `poll`. The atomic array also
 * carries the memory ordering a plain array would need a `VarHandle`
 * for.
 *
 * No THREAD BLOCKING. Every operation is a short CAS loop that either
 * succeeds or answers "full"/"empty" to its caller — it never parks,
 * never spins waiting on another thread's progress, and always makes
 * progress if any thread does (lock-free). Waiting stays where it
 * already is in this library: a callback registered with the channel,
 * fired after the operation.
 *
 * THE ALGORITHM is Vyukov's bounded MPMC queue, which is also what
 * ZIO's ring uses (their `seq` array, ours `stamp`). Each slot
 * carries a sequence number saying whose turn it is:
 *
 *   - a slot is WRITABLE for push position `p` when `stamp == p`
 *   - a push publishes `stamp = p + 1`, which is exactly the state a
 *     pop at position `p` waits for
 *   - a pop publishes `stamp = p + capacity`, the state the NEXT lap's
 *     push at that slot waits for
 *
 * The stamp is what makes the two-step "advance the position, then
 * write the slot" safe: a consumer cannot see an advanced tail before
 * the element is published, because it does not look at the tail — it
 * looks at the stamp of the slot it wants.
 */
private[okay] final class Ring[A](requested: Int) {

  /**
   * Capacity is rounded UP to a power of two so the index is a mask
   * rather than a division — the reason ZIO ships `RingBufferPow2`
   * separately from the arbitrary-capacity one — and is at LEAST two.
   *
   * One is not a ring. The stamp scheme says a slot is writable at
   * position `p` when `stamp == p`, and an un-popped slot holds
   * `stamp = p + 1` against a next-lap push at `p + capacity`, so the
   * difference is `1 - capacity`: negative (correctly full) for every
   * capacity but one, and zero (wrongly writable) at one. Found by
   * the boundary test, not reasoned about afterwards. ZIO draws the
   * same line — `MutableConcurrentQueue.bounded` hands capacity 1 to
   * a separate `OneElementConcurrentQueue` rather than to the ring.
   * Here a channel that small stays on the gate path instead.
   */
  val capacity: Int =
    if requested <= 2 then 2
    else Integer.highestOneBit(requested - 1) << 1

  private val mask = capacity - 1
  // the slot type IS nullable, rather than an `A` array with a cast
  // pretending an empty slot holds one: an empty slot really does
  // hold nothing, and saying so keeps `pop`'s clear-the-reference
  // step honest (`null.asInstanceOf[A]` was the first draft and is
  // exactly the cast this design refuses)
  private val slots = AtomicReferenceArray[A | Null](capacity)
  private val stamp = AtomicLongArray(capacity)
  private val head = AtomicLong(0L)   // next position to pop
  private val tail = AtomicLong(0L)   // next position to push

  // a slot's stamp starts at its own index: the state a push at that
  // position is waiting for
  private def initStamps(): Unit =
    var i = 0
    while i < capacity do { stamp.set(i, i.toLong); i += 1 }
  initStamps()

  /** how many elements are in the ring right now — a snapshot, and
   * only ever used for reporting, never for a decision */
  def size: Int =
    val n = tail.get - head.get
    if n < 0 then 0 else if n > capacity then capacity else n.toInt

  def isEmpty: Boolean = head.get >= tail.get

  /**
   * Take a slot at the tail and publish `a` into it. False means the
   * ring was full at some point during the attempt — the caller
   * decides what that means (park a sender, drop, refuse).
   */
  def push(a: A): Boolean =
    var done = false
    var full = false
    while !done && !full do
      val pos = tail.get
      val i = (pos & mask).toInt
      val st = stamp.get(i)
      val d = st - pos
      if d == 0 then
        // the slot is ours if we win the position
        if tail.compareAndSet(pos, pos + 1) then
          slots.set(i, a)
          stamp.set(i, pos + 1)   // publish: a pop at `pos` may proceed
          done = true
      else if d < 0 then full = true   // the lap ahead has not been popped
      // d > 0: another pusher won this position, look again
    done

  /**
   * Take the element at the head, or `null` — which is why this is
   * private to the library and wrapped by a typed caller: `Option`
   * here would allocate per element and undo the point of the ring.
   * The public surface never sees this shape.
   */
  def pop(): A | Null =
    var out: A | Null = null
    var empty = false
    while out == null && !empty do
      val pos = head.get
      val i = (pos & mask).toInt
      val st = stamp.get(i)
      val d = st - (pos + 1)
      if d == 0 then
        if head.compareAndSet(pos, pos + 1) then
          val a = slots.get(i)
          slots.set(i, null)                    // release the reference
          stamp.set(i, pos + capacity)          // publish: next lap may push
          out = a
      else if d < 0 then empty = true
    out

  /**
   * Claim up to `max` published slots with ONE move of the head, and
   * hand each element to `sink` in order. Answers how many.
   *
   * This is the difference between batching the handshake and
   * batching the QUEUE. Calling `pop` in a loop already amortized the
   * callback, the parking and the boxing across a batch -- measured,
   * 4000 handshakes became 299 -- and bought 4%, because the ring
   * still paid a head CAS per element and that CAS was a quarter of
   * the profile. Here the scan finds a run of consecutive published
   * slots, one `compareAndSet` takes the whole run, and only the
   * slot read and its stamp stay per element, because those carry the
   * data.
   *
   * Safe for the same reason the single pop is: a position is ours
   * only once we have won the head, and we only count positions whose
   * stamp says a push already published them. A producer parked on
   * one of those slots waits for its stamp, which we publish as we
   * go, so it never sees a slot released before its element is out.
   */
  def popMany(max: Int)(sink: A => Unit): Int =
    val limit = if max < capacity then max else capacity
    var n = 0
    var pos = 0L
    var claimed = false
    while !claimed do
      pos = head.get
      var k = 0
      var scanning = true
      while scanning && k < limit do
        val i = ((pos + k) & mask).toInt
        if stamp.get(i) - (pos + k + 1) == 0 then k += 1 else scanning = false
      if k == 0 then { n = 0; claimed = true }
      else if head.compareAndSet(pos, pos + k) then { n = k; claimed = true }
      // else another consumer moved the head; scan again from where
      // it left it
    var j = 0
    while j < n do
      val i = ((pos + j) & mask).toInt
      val a = slots.get(i)
      slots.set(i, null)
      stamp.set(i, pos + j + capacity)
      sink(a.nn)
      j += 1
    n
}
