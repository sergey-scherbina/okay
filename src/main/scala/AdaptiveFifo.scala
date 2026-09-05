package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReferenceArray}

/**
 * A buffer that grows PARTS as producers appear.
 *
 * WHY. Today's measurements say the right buffer depends on how many
 * producers there are: one ring wins at one producer, a relaxed one is
 * 19.6x faster at sixteen. The caller usually does not know that
 * number when the channel is made — it depends on how the program is
 * wired, sometimes on load. So this decides it by watching.
 *
 * WHAT MAY BE DECIDED AUTOMATICALLY, and what may not. Contract
 * (drain-on-close, STM composability) and boundedness are SEMANTICS:
 * a caller can see them in what their program does, and guessing them
 * would change behaviour behind the author's back. The part count is
 * not: the contract promises each producer's own order, no loss and
 * no duplication, and says nothing about the order BETWEEN producers.
 * That silence is what makes this legal, and it is stated here rather
 * than assumed.
 *
 * NOTHING EVER MIGRATES, which is the whole design. The obvious
 * version — notice contention, move to a partitioned buffer — has to
 * relocate the elements already buffered, and that is a
 * stop-the-world moment inside a lock-free structure. Here a new
 * producer simply gets a NEW part, empty, and every element stays
 * where it was written. One producer sees one part and pays a
 * one-part scan; sixteen producers grow sixteen parts between them.
 *
 * THE WINDOW THIS HAS TO CLOSE. Termination is a mark placed in every
 * part, so a part that appears AFTER close begins would never be
 * sealed and the stream would never end. Parts therefore stop being
 * created once closing has been seen, and a producer that arrives
 * then shares the last part rather than opening one. It is the same
 * shape as four earlier defects in this design — a question about one
 * part asked of the whole — so it is a law, not an argument.
 */
final class AdaptiveFifo[A](limit: Int, make: () => Buffer[A], eager: Boolean = false)
    extends Buffer[A] {

  private val cap = if limit < 1 then 1 else limit

  /**
   * Parts in use: index 0 exists from the start, the rest appear.
   *
   * A pre-sized array with nulls for the parts not opened yet, NOT a
   * `Vector` behind an `AtomicReference`. The vector was read and
   * indexed on every push and every pop — a trie walk where a plain
   * ring reads a field — and that showed up exactly where it should:
   * 143.9us against a ring's 114.5 at ONE producer, which is the
   * width at which an adaptive buffer must be indistinguishable from
   * the thing it adapts away from. Here an index is an array read.
   */
  private val slots = AtomicReferenceArray[Buffer[A] | Null](cap)
  // EAGER opens every part at once, which is the fixed relaxed buffer
  // this file used to have as a separate class: the difference between
  // "k parts from the start" and "parts as producers arrive" is one
  // flag, not one type, and two nearly-identical lock-free structures
  // is two places for the same defect to hide.
  if eager then { var i = 0; while i < cap do { slots.set(i, make()); i += 1 } }
  else slots.set(0, make())

  /** how many are open; grown only by the thread that opened one, so
   * a reader never has to walk the array to count */
  private val open = AtomicInteger(if eager then cap else 1)

  /** set once the channel is closing: no part may be opened after
   * that, or its end mark would never be placed */
  private val frozen = AtomicBoolean(false)

  /** the next part to hand out, and the route each thread keeps */
  private val nextPart = AtomicInteger(0)
  private val mine = new ThreadLocal[Integer]:
    override def initialValue(): Integer = claimPart()

  private val cursor = AtomicInteger(0)

  /** a fresh part for a producer that has not sent here before —
   * unless the buffer is frozen or the cap is reached, in which case
   * it shares an existing one */
  private def claimPart(): Integer =
    val want = nextPart.getAndIncrement()
    if want == 0 then Integer.valueOf(0)
    else if want >= cap || frozen.get then Integer.valueOf(want % open.get)
    else
      // open the slot if it is still empty; whoever wins the CAS also
      // counts it, so `open` never runs ahead of what exists
      if slots.get(want) == null then
        val fresh = make()
        if slots.compareAndSet(want, null, fresh) then open.incrementAndGet(): Unit
      if slots.get(want) == null then Integer.valueOf(want % open.get)
      else Integer.valueOf(want)

  private def part(i: Int): Buffer[A] =
    if open.get == 1 then slots.get(0).nn
    else partAt(i)

  private def partAt(i: Int): Buffer[A] =
    val at = if i >= cap then i % cap else i
    val b = slots.get(at)
    if b != null then b.nn else slots.get(0).nn

  override def parts: Int = open.get

  /** the cap, which is what a channel must size its own arrays by:
   * `parts` grows after construction and anything sized once from it
   * would be sized for a single part */
  override def maxParts: Int = cap
  override def route(): Int = mine.get.intValue

  private def eachOpen(f: Buffer[A] => Unit): Unit =
    var i = 0
    val n = open.get
    while i < n do
      val b = slots.get(i)
      if b != null then f(b.nn)
      i += 1

  override def capacity: Int =
    var c = 0L
    eachOpen(b => c += b.capacity.toLong)
    if c > Int.MaxValue then Int.MaxValue else c.toInt

  override def push(a: A): Boolean = part(route()).push(a)
  override def pushAt(r: Int, a: A): Boolean = part(r).push(a)

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    part(route()).pushDeciding(a, unless, orElse)

  override def pushDecidingAt(r: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
    part(r).pushDeciding(a, unless, orElse)

  override def pushMany(n: Int)(src: Int => A): Int = part(route()).pushMany(n)(src)

  override def hasRoom: Boolean = part(route()).hasRoom
  override def hasRoomAt(r: Int): Boolean = part(r).hasRoom

  /** freeze first, THEN seal: a part opened between the two would
   * never get its mark, and the stream would never end */
  override def seal(mark: A): Int =
    frozen.set(true)
    var placed = 0
    var i = 0
    val n = open.get
    while i < n do
      val b = slots.get(i)
      if b != null && sealedAt.get(i) == 0 && b.nn.push(mark) then
        sealedAt.set(i, 1)
        placed += 1
      i += 1
    placed

  private val sealedAt = java.util.concurrent.atomic.AtomicIntegerArray(cap)

  override def pop(): A | Null =
    // ONE PART is the common case and deserves the straight line: no
    // cursor, no loop, no scan. Measured at a single producer, a
    // partitioned buffer costs 30% over a plain ring (145.8 against
    // 112.4) -- and the hand-tuned relaxed lane costs the same, so
    // that price is partitioning itself rather than adapting. This
    // shaves what can be shaved off it.
    if open.get == 1 then slots.get(0).nn.pop()
    else popScanning()

  private def popScanning(): A | Null =
    val n = open.get
    var out: A | Null = null
    var tried = 0
    var i = cursor.get
    while out == null && tried < n do
      val at = if i >= n then i % n else i
      val b = slots.get(at)
      if b != null then out = b.nn.pop()
      if out == null then i += 1 else cursor.set(at)
      tried += 1
    out

  override def popMany(max: Int)(sink: A => Unit): Int =
    if open.get == 1 then slots.get(0).nn.popMany(max)(sink)
    else popManyScanning(max)(sink)

  private def popManyScanning(max: Int)(sink: A => Unit): Int =
    val n = open.get
    var took = 0
    var tried = 0
    var i = cursor.get
    while took == 0 && tried < n do
      val at = if i >= n then i % n else i
      val b = slots.get(at)
      if b != null then took = b.nn.popMany(max)(sink)
      if took == 0 then i += 1 else cursor.set(at)
      tried += 1
    took

  override def lastRoute: Int = cursor.get

  override def size: Int =
    var s = 0L
    eachOpen(b => s += b.size.toLong)
    if s > Int.MaxValue then Int.MaxValue else s.toInt

  override def isEmpty: Boolean =
    if open.get == 1 then slots.get(0).nn.isEmpty
    else isEmptyScanning

  private def isEmptyScanning: Boolean =
    var empty = true
    eachOpen(b => if !b.isEmpty then empty = false)
    empty

  override def hasReady: Boolean =
    if open.get == 1 then slots.get(0).nn.hasReady
    else hasReadyScanning

  private def hasReadyScanning: Boolean =
    var ready = false
    eachOpen(b => if b.hasReady then ready = true)
    ready
}
