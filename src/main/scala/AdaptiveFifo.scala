package okay

import java.util.concurrent.atomic.AtomicIntegerArray
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
final class AdaptiveFifo[A](limit: Int, make: () => Buffer[A], eager: Boolean = false,
                           first: Buffer[A] | Null = null,
                           firstOwner: Thread | Null = null)
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
  // `first` is an EXISTING buffer adopted as part 0, which is how a
  // channel becomes partitioned without moving an element: whatever is
  // already in it stays where it is and is read from where it is
  else slots.set(0, if first != null then first.nn else make())

  /** how many are open; grown only by the thread that opened one, so
   * a reader never has to walk the array to count */
  private val open = AtomicInteger(if eager then cap else 1)

  /** set once the channel is closing: no part may be opened after
   * that, or its end mark would never be placed */
  private val frozen = AtomicBoolean(false)

  /** the next part to hand out, and the route each thread keeps */
  // an ADOPTED part 0 already has an owner: the producer that filled
  // the ring this buffer grew out of. Its elements are in there, so it
  // must keep pushing there, and the next claimer must not be given
  // the same part
  private val nextPart = AtomicInteger(if firstOwner != null then 1 else 0)
  /** a producer's own part: the index the channel routes its parked
   * senders by, and the BUFFER itself, so the hot push is one
   * thread-local read and the ring's own push — no `open` read, no
   * slot read, no unboxing. Caching the buffer rather than the index
   * also keeps a producer's own order by construction: it pushes to
   * the same object for its whole life, whatever else opens. */
  private final class Home(val idx: Int, val buf: Buffer[A])

  private val mine = new ThreadLocal[Home]:
    override def initialValue(): Home =
      if (Thread.currentThread() eq firstOwner) then Home(0, slots.get(0).nn)
      else
        val i = claimPart()
        Home(i.intValue, slots.get(i.intValue).nn)

  /**
   * ONE CONSUMER AT A TIME PER PART (consumer-claim, 2026-09-07, the
   * operator's design). A producer has a part of its own; a consumer
   * takes a WHOLE DRAIN out of one part under an exclusive claim and
   * releases it before it processes anything, so two consumers never
   * work the same part's head and nobody is held up by what someone
   * else took. Measured before: adding consumers cost 2.3x (four
   * producers, elementwise, 510 -> 1 039 us).
   *
   * The claim is a plain CAS on a flag per part. It is held for a
   * drain and nothing else — never across a callback, never across a
   * park — so a consumer that stops between drains blocks no one.
   */
  private val claimed = AtomicIntegerArray(cap)

  /** where THIS consumer starts looking, so consumers do not convoy
   * onto part 0 the way a single shared cursor made them */
  private val startAt = new ThreadLocal[Integer]:
    override def initialValue(): Integer =
      Integer.valueOf(Math.floorMod(Thread.currentThread().threadId().toInt, if cap < 1 then 1 else cap))

  /**
   * The part THIS THREAD last took an element from — what the channel
   * wakes senders on. A shared scan cursor could not answer that: it
   * is one cell, so with several consumers rotating over the parts it
   * named whatever part was scanned LAST BY ANYONE, and the channel
   * then woke the senders of a part that had not freed a slot while
   * the sender on the part that had slept on
   * (`adaptive-p-x-c-deadlock`, reproduced at round 5 363 of the P x C
   * probe: four consumers parked on empty, one producer parked on
   * full). A thread's own last route is exact, because every
   * `wakeSender()` runs on the thread that just popped.
   */
  private val myRoute = ThreadLocal.withInitial[Integer](() => Integer.valueOf(0))

  /**
   * A fresh part for a producer that has not sent here before, unless
   * the buffer is frozen or the cap is reached, in which case it shares
   * an existing one.
   *
   * THE INDEX IS THE COUNT (adversarial-lanes, 2026-09-06). The first
   * cut opened slot `want` -- the producer's claim number -- and every
   * scan walked indices `0 until open`, a COUNT. Those agree only while
   * producers open their parts in claim order. One producer delayed
   * between claiming and opening, and the ones after it opened slots
   * 6..11 with `open` at 11: slot 11 held a producer's whole output
   * and no scan ever reached it. Measured at 16 x 16: the late
   * producers' elements lost entirely, and, once their part filled,
   * a producer parked on a full part with every consumer parked on
   * "empty" -- a deadlock the many-to-many law reproduces. Now the
   * slot a producer opens IS the value `open` had before it, so the
   * scanned range is dense by construction; a scanner that sees the
   * count before the slot is set skips the null and comes back.
   */
  private def claimPart(): Integer =
    val want = nextPart.getAndIncrement()
    if want == 0 then Integer.valueOf(0)
    else if frozen.get then Integer.valueOf(Math.floorMod(want, opened))
    else
      val idx = open.getAndIncrement()
      if idx < cap then
        slots.set(idx, make())
        Integer.valueOf(idx)
      else
        // the cap is reached: give the count back and share
        open.decrementAndGet(): Unit
        Integer.valueOf(Math.floorMod(want, cap))

  /** the open count, never past the cap: a claimer may have taken the
   * count one past it for the instant before it gives it back */
  private def opened: Int =
    val n = open.get
    if n > cap then cap else if n < 1 then 1 else n

  private def part(i: Int): Buffer[A] =
    if open.get == 1 then slots.get(0).nn
    else partAt(i)

  private def partAt(i: Int): Buffer[A] =
    val n = opened
    val at = if i >= n then i % n else i
    val b = slots.get(at)
    if b != null then b.nn else slots.get(0).nn

  override def parts: Int = opened

  /** the cap, which is what a channel must size its own arrays by:
   * `parts` grows after construction and anything sized once from it
   * would be sized for a single part */
  override def maxParts: Int = cap
  override def route(): Int = mine.get.idx

  private def eachOpen(f: Buffer[A] => Unit): Unit =
    var i = 0
    val n = opened
    while i < n do
      val b = slots.get(i)
      if b != null then f(b.nn)
      i += 1

  override def capacity: Int =
    var c = 0L
    eachOpen(b => c += b.capacity.toLong)
    if c > Int.MaxValue then Int.MaxValue else c.toInt

  override def push(a: A): Boolean = mine.get.buf.push(a)
  override def pushAt(r: Int, a: A): Boolean = part(r).push(a)

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    mine.get.buf.pushDeciding(a, unless, orElse)

  override def pushDecidingAt(r: Int, a: A, unless: AtomicBoolean, orElse: A): A | Null =
    part(r).pushDeciding(a, unless, orElse)

  override def pushMany(n: Int)(src: Int => A): Int = mine.get.buf.pushMany(n)(src)

  override def hasRoom: Boolean = mine.get.buf.hasRoom
  override def hasRoomAt(r: Int): Boolean = part(r).hasRoom

  /** freeze first, THEN seal: a part opened between the two would
   * never get its mark, and the stream would never end */
  /**
   * ONE MARK PER PART, under concurrent callers (adversarial-lanes,
   * 2026-09-06). `seal` is called by the channel after EVERY pop once
   * closing has begun, from every consumer thread at once. The first
   * cut checked `sealedAt` and then pushed -- a check-then-act -- so
   * two consumers could both see 0 and both push, and a part ended
   * up with several end marks. Receivers count marks met against the
   * part count, so the extra marks satisfied "all parts ended" while
   * parts still held thousands of elements: measured at 16x16, 83
   * seals placed for 16 parts and 11 019 elements left unread when
   * every consumer had already been told the stream was over. The
   * claim is now a CAS BEFORE the push; a push a full part refuses
   * gives the claim back so a later call retries it.
   */
  override def seal(mark: A): Int =
    frozen.set(true)
    var placed = 0
    var i = 0
    val n = opened
    while i < n do
      val b = slots.get(i)
      if b != null then
        // Three states, not two: 0 unsealed, 1 a caller is mid-push, 2
        // the mark is IN. The two-state version (claim, push, give the
        // claim back on refusal) had a window the channel laws found
        // (adversarial-lanes, 2026-09-06): the closer claims part 0 and
        // its push is refused because the part is full; meanwhile the
        // consumer pops the last element and ITS seal, finding the
        // claim taken, places nothing; the closer gives the claim back
        // -- and nobody is left to try again, since seal runs only from
        // pops and the ring is now empty. The consumer parks for good
        // on a closed, empty, unsealed channel. Now a caller that meets
        // a mid-push claim waits for its verdict -- the holder's push is
        // a few CASes, never a park -- and retries on a refusal.
        var done = false
        while !done do
          val st = sealedAt.get(i)
          if st == 2 then done = true
          else if st == 0 && sealedAt.compareAndSet(i, 0, 1) then
            if b.nn.push(mark) then { sealedAt.set(i, 2); placed += 1 }
            else sealedAt.set(i, 0)
            done = true
          else if st == 1 then Thread.onSpinWait()
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
    if open.get == 1 then { myRoute.set(0); slots.get(0).nn.pop() }
    else popScanning()

  private def popScanning(): A | Null =
    val n = opened
    var out: A | Null = null
    var tried = 0
    var i = startAt.get.intValue
    while out == null && tried < n do
      val at = if i >= n then Math.floorMod(i, n) else i
      val b = slots.get(at)
      if b != null then out = b.nn.pop()
      if out == null then i += 1 else { startAt.set(at); myRoute.set(at) }
      tried += 1
    out

  override def popMany(max: Int)(sink: A => Unit): Int =
    if open.get == 1 then { myRoute.set(0); slots.get(0).nn.popMany(max)(sink) }
    else popManyScanning(max)(sink)

  private def popManyScanning(max: Int)(sink: A => Unit): Int =
    val n = opened
    var took = 0
    var tried = 0
    var i = startAt.get.intValue
    while took == 0 && tried < n do
      val at = if i >= n then Math.floorMod(i, n) else i
      val b = slots.get(at)
      // the claim: one consumer drains a part at a time, and a part
      // someone else holds is skipped rather than waited for
      if b != null && claimed.compareAndSet(at, 0, 1) then
        try took = b.nn.popMany(max)(sink)
        finally claimed.set(at, 0)
      if took == 0 then i += 1 else myRoute.set(at)
      tried += 1
    took

  override def lastRoute: Int = myRoute.get.intValue

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
