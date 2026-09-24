package okay2.stream

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReferenceArray}

/** a small fixed array of counters, one per part (one `AtomicInteger`
 * each, as in the Scala 3 core, which cannot use `AtomicIntegerArray` on
 * Native — okay2 keeps the same shape for when okay2-cross lands) */
private final class Cells(n: Int) {
  private val a: Array[AtomicInteger] = Array.fill(n)(new AtomicInteger(0))
  def get(i: Int): Int = a(i).get
  def set(i: Int, v: Int): Unit = a(i).set(v)
  def compareAndSet(i: Int, expect: Int, update: Int): Boolean = a(i).compareAndSet(expect, update)
}

/**
 * A buffer that grows PARTS as producers appear — okay-stream's
 * AdaptiveFifo.scala, with every correction its header records.
 *
 * The contract promises each producer's own order, no loss and no
 * duplication, and says nothing about the order BETWEEN producers; that
 * silence is what makes choosing the part count automatically legal.
 * NOTHING EVER MIGRATES: a new producer gets a new, empty part.
 *
 * - `eager` opens every part at once (the fixed relaxed buffer);
 *   otherwise parts open as producers claim them.
 * - `first`, when given, is an EXISTING buffer ADOPTED as part 0 — how
 *   `Growing` becomes partitioned without moving an element. An adopted
 *   part 0 is drain-only and READ FIRST while it holds anything, so a
 *   producer's earlier elements (in part 0) come out before its later
 *   ones (in its own part). The rule holds PER CALL and the call is not
 *   atomic: one displacement per producer across the swap is what the
 *   laws allow (`growing-order-drain-guarantee`, Scala 3 BACKLOG).
 * - parts stop opening once closing is seen, or a part opened after close
 *   would never be sealed and the stream never end. The freeze and the
 *   opening of a part are ONE atomic word (`open`'s `Frozen` bit), not a
 *   flag checked before a counter is bumped: that check-then-act let a
 *   part open between `seal`'s freeze and its count, unsealed, and the
 *   consumer then parked for good on a closed, drained channel
 *   (adaptive-seal-race, 2026-09-24 — law 1b under load, 4 of 6
 *   concurrent runners in 800 rounds; the Scala 3 core has the same code).
 * - the index a producer opens IS the count `open` had before it, so
 *   the scanned range is dense (the claim-index/count mismatch lost late
 *   producers' elements at 16x16 in the Scala 3 core).
 * - one consumer drains a part at a time under a CAS claim (`claimed`).
 * - `seal` is three-state per part (unsealed, mid-push, in) so a refused
 *   push and a concurrent seal cannot leave a part unsealed for good.
 */
final class AdaptiveFifo[A >: Null](limit: Int, make: () => Buffer[A], eager: Boolean = false,
                                    first: Buffer[A] = null) extends Buffer[A] {

  private val cap = if (limit < 1) 1 else limit

  private val slots = new AtomicReferenceArray[Buffer[A]](cap)
  if (eager) { var i = 0; while (i < cap) { slots.set(i, make()); i += 1 } }
  else slots.set(0, if (first != null) first else make())

  /** how many are open, and — in the `Frozen` bit — whether any more may
   * open. One word, so opening a part and freezing are ordered by one CAS */
  private val open = new AtomicInteger(if (eager) cap else 1)
  private final val Frozen = 1 << 30

  private val adopted: Boolean = first != null


  /** the next part to hand out; an adopted part 0 is never handed out */
  private val nextPart = new AtomicInteger(if (first != null) 1 else 0)

  /** a producer's own part: its index and the buffer itself, so the hot
   * push is one thread-local read and that buffer's push */
  private final class Home(val idx: Int, val buf: Buffer[A])

  private val mine = new ThreadLocal[Home] {
    override def initialValue(): Home = {
      val i = claimPart()
      new Home(i, slotAt(i))
    }
  }

  /** the slot for an index the count already covers: `claimPart`
   * publishes the count before the slot, so a producer sharing a part
   * can read a null for an instant — and a HOME cannot fall back, its
   * whole order lives in that part, so it waits for the opener's very
   * next statement */
  private def slotAt(i: Int): Buffer[A] = {
    var b = slots.get(i)
    while (b == null) b = slots.get(i)
    b
  }

  private val claimed = new Cells(cap)

  /** where THIS consumer starts looking, so consumers do not convoy */
  private val startAt = new ThreadLocal[Integer] {
    override def initialValue(): Integer =
      Integer.valueOf(Math.floorMod(System.identityHashCode(Thread.currentThread()), cap))
  }

  /** the part THIS thread last took from — exact, because every
   * `wakeSender` runs on the thread that just popped */
  private val myRoute = new ThreadLocal[Integer] {
    override def initialValue(): Integer = Integer.valueOf(0)
  }

  private def claimPart(): Int = {
    val want = nextPart.getAndIncrement()
    if (want == 0) 0
    else {
      var out = -1
      while (out < 0) {
        val v = open.get
        val n = v & ~Frozen
        if ((v & Frozen) != 0) out = share(want, if (n < 1) 1 else n)
        else if (n >= cap) out = share(want, cap)
        // the index a producer opens IS the count before its CAS, and the
        // CAS fails if the freeze landed first
        else if (open.compareAndSet(v, v + 1)) { slots.set(n, make()); out = n }
      }
      out
    }
  }

  /** more producers than parts SHARE — never an adopted part 0, which
   * they would refill ahead of everyone waiting behind it */
  private def share(want: Int, n: Int): Int =
    if (!adopted || n <= 1) Math.floorMod(want, n)
    else 1 + Math.floorMod(want, n - 1)

  private def opened: Int = {
    val n = open.get & ~Frozen
    if (n > cap) cap else if (n < 1) 1 else n
  }

  private def part(i: Int): Buffer[A] =
    if (open.get == 1) slots.get(0) else partAt(i)

  private def partAt(i: Int): Buffer[A] = {
    val n = opened
    val at = if (i >= n) i % n else i
    val b = slots.get(at)
    if (b != null) b else slots.get(0)
  }

  override def parts: Int = opened
  override def maxParts: Int = cap
  override def route(): Int = mine.get.idx

  private def eachOpen(f: Buffer[A] => Unit): Unit = {
    var i = 0
    val n = opened
    while (i < n) {
      val b = slots.get(i)
      if (b != null) f(b)
      i += 1
    }
  }

  override def capacity: Int = {
    var c = 0L
    eachOpen(b => c += b.capacity.toLong)
    if (c > Int.MaxValue) Int.MaxValue else c.toInt
  }

  override def push(a: A): Boolean = mine.get.buf.push(a)
  override def pushAt(r: Int, a: A): Boolean = part(r).push(a)
  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A = mine.get.buf.pushDeciding(a, unless, orElse)
  override def pushDecidingAt(r: Int, a: A, unless: AtomicBoolean, orElse: A): A = part(r).pushDeciding(a, unless, orElse)
  override def pushMany(n: Int)(src: Int => A): Int = mine.get.buf.pushMany(n)(src)
  override def hasRoom: Boolean = mine.get.buf.hasRoom
  override def hasRoomAt(r: Int): Boolean = part(r).hasRoom

  private val sealedAt = new Cells(cap)

  /** freeze, THEN seal exactly the parts the freeze saw; one mark per
   * part under concurrent callers */
  override def seal(mark: A): Int = {
    var v = open.get
    while ((v & Frozen) == 0 && !open.compareAndSet(v, v | Frozen)) v = open.get
    val n = { val c = open.get & ~Frozen; if (c > cap) cap else if (c < 1) 1 else c }
    var placed = 0
    var i = 0
    while (i < n) {
      // a part the count covers is set by its opener's next statement
      val b = slotAt(i)
      locally {
        var done = false
        while (!done) {
          val st = sealedAt.get(i)
          if (st == 2) done = true
          else if (st == 0 && sealedAt.compareAndSet(i, 0, 1)) {
            if (b.push(mark)) { sealedAt.set(i, 2); placed += 1 }
            else sealedAt.set(i, 0)
            done = true
          }
          // st == 1: another caller is mid-push, a few CASes away — wait
        }
      }
      i += 1
    }
    placed
  }

  override def pop(): A =
    if (adopted) popAdoptedFirst()
    else if (open.get == 1) { myRoute.set(0); slots.get(0).pop() }
    else popScanning()

  private def popAdoptedFirst(): A = {
    val out = slots.get(0).pop()
    if (out != null) { myRoute.set(0); out }
    else if (open.get == 1) null
    else popScanning()
  }

  private def popScanning(): A = {
    val n = opened
    var out: A = null
    var tried = 0
    var i = startAt.get.intValue
    while (out == null && tried < n) {
      val at = if (i >= n) Math.floorMod(i, n) else i
      val b = slots.get(at)
      if (b != null) out = b.pop()
      if (out == null) i += 1 else { startAt.set(at); myRoute.set(at) }
      tried += 1
    }
    out
  }

  override def popMany(max: Int)(sink: A => Unit): Int =
    if (adopted) popManyAdoptedFirst(max)(sink)
    else if (open.get == 1) { myRoute.set(0); slots.get(0).popMany(max)(sink) }
    else popManyScanning(max)(sink)

  private def popManyAdoptedFirst(max: Int)(sink: A => Unit): Int = {
    var took = 0
    if (claimed.compareAndSet(0, 0, 1)) {
      try took = slots.get(0).popMany(max)(sink) finally claimed.set(0, 0)
    }
    if (took > 0) { myRoute.set(0); took }
    else if (open.get == 1) 0
    else popManyScanning(max)(sink)
  }

  private def popManyScanning(max: Int)(sink: A => Unit): Int = {
    val n = opened
    var took = 0
    var tried = 0
    var i = startAt.get.intValue
    while (took == 0 && tried < n) {
      val at = if (i >= n) Math.floorMod(i, n) else i
      val b = slots.get(at)
      if (b != null && claimed.compareAndSet(at, 0, 1)) {
        try took = b.popMany(max)(sink) finally claimed.set(at, 0)
      }
      if (took == 0) i += 1 else myRoute.set(at)
      tried += 1
    }
    took
  }

  override def lastRoute: Int = myRoute.get.intValue

  override def size: Int = {
    var s = 0L
    eachOpen(b => s += b.size.toLong)
    if (s > Int.MaxValue) Int.MaxValue else s.toInt
  }

  override def isEmpty: Boolean =
    if (open.get == 1) slots.get(0).isEmpty
    else { var empty = true; eachOpen(b => if (!b.isEmpty) empty = false); empty }

  override def hasReady: Boolean =
    if (open.get == 1) slots.get(0).hasReady
    else { var ready = false; eachOpen(b => if (b.hasReady) ready = true); ready }
}
