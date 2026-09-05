package okay

import java.util.concurrent.atomic.AtomicBoolean

/**
 * A RELAXED buffer: k sub-buffers, a producer bound to one of them, a
 * consumer taking from whichever has something.
 *
 * WHAT IT TRADES. A single ring makes every producer contend for one
 * tail counter; that CAS is 24% of the profile at one producer and
 * gets worse with each one added. Here each producer works its own
 * part, so producers contend only when two land on the same one.
 * What is given up is the GLOBAL order: an element pushed later into
 * a quiet part can come out before one pushed earlier into a busy
 * part. (Koch, Sanders & Williams call the distance from true FIFO
 * the rank error, and measure the same trade at p=32..192.)
 *
 * WHAT IT DOES NOT TRADE, and this is why it can still be a channel's
 * buffer: a producer stays on ITS part for the life of the buffer, so
 * the elements of one producer keep their order among themselves.
 * That is exactly the law `TestChannelLaws` states — "one producer's
 * elements arrive in the order it sent them" — and it survives.
 * Nothing is lost or duplicated either. Only the interleaving BETWEEN
 * producers is relaxed, which is the part almost no consumer relies
 * on and almost every benchmark quietly assumes.
 *
 * PARTS SEALS, and that is why `Buffer` grew `seal`. Termination in
 * `SentinelChannel` travels as a mark through the buffer and works
 * because nothing can follow it. With k parts a mark placed in one is
 * reached while other parts still hold elements — the stream would
 * end early and drop what was already accepted. So closing seals
 * EVERY part and the channel counts the marks it meets; only the last
 * one ends the stream.
 */
final class MultiFifo[A](requested: Int, make: Int => Buffer[A]) extends Buffer[A] {

  private val n = if requested < 1 then 1 else requested
  private val part = Array.tabulate(n)(_ => make(n))

  /** which part this thread pushes into: fixed for the thread's life,
   * which is what keeps one producer's own order intact */
  private val mine = new ThreadLocal[Integer]:
    private val next = java.util.concurrent.atomic.AtomicInteger(0)
    override def initialValue(): Integer = next.getAndIncrement() % n

  /** where the consumer looked last — a hint, so a drained part is not
   * rescanned from the start every time */
  private val cursor = java.util.concurrent.atomic.AtomicInteger(0)

  override def capacity: Int =
    var c = 0L
    var i = 0
    while i < n do { c += part(i).capacity.toLong; i += 1 }
    if c > Int.MaxValue then Int.MaxValue else c.toInt

  override def parts: Int = n

  override def maxParts: Int = n

  override def push(a: A): Boolean = part(mine.get.intValue).push(a)

  override def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A | Null =
    part(mine.get.intValue).pushDeciding(a, unless, orElse)

  /** the whole run into THIS producer's part, which is what keeps its
   * own elements in order */
  override def pushMany(n: Int)(src: Int => A): Int =
    part(mine.get.intValue).pushMany(n)(src)

  /** room in THIS thread's part -- the only part it can push to */
  override def hasRoom: Boolean = part(mine.get.intValue).hasRoom

  /** the route is the thread's part, taken ONCE per send: a send that
   * parks resumes on the waker's thread, and re-asking there would
   * put the element in a stranger's part */
  override def route(): Int = mine.get.intValue

  override def pushAt(route: Int, a: A): Boolean = part(route % n).push(a)

  override def pushDecidingAt(route: Int, a: A,
                              unless: java.util.concurrent.atomic.AtomicBoolean,
                              orElse: A): A | Null =
    part(route % n).pushDeciding(a, unless, orElse)

  override def hasRoomAt(route: Int): Boolean = part(route % n).hasRoom

  /** the cursor is already where the last take came from */
  override def lastRoute: Int = cursor.get

  /** a part that is full right now cannot take its mark; it will be
   * asked again as room frees up, and a part already sealed is left
   * alone */
  private val sealedAt = java.util.concurrent.atomic.AtomicIntegerArray(n)

  override def seal(mark: A): Int =
    var placed = 0
    var i = 0
    while i < n do
      if sealedAt.get(i) == 0 && part(i).push(mark) then
        sealedAt.set(i, 1)
        placed += 1
      i += 1
    placed

  override def pop(): A | Null =
    var out: A | Null = null
    var tried = 0
    var i = cursor.get
    while out == null && tried < n do
      val p = part(if i >= n then i - n else i)
      out = p.pop()
      if out == null then { i += 1; if i >= n then i -= n }
      tried += 1
    if out != null then cursor.set(i)
    out

  override def popMany(max: Int)(sink: A => Unit): Int =
    // one part per call: a batch that spans parts would have to
    // interleave them, and there is no order to interleave BY
    var took = 0
    var tried = 0
    var i = cursor.get
    while took == 0 && tried < n do
      val at = if i >= n then i - n else i
      took = part(at).popMany(max)(sink)
      if took == 0 then { i += 1; if i >= n then i -= n }
      tried += 1
    if took > 0 then cursor.set(i)
    took

  override def size: Int =
    var s = 0L
    var i = 0
    while i < n do { s += part(i).size.toLong; i += 1 }
    if s > Int.MaxValue then Int.MaxValue else s.toInt

  override def isEmpty: Boolean =
    var i = 0
    var empty = true
    while empty && i < n do { empty = part(i).isEmpty; i += 1 }
    empty

  override def hasReady: Boolean =
    var i = 0
    var ready = false
    while !ready && i < n do { ready = part(i).hasReady; i += 1 }
    ready
}
