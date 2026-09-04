package okay

/**
 * The persistent FIFO a `StmChannel` keeps its buffer in — a seam,
 * with two implementations, for the same reason `Buffer` is one: two
 * plausible structures, and the only way to choose between them is to
 * measure both.
 *
 * It replaces `immutable.Queue`, which is the same two lists, for one
 * reason: `Queue` only lets you take elements ONE at a time, through
 * `dequeue`, and each of those allocates a tuple and a whole new
 * queue. In a batched receive that is thousands of intermediate
 * buffers nobody will ever look at, built inside a single transaction
 * that needs only its FINAL state.
 *
 * So the batched pair here is `drop` (what remains) and `fill` (what
 * is taken), written to be used together: `drop` decides the new
 * state inside the transaction, `fill` runs in the action AFTER the
 * CAS has won, so a losing transaction pays for none of it.
 *
 * WHY TWO. `ListFifo` is the classic banker's pair of lists.
 * `ArrayFifo` carries `Segments`' idea into a persistent structure —
 * elements in an array, each version holding its own index — which in
 * principle turns the refill into one allocation instead of n cons
 * cells and makes `drop` within the front free. Whether that is worth
 * anything is a question for a benchmark, not for taste, and the two
 * lanes exist so it can be asked with both under the same host load.
 */
trait Fifo[A] {

  def isEmpty: Boolean
  def nonEmpty: Boolean

  /** O(1) — the back holds newest-first precisely so this costs one
   * cons cell and nothing else */
  def enqueue(a: A): Fifo[A]

  /** the oldest element and the rest; the one path that still pays a
   * tuple, and the one that has to turn the back round */
  def dequeue: (A, Fifo[A])

  /** what remains after the `n` oldest are taken, given `total` */
  def drop(n: Int, total: Int): Fifo[A]

  /**
   * Write the `n` oldest into `out`, in FIFO order.
   *
   * Meant to run AFTER the transaction's CAS: the contents are
   * immutable and the CAS already made the take exclusive, so reading
   * them here is safe and a losing transaction pays nothing.
   */
  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit
}

/**
 * Two lists: the front oldest-first, the back newest-first.
 *
 * `List.reverse` never runs on the batched path — the back's FIFO
 * order is its reverse, so `fill` walks it FORWARD and writes at
 * DESCENDING indices, and the order falls out of the arithmetic with
 * no list allocated. The elementwise `dequeue` still turns it round
 * when the front runs out: amortized O(1), and the only place a
 * reversed list is built at all.
 */
final class ListFifo[A](val front: List[A], val back: List[A]) extends Fifo[A] {

  def isEmpty: Boolean = front.isEmpty && back.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def enqueue(a: A): Fifo[A] = ListFifo(front, a :: back)

  def dequeue: (A, Fifo[A]) =
    if front.nonEmpty then (front.head, ListFifo(front.tail, back))
    else
      val f = back.reverse
      (f.head, ListFifo(f.tail, Nil))

  def drop(n: Int, total: Int): Fifo[A] =
    if n >= total then ListFifo(Nil, Nil)
    else if n <= 0 then this
    else
      var k = n
      var f = front
      while k > 0 && f.nonEmpty do { f = f.tail; k -= 1 }
      if k == 0 then ListFifo(f, back)
      else
        var lb = 0
        var b = back
        while b.nonEmpty do { lb += 1; b = b.tail }
        // prepending the back's newest first leaves `keep` already
        // oldest-first, which is exactly what a FRONT list is -- an
        // extra reverse here put the remainder backwards, and the
        // property test against a plain list caught it at once
        var keep = List.empty[A]
        var i = 0
        var c = back
        while i < lb - k do { keep = c.head :: keep; i += 1; c = c.tail }
        ListFifo(keep, Nil)

  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit =
    var i = 0
    var f = front
    while i < n && f.nonEmpty do
      out.update(i, f.head)
      i += 1
      f = f.tail
    if i < n then
      var b = back
      if n < total then
        var lb = 0
        var c = back
        while c.nonEmpty do { lb += 1; c = c.tail }
        var skip = lb - (n - i)
        while skip > 0 do { b = b.tail; skip -= 1 }
      var j = n - 1
      while j >= i && b.nonEmpty do
        out.update(j, b.head)
        j -= 1
        b = b.tail
}

/**
 * The front as an immutable array plus an index — `Segments`' idea
 * carried into a persistent structure. Each version keeps its own
 * `start`, the chunk is never mutated, so persistence and with it STM
 * composability are untouched.
 *
 * What it is meant to buy: turning the back round allocates ONE array
 * instead of n cons cells, `dequeue` becomes index arithmetic rather
 * than pointer chasing, and `drop` of anything the front covers only
 * moves the index.
 */
final class ArrayFifo[A](val front: Chunk[A], val start: Int, val back: List[A])
    extends Fifo[A] {

  def frontSize: Int = front.length - start
  def isEmpty: Boolean = frontSize <= 0 && back.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def enqueue(a: A): Fifo[A] = ArrayFifo(front, start, a :: back)

  /** the back, turned round into a chunk: walking it forward while
   * writing DESCENDING indices puts it in FIFO order without a
   * reversed list ever existing */
  private def turned(keep: Int): Chunk[A] =
    val buf = ChunkBuf[A](keep)
    var j = keep - 1
    var c = back
    while j >= 0 do { buf.update(j, c.head); j -= 1; c = c.tail }
    buf.chunk

  private def backSize: Int =
    var n = 0
    var b = back
    while b.nonEmpty do { n += 1; b = b.tail }
    n

  def dequeue: (A, Fifo[A]) =
    if frontSize > 0 then (front(start), ArrayFifo(front, start + 1, back))
    else
      val f = turned(backSize)
      (f(0), ArrayFifo(f, 1, Nil))

  def drop(n: Int, total: Int): Fifo[A] =
    if n >= total then ArrayFifo(Chunks.emptyChunk[A], 0, Nil)
    else if n <= 0 then this
    else if n <= frontSize then ArrayFifo(front, start + n, back)
    else ArrayFifo(turned(backSize - (n - frontSize)), 0, Nil)

  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit =
    var i = 0
    val fs = frontSize
    val m = if n < fs then n else fs
    while i < m do
      out.update(i, front(start + i))
      i += 1
    if i < n then
      var b = back
      if n < total then
        var skip = backSize - (n - i)
        while skip > 0 do { b = b.tail; skip -= 1 }
      var j = n - 1
      while j >= i && b.nonEmpty do
        out.update(j, b.head)
        j -= 1
        b = b.tail
}

object Fifo {
  /** allocated rather than shared: a covariant `Fifo[Nothing]` would
   * let one instance serve every element type, but covariance costs
   * `enqueue` its precise signature, and a cast to fake it is not
   * worth saving one small object per BATCH */
  def list[A]: Fifo[A] = ListFifo(Nil, Nil)
  def array[A]: Fifo[A] = ArrayFifo(Chunks.emptyChunk[A], 0, Nil)
}
