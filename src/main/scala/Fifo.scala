package okay

/**
 * The persistent FIFO `StmChannel` keeps its buffer in: two lists, the
 * front oldest-first and the back newest-first.
 *
 * It replaces `immutable.Queue`, which is the same two lists, for one
 * reason: `Queue` only lets you take elements ONE at a time, through
 * `dequeue`, and each of those allocates a tuple and a whole new queue.
 * In a batched receive that is 4000 intermediate buffers nobody will
 * ever look at — 23% of the lane's profile in `Tuple2` alone, plus 8%
 * in `dequeue` — built inside a single transaction that needs only its
 * FINAL state.
 *
 * So this type exposes the two lists, and the batched take is written
 * against them directly:
 *
 *   - `drop` answers the buffer that remains. Taking the whole buffer
 *     — the common case, since a consumer usually asks for more than
 *     is there — is O(1) and allocates nothing.
 *   - `fill` writes the taken elements into a chunk, and it is the
 *     reason `List.reverse` disappears rather than gets faster. The
 *     back is newest-first, so its FIFO order is its reverse; instead
 *     of building that reversed list, `fill` walks the back FORWARD
 *     and writes at DESCENDING indices. The order falls out of the
 *     arithmetic and no list is allocated at all.
 *
 * The elementwise `dequeue` still pays a tuple and still needs the
 * amortized reverse when the front runs out. That is inherent to a
 * persistent FIFO taken one element at a time, and it is what the
 * batched path exists to avoid.
 */
private[okay] final class Fifo[A](val front: List[A], val back: List[A]) {

  def isEmpty: Boolean = front.isEmpty && back.isEmpty
  def nonEmpty: Boolean = !isEmpty

  /** O(1), one cons cell — the back holds newest-first precisely so
   * that this costs nothing */
  def enqueue(a: A): Fifo[A] = Fifo(front, a :: back)

  /** the oldest element and the rest. The front is refilled from the
   * reversed back when it runs out: amortized O(1), and the one place
   * a reverse is still built */
  def dequeue: (A, Fifo[A]) =
    if front.nonEmpty then (front.head, Fifo(front.tail, back))
    else
      val f = back.reverse
      (f.head, Fifo(f.tail, Nil))

  /**
   * The buffer left after the `n` oldest elements are taken, given the
   * current `total`. Taking everything answers the empty buffer with
   * no work at all, which is the case a batched receive normally hits.
   */
  def drop(n: Int, total: Int): Fifo[A] =
    if n >= total then Fifo.empty[A]
    else if n <= 0 then this
    else
      var k = n
      var f = front
      while k > 0 && f.nonEmpty do { f = f.tail; k -= 1 }
      if k == 0 then Fifo(f, back)
      else
        // the rest must come from the back's OLDEST end, which is its
        // TAIL, so what remains is the back's newest `lb - k`
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
        Fifo(keep, Nil)

  /**
   * Write the `n` oldest elements into `out`, in FIFO order.
   *
   * Meant to run AFTER the transaction's CAS has won: the lists are
   * immutable and the CAS already made this take exclusive, so reading
   * them here is safe, and a transaction that LOST pays none of it.
   */
  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit =
    var i = 0
    var f = front
    while i < n && f.nonEmpty do
      out.update(i, f.head)
      i += 1
      f = f.tail
    if i < n then
      // the back, newest-first: walk it forward and write DESCENDING,
      // so FIFO order appears without a reversed list existing
      var b = back
      if n < total then
        // only part of the back is wanted, and it is its OLDEST part
        // -- skip past the newest ones first
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

private[okay] object Fifo {
  /** allocated rather than shared: a covariant `Fifo[Nothing]` would
   * make one instance serve every element type, but covariance costs
   * `enqueue` its precise signature, and a cast to fake it is not
   * worth saving one small object per BATCH */
  def empty[A]: Fifo[A] = Fifo(Nil, Nil)
}
