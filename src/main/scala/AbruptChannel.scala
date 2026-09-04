package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

/**
 * A channel with DELIBERATELY WEAKER guarantees: `close` ends it at
 * once and whatever is still buffered is discarded.
 *
 * This is the trade named rather than hidden. `StmChannel`'s contract
 * — buffered elements still drain, an accepted element is always
 * delivered — is strictly stronger than a queue usually offers
 * (`zio.Queue.shutdown` interrupts and promises no drain), and every
 * defect in `ring-channel` lived in honouring it: the in-flight
 * barrier, the two-phase close and the `closed && empty` derivation
 * exist ONLY to make termination detectable across two structures.
 *
 * Drop the promise and the difficulty goes with it. There is no
 * termination to detect: `close` sets a flag, wakes everyone, and a
 * receiver that finds the flag is done, buffer or no buffer. What is
 * left is a ring and two CAS'd waiter queues.
 *
 * WHEN THIS IS THE RIGHT CHANNEL: a live feed where the consumer
 * stopping means the remaining elements are stale anyway — a UI's
 * event stream, a cancelled request's frames, a shutdown path. NOT
 * `Channel.merge`, whose consumers rely on draining what the sources
 * already produced, and not anywhere `send`'s `true` is read as a
 * promise of delivery.
 *
 * It answers `finished` honestly: once closed, nothing further can be
 * delivered — which is true here precisely because the buffer is
 * abandoned.
 */
final class AbruptChannel[A](requested: Int) extends Channel[A] {

  private final class Waiter(val resume: () => Unit):
    val claimed = AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)

  private val ring = Ring[A](requested)
  private val receivers = AtomicReference[List[Waiter]](Nil)
  private val senders = AtomicReference[List[Waiter]](Nil)
  private val closed = AtomicBoolean(false)
  private val failure = AtomicReference[Throwable | Null](null)

  val capacity: Int = ring.capacity

  @tailrec private def enqueue(q: AtomicReference[List[Waiter]], w: Waiter): Unit =
    val cur = q.get
    if !q.compareAndSet(cur, w :: cur) then enqueue(q, w)

  @tailrec private def wakeOne(q: AtomicReference[List[Waiter]]): Boolean =
    val cur = q.get
    if cur.isEmpty then false
    else
      val oldest = cur.last
      if !q.compareAndSet(cur, cur.init) then wakeOne(q)
      else if oldest.claim() then { oldest.resume(); true }
      else wakeOne(q)

  @tailrec private def wakeAll(q: AtomicReference[List[Waiter]]): Unit =
    val cur = q.get
    if cur.nonEmpty then
      if !q.compareAndSet(cur, Nil) then wakeAll(q)
      else cur.reverse.foreach(w => if w.claim() then w.resume())

  private def endNow: End =
    val f = failure.get
    if f != null then Left(f.nn) else Right(None)

  def sendAsync(a: A)(k: Accepted): Unit = attemptSend(a, granted0 = false)(k)

  /**
   * `granted` is the wakeup being spent. A first attempt yields to any
   * queued sender so a full ring stays FIFO; a woken sender must NOT
   * re-check that gate -- the pop that woke it freed a slot FOR it,
   * and re-queueing there would swallow the wakeup without filling the
   * slot, which is a lost wakeup and hangs the producer for good.
   * (`StmChannel` never had this: the slot passes to the waiter inside
   * the one transaction that frees it.)
   */
  // a LOOP, never recursion: a thread that claims its own waiter would
  // otherwise resume on its own stack and overflow it under load
  private def attemptSend(a: A, granted0: Boolean)(k: Accepted): Unit =
    val granted = granted0
    var go = true
    while go do
      go = false
      if closed.get then k(false)
      else if (granted || senders.get.isEmpty) && ring.push(a) then
        k(true)
        val _ = wakeOne(receivers)
      else
        val w = Waiter(() => attemptSend(a, granted0 = true)(k))
        enqueue(senders, w)
        // check-register-recheck: a pop between the failed push and the
        // enqueue leaves space no one will wake us for
        if (ring.size < ring.capacity || closed.get) && w.claim() then
          val _ = senders.updateAndGet(_.filterNot(_.claimed.get))
          go = true

  def receiveAsync(k: End => Unit): Unit =
    var go = true
    while go do
      go = false
      // CLOSED WINS OVER THE BUFFER -- the whole difference from
      // StmChannel, and the reason there is nothing to detect
      if closed.get then k(endNow)
      else
        val a = ring.pop()
        if a != null then
          k(Right(Some(a.nn)))
          val _ = wakeOne(senders)
        else
          val w = Waiter(() => receiveAsync(k))
          enqueue(receivers, w)
          if (!ring.isEmpty || closed.get) && w.claim() then
            val _ = receivers.updateAndGet(_.filterNot(_.claimed.get))
            go = true

  /**
   * Drain what the ring already holds, in one call. The batch is not
   * a relaxation: an element already buffered is already late, so
   * handing over ten of them costs no freshness -- but it does pay
   * the ring's head CAS and stamp write ONCE for the batch instead of
   * once per element, which is where the elementwise lane spends most
   * of its samples.
   */
  private[okay] override def receiveManyAsync(max: Int)
                                             (k: Either[Throwable, Chunk[A]] => Unit): Unit =
    if closed.get then k(endNow.map(_ => Chunks.emptyChunk[A]))
    else
      // bounded by the RING, not by max: a producer refills while we
      // drain, so "up to max" must not outrun the buffer we sized
      val room = if max < ring.capacity then max else ring.capacity
      val out = ChunkBuf[A](room)
      var n = 0
      val took = ring.popMany(room) { a => out.update(n, a); n += 1 }
      if took == 0 then
        // nothing buffered: exactly the single receive, its one
        // element handed over as a chunk of one
        receiveAsync(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
      else
        var i = 0
        while i < took do { val _ = wakeOne(senders); i += 1 }
        k(Right(out.take(took)))

  def offer(a: A): Boolean =
    if closed.get then false
    else if !senders.get.isEmpty then false
    else if ring.push(a) then { val _ = wakeOne(receivers); true }
    else false

  /** the bulk offer: one tail move for the whole run, and a receiver
   * woken per element admitted */
  private[okay] override def sendManyNow(n: Int)(src: Int => A): Int =
    // the same fairness gate `offer` keeps: a parked sender was
    // promised the next slot, so a bulk offer must not jump it
    if closed.get || !senders.get.isEmpty then 0
    else
      val took = ring.pushMany(n)(src)
      var i = 0
      while i < took do { val _ = wakeOne(receivers); i += 1 }
      took

  def close(): Unit =
    closed.set(true)
    wakeAll(senders)
    wakeAll(receivers)

  def fail(e: Throwable): Unit = { val _ = failure.compareAndSet(null, e) }
  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closed.get

  /** true the moment it closes: nothing further can be delivered
   * BECAUSE the buffer is abandoned, which is the honest reading of
   * this channel's weaker promise */
  private[okay] def finished: Boolean = closed.get

  private[okay] def cancelSend(cb: Accepted): Unit = ()
  private[okay] def cancelReceive(k: End => Unit): Unit = ()
}
