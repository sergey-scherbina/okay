package okay2.stream

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import okay2.async.Accepted

/**
 * A channel with DELIBERATELY WEAKER guarantees — okay-stream's
 * AbruptChannel.scala: `close` ends it at once and whatever is still
 * buffered is discarded. Drop the drain-on-close promise and the
 * difficulty goes with it: `close` sets a flag, wakes everyone, and a
 * receiver that finds the flag is done. What is left is a ring and two
 * CAS'd waiter queues.
 *
 * The right channel for a live feed whose remainder is stale the moment
 * the consumer stops — a UI's events, a cancelled request's frames. NOT
 * `Channel.merge`, and not anywhere `send`'s `true` is read as a promise
 * of delivery. The laws record which tier it signs for.
 */
final class AbruptChannel[A](buf: Buffer[Any]) extends Channel[A] {

  def this(requested: Int) = this(new Ring[Any](requested))

  private final class Waiter(val resume: () => Unit) {
    val claimed = new AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)
  }

  private val ring: Buffer[Any] = buf
  private val receivers = new AtomicReference[List[Waiter]](Nil)
  private val senders = new AtomicReference[List[Waiter]](Nil)
  private val closed = new AtomicBoolean(false)
  private val failure = new AtomicReference[Throwable](null)

  val capacity: Int = ring.capacity

  /** THE ONE CAST: only A's are ever pushed into this buffer (no marks
   * ride it), so what comes out is an A */
  private def element(x: Any): A = x.asInstanceOf[A]

  @tailrec private def enqueue(q: AtomicReference[List[Waiter]], w: Waiter): Unit = {
    val cur = q.get
    if (!q.compareAndSet(cur, w :: cur)) enqueue(q, w)
  }

  @tailrec private def wakeOne(q: AtomicReference[List[Waiter]]): Boolean = {
    val cur = q.get
    if (cur.isEmpty) false
    else {
      val oldest = cur.last
      if (!q.compareAndSet(cur, cur.init)) wakeOne(q)
      else if (oldest.claim()) { oldest.resume(); true }
      else wakeOne(q)
    }
  }

  @tailrec private def wakeAll(q: AtomicReference[List[Waiter]]): Unit = {
    val cur = q.get
    if (cur.nonEmpty) {
      if (!q.compareAndSet(cur, Nil)) wakeAll(q)
      else cur.reverse.foreach(w => if (w.claim()) w.resume())
    }
  }

  private def endNow: End = {
    val f = failure.get
    if (f != null) Left(f) else Right(None)
  }

  def sendAsync(a: A)(k: Accepted): Unit = attemptSend(a, granted0 = false)(k)

  /** `granted` is the wakeup being spent: a woken sender must NOT
   * re-queue behind the gate, or the wakeup is lost. A LOOP, never
   * recursion, so a thread that claims its own waiter cannot overflow */
  private def attemptSend(a: A, granted0: Boolean)(k: Accepted): Unit = {
    val granted = granted0
    var go = true
    while (go) {
      go = false
      if (closed.get) k(false)
      else if ((granted || senders.get.isEmpty) && ring.push(a)) {
        k(true)
        val _ = wakeOne(receivers)
      } else {
        val w = new Waiter(() => attemptSend(a, granted0 = true)(k))
        enqueue(senders, w)
        if ((ring.hasRoom || closed.get) && w.claim()) {
          val _ = senders.updateAndGet(_.filterNot(_.claimed.get))
          go = true
        }
      }
    }
  }

  def receiveAsync(k: End => Unit): Unit = {
    var go = true
    while (go) {
      go = false
      // CLOSED WINS OVER THE BUFFER — the whole difference from StmChannel
      if (closed.get) k(endNow)
      else {
        val a = ring.pop()
        if (a != null) {
          k(Right(Some(element(a))))
          val _ = wakeOne(senders)
        } else {
          val w = new Waiter(() => receiveAsync(k))
          enqueue(receivers, w)
          if ((ring.hasReady || closed.get) && w.claim()) {
            val _ = receivers.updateAndGet(_.filterNot(_.claimed.get))
            go = true
          }
        }
      }
    }
  }

  override private[stream] def receiveManyAsync(max: Int)(k: Either[Throwable, Chunk[A]] => Unit): Unit =
    if (closed.get) k(endNow.map(_ => Chunks.emptyChunk[A]))
    else {
      val room = if (max < ring.capacity) max else ring.capacity
      val out = ChunkBuf[A](room)
      var n = 0
      val took = ring.popMany(room) { a => out.update(n, element(a)); n += 1 }
      if (took == 0) receiveAsync(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
      else {
        var i = 0
        while (i < took) { val _ = wakeOne(senders); i += 1 }
        k(Right(out.take(took)))
      }
    }

  def offer(a: A): Boolean =
    if (closed.get) false
    else if (!senders.get.isEmpty) false
    else if (ring.push(a)) { val _ = wakeOne(receivers); true }
    else false

  /** the bulk offer: one tail move for the run, behind the same fairness gate */
  override private[stream] def sendManyNow(n: Int)(src: Int => A): Int =
    if (closed.get || !senders.get.isEmpty) 0
    else {
      val took = ring.pushMany(n)(src)
      var i = 0
      while (i < took) { val _ = wakeOne(receivers); i += 1 }
      took
    }

  def close(): Unit = {
    closed.set(true)
    wakeAll(senders)
    wakeAll(receivers)
  }

  def fail(e: Throwable): Unit = { val _ = failure.compareAndSet(null, e) }
  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closed.get

  /** true the moment it closes: nothing further can be delivered BECAUSE
   * the buffer is abandoned */
  private[stream] def finished: Boolean = closed.get

  private[stream] def cancelSend(cb: Accepted): Unit = ()
  private[stream] def cancelReceive(k: End => Unit): Unit = ()
}
