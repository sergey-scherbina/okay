package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

/**
 * A channel whose elements live in a lock-free ring instead of an
 * immutable state rebuilt per operation.
 *
 * WHY: `StmChannel` costs three allocations per send (a cons cell, a
 * `Queue`, a `State`) and CASes its whole six-field state, so a
 * concurrent operation on an unrelated field re-runs the entire
 * transition — measured at 28-49% CAS-failure rates
 * (channel-cas-contention) and at 3.4x the cost of the same two
 * operations on a ring (channel-ring). This is the other end of that
 * trade: no allocation per element, and no STM composability, since
 * there is no single cell to read inside a `Tx`.
 *
 * THE PROTOCOL, and the bug it exists to avoid. The first attempt at
 * this (channel-ring-integration) temporarily REMOVED a waiter from
 * its queue to attempt a `pop`, putting it back when the ring turned
 * out empty. A producer pushing during that window saw no waiters,
 * woke nobody, and stranded the element with the receiver parked
 * forever. The deadlock was real and reproducible.
 *
 * The rule that closes it: A WAITER IS CLAIMED, NEVER BORROWED.
 * Whoever CASes `claimed` from false owns COMPLETING that waiter, and
 * completion means running `resume`, which re-attempts the whole
 * operation from the top and re-parks if it still cannot proceed. A
 * waiter therefore never returns to the queue in the state it left
 * it: either it was completed, or a fresh registration replaced it.
 * Waiters are only dequeued by their own claimer, or discarded when
 * already claimed (dead, so nothing is lost).
 *
 * Spurious wake-ups are legal by construction: `resume` re-checks
 * everything, so a wake that finds nothing simply parks again. That
 * is what makes the register-then-recheck safe — anything pushed
 * before the recheck is seen by it, anything pushed after it sees the
 * registration.
 */
final class RingChannel[A](requested: Int) extends Channel[A] {

  /** one-shot: whoever wins the CAS owes the completion */
  private final class Waiter(val resume: () => Unit):
    val claimed = AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)

  private val ring = Ring[A](requested)
  private val receivers = AtomicReference[List[Waiter]](Nil)
  private val senders = AtomicReference[List[Waiter]](Nil)
  private val closed = AtomicBoolean(false)
  private val failure = AtomicReference[Throwable | Null](null)

  val capacity: Int = ring.capacity

  // ── the waiter queues: append at the back, take from the front ───
  // Lists reversed on take, the same amortisation `Queue` uses, but
  // touched once per PARK rather than once per element — which is the
  // whole point: the hot path never comes here at all.

  @tailrec private def enqueue(q: AtomicReference[List[Waiter]], w: Waiter): Unit =
    val cur = q.get
    if !q.compareAndSet(cur, w :: cur) then enqueue(q, w)

  /** drop every waiter already claimed — they are dead, nobody will
   * run them, and without this they ACCUMULATE: the retry loops
   * register a fresh waiter per iteration and self-claim it, so
   * under load the queue grows until the heap is gone. Found by the
   * benchmark (OutOfMemoryError) after the stack-overflow fix turned
   * one symptom of the same cause into another. */
  private def purge(q: AtomicReference[List[Waiter]]): Unit =
    val _ = q.updateAndGet(_.filterNot(_.claimed.get))

  /** claim and run ONE live waiter, discarding any dead ones we pass;
   * answers whether one was actually woken */
  @tailrec private def wakeOne(q: AtomicReference[List[Waiter]]): Boolean =
    val cur = q.get
    if cur.isEmpty then false
    else
      // oldest last, since we prepend: take from the end
      val oldest = cur.last
      val rest = cur.init
      if !q.compareAndSet(cur, rest) then wakeOne(q)
      else if oldest.claim() then { oldest.resume(); true }
      else wakeOne(q)   // it was already claimed: dead, drop it

  /** claim and run everyone — what close owes every waiter */
  @tailrec private def wakeAll(q: AtomicReference[List[Waiter]]): Unit =
    val cur = q.get
    if cur.nonEmpty then
      if !q.compareAndSet(cur, Nil) then wakeAll(q)
      else cur.reverse.foreach(w => if w.claim() then w.resume())

  /**
   * THE CLOSE BARRIER, and why splitting elements out of the state
   * makes it necessary.
   *
   * `StmChannel` checks "is it open" and enqueues in ONE atomic
   * transition, so an element is never accepted into a channel that
   * has closed. Here those are two steps, and between them `close`
   * can run: the producer passes the check, the consumer sees closed
   * AND empty and ends, and then the push lands in a buffer nobody
   * will read — accepted by `k(true)` and never delivered. The
   * accounting test caught exactly that, in BOTH implementations,
   * which is what showed it is a property of the split rather than of
   * either mechanism.
   *
   * So a send announces itself before checking, and `close` waits for
   * announced sends to finish before it lets anyone conclude the
   * channel is drained. A short spin, never a park: an in-flight send
   * is a few instructions from completing.
   */
  private val inFlight = java.util.concurrent.atomic.AtomicInteger(0)

  private def awaitInFlight(): Unit =
    while inFlight.get > 0 do Thread.onSpinWait()

  private def endNow: End =
    val f = failure.get
    if f != null then Left(f.nn) else Right(None)

  // ── the operations ──────────────────────────────────────────────

  def sendAsync(a: A)(k: Boolean => Unit): Unit = attemptSend(a, k)

  /**
   * A LOOP, not a recursion, and that distinction is load-bearing.
   *
   * `resume` re-attempts the whole operation, and when the thread
   * that claims a waiter is the one that registered it — the recheck
   * path, which is the common case under contention — calling
   * `resume()` would re-enter this method on the same stack. A
   * producer against a channel that stays full then recurses once per
   * failed attempt and overflows the stack. Tests never caught it
   * (their consumers always kept up); the benchmark did, on its first
   * loaded run. Claiming our OWN waiter therefore continues the loop
   * instead, and only a claim by ANOTHER thread runs `resume`, which
   * costs that thread one frame and returns.
   */
  private def attemptSend(a: A, k: Boolean => Unit): Unit =
    var go = true
    while go do
      go = false
      inFlight.incrementAndGet(): Unit
      if closed.get then { inFlight.decrementAndGet(): Unit; k(false) }
      // a parked sender must go first, or FIFO among senders is lost
      else if senders.get.isEmpty && ring.push(a) then
        inFlight.decrementAndGet(): Unit
        k(true)
        val _ = wakeOne(receivers)
      else
        inFlight.decrementAndGet(): Unit
        val w = Waiter(() => attemptSend(a, k))
        enqueue(senders, w)
        // RECHECK: room may have opened, or the channel closed,
        // between the failed push and the registration
        if (!ring.isFull || closed.get) && w.claim() then
          // we took back our own registration: it is dead in the
          // queue now, so clear it rather than leaving it to pile up
          purge(senders)
          go = true

  def receiveAsync(k: End => Unit): Unit = attemptReceive(k)

  /** a loop for the same reason `attemptSend` is one */
  private def attemptReceive(k: End => Unit): Unit =
    var go = true
    while go do
      go = false
      attemptReceiveOnce(k, () => go = true)

  private def attemptReceiveOnce(k: End => Unit, again: () => Unit): Unit =
    val a = ring.pop()
    if a != null then
      k(Right(Some(a.nn)))
      val _ = wakeOne(senders)
    // CLOSED IS NOT ENOUGH: a closed channel still owes whatever is
    // buffered ("buffered elements still drain"). Checking `closed`
    // alone loses the element a producer pushed between this pop and
    // this check -- it answered k(true), so the element is accepted,
    // and ending the stream here strands it. Found by the accounting
    // test, round 170: one element accepted and never delivered.
      // CLOSED IS STILL NOT ENOUGH. The close barrier makes `close`
      // wait for announced sends, but a CONSUMER that reads the flag
      // directly does not: a producer can pass its open-check, close
      // can set the flag, the consumer can see closed-and-empty and
      // end, and only then does the push land -- accepted and lost.
      // The end of the stream is therefore closed AND nothing in
      // flight AND empty; with no sends in flight and the flag set,
      // no new send can start, so the emptiness read is stable.
    else if closed.get && inFlight.get == 0 && ring.isEmpty then k(endNow)
    else
      val w = Waiter(() => attemptReceive(k))
      enqueue(receivers, w)
      if (!ring.isEmpty || closed.get) && w.claim() then
        purge(receivers)
        again()

  def offer(a: A): Boolean =
    if closed.get then false
    else if !senders.get.isEmpty then false   // never jump a parked sender
    else
      inFlight.incrementAndGet(): Unit
      val ok = !closed.get && ring.push(a)
      inFlight.decrementAndGet(): Unit
      if ok then { val _ = wakeOne(receivers); true } else false

  def close(): Unit =
    closed.set(true)
    awaitInFlight()
    // everyone re-checks and finds the truth for themselves
    wakeAll(senders)
    wakeAll(receivers)

  def fail(e: Throwable): Unit =
    val _ = failure.compareAndSet(null, e)

  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closed.get

  private[okay] def cancelSend(cb: Boolean => Unit): Unit = ()
  private[okay] def cancelReceive(k: End => Unit): Unit = ()
}
