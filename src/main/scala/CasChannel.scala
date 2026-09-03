package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec

/**
 * An UNBOUNDED channel on a Michael-Scott queue — the one candidate
 * that can replace `StmChannel` where `RingChannel` cannot go.
 *
 * WHY THIS EXISTS AT ALL, since a segmented ring would dominate it on
 * paper: `RingChannel` is bounded (a ring is a fixed array), and
 * `Channel.merge`'s own default capacity is `Int.MaxValue`. So the
 * unbounded path has no alternative to `StmChannel` today, and the
 * segmented ring that would beat this is not written. Filing this as
 * "dominated in theory" and stopping was the wrong call — it compared
 * MS to something that does not exist rather than to the default it
 * would actually replace (backlog-dedup, corrected here).
 *
 * WHAT IT CHANGES against `StmChannel`, concretely: a send there
 * costs three allocations (a cons cell, a `Queue`, a `State`) and
 * CASes the whole six-field state, so a concurrent operation on an
 * unrelated field re-runs the entire transition (28-49% CAS-failure
 * rates, channel-cas-contention). Here a send costs ONE node and
 * CASes a single pointer, and the head and tail contend separately.
 *
 * The waiter protocol is `RingChannel`'s, for the reason given there:
 * a waiter is CLAIMED, never borrowed, and whoever claims it owes the
 * completion.
 */
final class CasChannel[A] extends Channel[A] {

  private final class Node(val value: A | Null):
    val next = AtomicReference[Node | Null](null)

  private final class Waiter(val resume: () => Unit):
    val claimed = AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)

  // the classic dummy-node queue: head is the node BEFORE the next
  // element, tail is at or near the last
  private val dummy = Node(null)
  private val head = AtomicReference[Node](dummy)
  private val tail = AtomicReference[Node](dummy)

  private val receivers = AtomicReference[List[Waiter]](Nil)
  private val closed = AtomicBoolean(false)
  private val failure = AtomicReference[Throwable | Null](null)

  @tailrec private def push(a: A): Unit =
    val n = Node(a)
    val t = tail.get
    val nx = t.next.get
    if nx != null then { tail.compareAndSet(t, nx.nn): Unit; push(a) }   // help, retry
    else if t.next.compareAndSet(null, n) then tail.compareAndSet(t, n): Unit
    else push(a)

  @tailrec private def pop(): A | Null =
    val h = head.get
    val nx = h.next.get
    if nx == null then null
    else if head.compareAndSet(h, nx.nn) then nx.nn.value
    else pop()

  private def isEmpty: Boolean = head.get.next.get == null

  @tailrec private def enqueue(w: Waiter): Unit =
    val cur = receivers.get
    if !receivers.compareAndSet(cur, w :: cur) then enqueue(w)

  @tailrec private def wakeOne(): Boolean =
    val cur = receivers.get
    if cur.isEmpty then false
    else
      val oldest = cur.last
      if !receivers.compareAndSet(cur, cur.init) then wakeOne()
      else if oldest.claim() then { oldest.resume(); true }
      else wakeOne()

  @tailrec private def wakeAll(): Unit =
    val cur = receivers.get
    if cur.nonEmpty then
      if !receivers.compareAndSet(cur, Nil) then wakeAll()
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

  /** unbounded: a send never parks, so it is always immediate */
  def sendAsync(a: A)(k: Boolean => Unit): Unit =
    inFlight.incrementAndGet(): Unit
    if closed.get then { inFlight.decrementAndGet(): Unit; k(false) }
    else
      push(a)
      inFlight.decrementAndGet(): Unit
      k(true)
      val _ = wakeOne()

  def receiveAsync(k: End => Unit): Unit = attempt(k)

  private def attempt(k: End => Unit): Unit =
    val a = pop()
    if a != null then k(Right(Some(a.nn)))
    // closed is not enough: a closed channel still owes what is
    // buffered (the bug the accounting test caught in RingChannel)
    else if closed.get && isEmpty then k(endNow)
    else
      val w = Waiter(() => attempt(k))
      enqueue(w)
      if (!isEmpty || closed.get) && w.claim() then w.resume()

  def offer(a: A): Boolean =
    inFlight.incrementAndGet(): Unit
    val ok = !closed.get
    if ok then push(a)
    inFlight.decrementAndGet(): Unit
    if ok then { val _ = wakeOne(); true } else false

  def close(): Unit = { closed.set(true); awaitInFlight(); wakeAll() }
  def fail(e: Throwable): Unit = { val _ = failure.compareAndSet(null, e) }
  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closed.get

  private[okay] def cancelSend(cb: Boolean => Unit): Unit = ()
  private[okay] def cancelReceive(k: End => Unit): Unit = ()
}
