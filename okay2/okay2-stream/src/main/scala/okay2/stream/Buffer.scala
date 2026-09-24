package okay2.stream

import java.util.concurrent.atomic.AtomicBoolean

/**
 * The element store a channel keeps its buffer in: a claim, a take, and
 * the batched forms of each — okay-stream's Buffer.scala (okay2 spec
 * stage 28). Everything the channel relies on is here and nothing else
 * is: a position is won by ONE atomic, a value is published after the
 * position is won, and a batched take claims a run of published
 * positions with ONE atomic. A `popMany` that loops over `pop` is
 * correct and worthless — measured in the Scala 3 core, batching the
 * handshake without batching the queue bought 4%.
 *
 * `A >: Null` is Scala 3's `A | Null`: an empty answer is `null` AT THE
 * ELEMENT TYPE, with no cast. The channels store `Buffer[Any]`.
 */
trait Buffer[A >: Null] {

  /** the bound, or `Int.MaxValue` when there is none */
  def capacity: Int

  /** publish `a`; false only if a bounded buffer is full */
  def push(a: A): Boolean

  /**
   * Claim a position, and only THEN decide what goes into it: `a`
   * normally, `orElse` if `unless` reads true at that instant. Answers
   * the published value, or null if a bounded buffer is full. A FLAG,
   * not a function: whatever runs here runs between the claim and the
   * publish, where a concurrent `popMany` scan stops (a closure there
   * cost the Scala 3 core 65.6 elements per batch down to 43.5).
   */
  def pushDeciding(a: A, unless: AtomicBoolean, orElse: A): A

  /** take the oldest published element, or null if none is ready */
  def pop(): A

  /** claim a run of consecutive published positions with ONE atomic and
   * hand each element to `sink` in order; answers how many */
  def popMany(max: Int)(sink: A => Unit): Int

  /** publish up to `n` elements read from `src` by index; the default is
   * the honest one-at-a-time answer */
  def pushMany(n: Int)(src: Int => A): Int = {
    var i = 0
    var go = true
    while (go && i < n) { if (push(src(i))) i += 1 else go = false }
    i
  }

  /** a snapshot, for reporting only — never for a decision */
  def size: Int

  /** nothing has been CLAIMED here — not the same question as `hasReady`:
   * a claimed, unpublished position answers "not empty" here and
   * "nothing ready" there, and a receiver rechecking this before parking
   * spins on the publisher it waits for */
  def isEmpty: Boolean

  /** something is published and can be taken right now */
  def hasReady: Boolean

  /** the part a producer pushes to, taken ONCE per send and carried
   * through every retry: a parked send resumes on its waker's thread */
  def route(): Int = 0

  def pushAt(route: Int, a: A): Boolean = { val _ = route; push(a) }

  def pushDecidingAt(route: Int, a: A, unless: AtomicBoolean, orElse: A): A = {
    val _ = route
    pushDeciding(a, unless, orElse)
  }

  /** the same push made BY THE CHANNEL for a producer that parked — the
   * calling thread is not the producer, so a buffer that learns from its
   * caller (`Growing`) must not learn from this */
  def pushDecidingAtOnBehalf(route: Int, a: A, unless: AtomicBoolean, orElse: A): A =
    pushDecidingAt(route, a, unless, orElse)

  /** the part THIS thread's last take came from — which senders to wake */
  def lastRoute: Int = 0

  def hasRoomAt(route: Int): Boolean = { val _ = route; hasRoom }

  /** room for THIS thread's push now — not `size < capacity` on a relaxed
   * buffer, where a producer is bound to one part */
  def hasRoom: Boolean = size < capacity

  /** how many independent orders this buffer keeps: one is a global FIFO */
  def parts: Int = 1

  /** the most parts it will ever have — what a channel sizes its per-part
   * waiter queues by, once */
  def maxParts: Int = parts

  /** put `mark` where nothing can come out after it, one per part;
   * answers how many were placed BY THIS CALL (a full part cannot take
   * its mark yet, so the caller asks again as room frees) */
  def seal(mark: A): Int = if (push(mark)) 1 else 0
}
