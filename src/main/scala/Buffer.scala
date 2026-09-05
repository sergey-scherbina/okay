package okay

/**
 * The element store a channel keeps its buffer in: a claim, a take,
 * and the batched forms of each.
 *
 * It exists because `SentinelChannel` needs to work bounded and
 * unbounded, and those are two data structures with one protocol.
 * Everything the channel relies on is here and nothing else is: a
 * position is won by ONE atomic, a value is published after the
 * position is won, and a batched take claims a run of published
 * positions with ONE atomic.
 *
 * That last property is the one that matters and the one an
 * implementation can quietly fail to provide, so it is stated: a
 * `popMany` that loops over `pop` is correct and worthless — measured,
 * batching the handshake without batching the queue bought 4%.
 */
trait Buffer[A] {

  /** the bound, or `Int.MaxValue` when there is none */
  def capacity: Int

  /** publish `a`; false only if a bounded buffer is full */
  def push(a: A): Boolean

  /**
   * Claim a position, and only THEN decide what goes into it: `a`
   * normally, `orElse` if `unless` reads true at that instant.
   * Answers the published value, or null if a bounded buffer is full.
   *
   * The decision must happen after the claim, because a channel that
   * can close needs "accepted" and "ordered" to be the same instant —
   * see `SentinelChannel`. And it takes a FLAG rather than a function
   * because whatever runs here runs between the claim and the
   * publish, where a concurrent `popMany` scan stops: a closure there
   * cost 65.6 elements per batch down to 43.5.
   */
  def pushDeciding(a: A, unless: java.util.concurrent.atomic.AtomicBoolean, orElse: A): A | Null

  /** take the oldest published element, or null if there is none
   * ready — `Option` here would allocate per element and undo the
   * point of the structure */
  def pop(): A | Null

  /** claim a run of consecutive published positions with ONE atomic
   * and hand each element to `sink` in order; answers how many */
  def popMany(max: Int)(sink: A => Unit): Int

  /**
   * Publish up to `n` elements, read from `src` by index, with ONE
   * atomic; answers how many were written.
   *
   * The default is the honest one-at-a-time answer, so an
   * implementation is correct before it is fast. An implementation
   * that can claim a run should override it — and the caveat from
   * `Channel.sendManyNow` applies: batch BOTH ends or neither, since
   * a consumer taking one element at a time keeps the buffer full and
   * every bulk attempt then fails its scan and falls back anyway.
   */
  def pushMany(n: Int)(src: Int => A): Int =
    var i = 0
    var go = true
    while go && i < n do
      if push(src(i)) then i += 1 else go = false
    i

  /** a snapshot, for reporting only — never for a decision */
  def size: Int

  /**
   * Nothing has been claimed here at all.
   *
   * NOT the same question as `hasReady`, and the difference is a
   * livelock. A position can be CLAIMED and not yet published — the
   * publisher is between its atomic and its store — and then this
   * answers "not empty" while `pop` answers "nothing ready". A
   * consumer that rechecks THIS before parking spins instead, and the
   * CPU it burns is taken from the very publisher it is waiting for.
   * On a bounded ring that window is two stores wide; on `Segments`
   * it can contain a whole segment allocation, which is how it
   * surfaced — hanging under load and passing on a quiet box.
   */
  def isEmpty: Boolean

  /** something is published and can be taken right now — the question
   * a receiver must ask before it decides to wait */
  def hasReady: Boolean

  /**
   * The part a producer should push to, decided once per send.
   *
   * A relaxed buffer keeps one producer's elements in order only
   * because that producer stays on one part — and "producer" cannot
   * mean "thread", because a send that PARKS is resumed on the thread
   * of whoever woke it, which is the consumer. Binding by thread of
   * execution therefore scatters exactly the sends that had to wait,
   * and the per-producer order law fails on precisely the elements
   * that were under contention.
   *
   * So the route is taken once, at the first attempt, and carried
   * through every retry. Buffers with a single order ignore it.
   */
  def route(): Int = 0

  /** as `push`, to a route taken earlier */
  def pushAt(@annotation.unused route: Int, a: A): Boolean = push(a)

  /** as `pushDeciding`, to a route taken earlier */
  def pushDecidingAt(@annotation.unused route: Int, a: A,
                     unless: java.util.concurrent.atomic.AtomicBoolean,
                     orElse: A): A | Null = pushDeciding(a, unless, orElse)

  /**
   * The part the most recent take by THIS thread came from.
   *
   * A hint for waking the right producer, and only that: a wrong
   * answer costs a wasted wakeup, never correctness. It exists
   * because a freed slot belongs to ONE part, and a channel that
   * wakes an arbitrary sender wakes one whose own part is still full
   * -- with k parts, one wakeup in k is useful and the rest is
   * churn. Measured: 111546us against a single ring's 3150 at
   * sixteen producers.
   */
  def lastRoute: Int = 0

  /** is there room on that route right now — the question a sender
   * must ask before it decides to wait */
  def hasRoomAt(@annotation.unused route: Int): Boolean = hasRoom

  /**
   * Is there room for THIS thread to push right now — the question a
   * sender must ask before it decides to wait.
   *
   * Not the same as `size < capacity`, and the difference is a
   * spin. On a relaxed buffer a producer is bound to ONE part, so the
   * sum having room says nothing about whether ITS part does: the
   * sender wakes itself, fails to push again, and never stops. It is
   * the mirror of `isEmpty` versus `hasReady`, and it cost a
   * benchmark forty-five minutes of spinning before the stack said so.
   */
  def hasRoom: Boolean = size < capacity

  /**
   * How many independent orders this buffer keeps. One means a single
   * global FIFO: what goes in first comes out first, full stop. More
   * than one means a RELAXED buffer — each part is a FIFO of its own
   * and a producer stays on one part, so the elements of ONE producer
   * keep their order, while the interleaving between producers does
   * not.
   *
   * It is stated here rather than left implicit because a relaxed
   * buffer that quietly passes a global-FIFO test is a test that is
   * not testing anything.
   */
  def parts: Int = 1

  /**
   * The most parts this buffer will ever have.
   *
   * `parts` is what exists NOW; a buffer whose parts appear as
   * producers do will report more later. Anything the channel sizes
   * ONCE — its per-part queues of waiting senders — must be sized by
   * this instead, or the per-part wakeup silently degrades into the
   * single-queue behaviour it was written to replace: measured, that
   * is 86876us against 99 at sixteen producers.
   */
  def maxParts: Int = parts

  /**
   * Put `mark` where nothing can come out after it, and answer how
   * many copies were placed BY THIS CALL.
   *
   * A part with no room right now cannot take its mark, so this may
   * place fewer than `parts` and must be called again as room frees
   * up. It is idempotent per part: a part already sealed is not
   * sealed twice. The caller keeps asking until the total reaches
   * `parts`.
   *
   * A channel whose termination travels as an element needs exactly
   * this and cannot get it from `push` alone: with one order a single
   * mark suffices, but with `parts` orders a mark in one part is
   * reached while other parts still hold elements, and the stream
   * would end early on what was already accepted. Sealing every part
   * and counting the marks back keeps drain-on-close.
   */
  def seal(mark: A): Int = if push(mark) then 1 else 0
}
