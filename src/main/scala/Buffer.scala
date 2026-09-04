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
private[okay] trait Buffer[A] {

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
}
