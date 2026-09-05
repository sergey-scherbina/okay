package okay

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/**
 * A mark travelling in the ring alongside the elements. `end` closes
 * the stream at that exact position; a void is a slot a sender won
 * and then declined to use, which the receiver steps over.
 *
 * It is `private[okay]` and has no public constructor, so no caller
 * can put one into a channel: a `Channel[Any]` cannot be handed a
 * value that this class would confuse for termination.
 */
private[okay] final class Mark(val end: Boolean)

/**
 * The default channel: a mutable ring for the mechanism and a mark in
 * the FIFO stream for the guarantee.
 *
 * WHY THIS SHAPE, measured rather than guessed. `StmChannel` gives
 * the same contract by making the whole channel one immutable value
 * rebuilt per transaction. Profiled at chunk granularity, 40% of its
 * leaf samples are `List.reverse` and about 75% the persistent queue
 * around it: `immutable.Queue` is two lists with an amortized
 * reverse, and in a bulk receive the amortization does not hold,
 * because the producer keeps refilling `in` while the consumer drains
 * `out`, so nearly every batch reverses a buffer up to 1024 deep.
 * It measured 166.1us against `zio.Queue` carrying our contract at
 * 124.0 — the one lane we lost.
 *
 * The same contract assembled the other way round — a weak mutable
 * ring plus a sentinel riding the buffer — measured 115.9. This is
 * that, made a real channel: 2.4% over the weak mechanism, where the
 * invariant cost 47%.
 *
 * WHY THE MARK IS IN THE RING and not beside it. The hard part of
 * drain-on-close is not draining, it is that "the channel is open"
 * and "my element is in" are two facts a sender learns at two
 * different instants. Between them close can land, and then the
 * element is either stranded after the end (the sender was told true)
 * or resent (told false, but published anyway). Four `RingChannel`
 * drafts met that window and each patch — an in-flight counter, a
 * two-phase close — moved it rather than closed it.
 *
 * Here termination is an ELEMENT. It takes a position through the
 * same tail CAS as everything else, so the ring's own atomics order
 * it against every send, and there is no second structure to
 * reconcile. A sender decides what to publish only after winning its
 * position (`Ring.pushDeciding`): if close has already landed by
 * then, it publishes a void and answers false. So a sender ordered
 * after the end mark ALWAYS sees the close — its own CAS is the fence
 * — and never claims acceptance, while a sender ordered before it is
 * delivered. The window has nowhere to open.
 */
final class SentinelChannel[A](buf: Buffer[A | Mark]) extends Channel[A] {

  /** the bounded channel: a fixed ring */
  def this(requested: Int) = this(Ring[A | Mark](requested))

  private final class Waiter(val resume: () => Unit):
    val claimed = AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)

  private val ring: Buffer[A | Mark] = buf
  private val receivers = AtomicReference[List[Waiter]](Nil)
  /**
   * Senders wait PER PART, because room appears per part. One queue
   * over a partitioned buffer means a freed slot wakes an arbitrary
   * sender, who finds its own part still full and parks again; with k
   * parts that is one useful wakeup in k, and it measured 111546us at
   * sixteen producers against a single ring's 3150.
   *
   * A single-order buffer has exactly one of these, so nothing about
   * the ordinary path changes.
   */
  private val senders: Array[AtomicReference[List[Waiter]]] =
    Array.fill(if buf.parts < 1 then 1 else buf.parts)(AtomicReference[List[Waiter]](Nil))

  private def sendersAt(route: Int): AtomicReference[List[Waiter]] =
    senders(if senders.length == 1 then 0 else Math.floorMod(route, senders.length))

  /** wake a sender waiting where room has just appeared */
  private def wakeSender(): Unit =
    val _ = wakeOne(sendersAt(ring.lastRoute))

  private def wakeAllSenders(): Unit =
    var i = 0
    while i < senders.length do { wakeAll(senders(i)); i += 1 }

  /** close has been decided: no further element is accepted */
  private val closing = AtomicBoolean(false)
  /** the end mark did not fit yet; the next freed slot takes it */
  private val endPending = AtomicBoolean(false)
  /** the end mark has been REACHED by a receiver: nothing can arrive */
  private val ended = AtomicBoolean(false)
  /** how many parts have their end mark in, and how many marks a
   * receiver has met. The stream is over when the second reaches the
   * buffer's `parts`: every independent order has been sealed AND
   * drained. Counting marks met against `parts` rather than against
   * marks placed is what stops an early end while other parts are
   * still full and unsealed. */
  private val partsSealed = java.util.concurrent.atomic.AtomicInteger(0)
  private val metEnds = java.util.concurrent.atomic.AtomicInteger(0)
  /** the end mark, once a receiver has taken it out of the ring but
   * not yet answered with it (a bulk receive that also carried
   * elements) */
  private val reached = AtomicReference[Mark | Null](null)
  private val failure = AtomicReference[Throwable | Null](null)
  /**
   * How many MARKS the ring currently holds. Marks are rare — one end
   * and, only under a close race, a void — so this counter is not on
   * the hot path, which is the whole reason it counts marks and not
   * elements. Elements outstanding is then `ring.size - marks`, and
   * that is all `finished` needs.
   *
   * A first draft counted elements instead: an atomic increment per
   * send and decrement per receive, on a cell shared by producer and
   * consumer. It answered the same question and cost 2x — it slowed
   * the PRODUCER, which shrank the batches the consumer could take
   * (8.0 elements against `AbruptChannel`'s 17.5), and the loss
   * compounded from there.
   */
  private val marks = java.util.concurrent.atomic.AtomicInteger(0)

  /** the one declined-slot value, held for the life of the channel:
   * allocating it per declined send would put an allocation in the
   * claim-to-publish window, which is the last place that can afford
   * one */
  private val void: A | Mark = Mark(false)

  val capacity: Int = ring.capacity

  /**
   * The ONE cast in this file, and the argument for it.
   *
   * A slot holds `A | Mark`, and every `Mark` is matched on the line
   * above every call to this. What is left is an element — but the
   * compiler cannot know that, because `A` is unconstrained and could
   * in principle BE `Mark`, so the union does not narrow.
   *
   * It cannot: `Mark` is `private[okay]` and has no public
   * constructor, so the only `Mark` values that exist are the ones
   * this class writes into its own ring, and each is matched before
   * this narrowing runs.
   *
   * The typed routes were tried and each costs more than it saves. A
   * sealed `Elem(a) | Mark` wrapper is an allocation per element,
   * which is the exact cost this channel exists to remove. A parallel
   * tag array inside `Ring` keeps the types honest but adds an atomic
   * write and read per operation on the hot path, and changes a
   * primitive the laws already cover. Recording an end POSITION
   * instead of a mark is the design whose two-facts-two-instants
   * window broke four ring-channel drafts.
   */
  private inline def element(x: A | Mark): A = x.asInstanceOf[A]

  private def enqueue(q: AtomicReference[List[Waiter]], w: Waiter): Unit =
    var go = true
    while go do
      val cur = q.get
      if q.compareAndSet(cur, w :: cur) then go = false

  private def wakeOne(q: AtomicReference[List[Waiter]]): Boolean =
    var out = false
    var go = true
    while go do
      val cur = q.get
      if cur.isEmpty then go = false
      else if q.compareAndSet(cur, cur.init) then
        val oldest = cur.last
        if oldest.claim() then { oldest.resume(); out = true; go = false }
    out

  private def wakeAll(q: AtomicReference[List[Waiter]]): Unit =
    var go = true
    while go do
      val cur = q.get
      if cur.isEmpty then go = false
      else if q.compareAndSet(cur, Nil) then
        cur.reverse.foreach(w => if w.claim() then w.resume())
        go = false

  /**
   * Put the end mark in as soon as there is room. Nothing can be
   * accepted after `closing`, so whatever is already buffered stays
   * ahead of it and the mark is the last thing in the stream.
   *
   * ONE MARK PER ORDER. A buffer with several independent orders — a
   * relaxed one, `MultiFifo` — needs a mark in each, or the stream
   * ends at the first mark reached while other parts still hold
   * elements that were already accepted. `seal` answers how many it
   * placed, and the receiver counts them back: only the LAST one ends
   * the stream.
   */
  private def placeEnd(): Unit =
    if endPending.get then
      val placed = ring.seal(Mark(true))
      if placed > 0 then
        marks.addAndGet(placed): Unit
        // a FULL part cannot take its mark now; keep asking, or the
        // stream never ends and the producers behind it never move
        if partsSealed.addAndGet(placed) >= ring.parts then endPending.set(false)

  private def endAnswer: End =
    val e = failure.get
    if e != null then Left(e.nn) else Right(None)

  /** the failure is read HERE, at the end, and not carried on the
   * mark: `fail` records without closing — a merge whose one source
   * breaks must let the other finish sending — so the failure can
   * arrive after the mark was already placed */
  private def endReached(): End =
    ended.set(true)
    wakeAll(receivers)
    endAnswer

  def sendAsync(a: A)(k: Accepted): Unit =
    // the route is taken HERE, once, and carried through every retry:
    // a parked send resumes on the waker's thread, so asking again
    // there would scatter one producer's elements across parts
    attemptSend(a, granted0 = false, ring.route())(k)

  private def attemptSend(a: A, granted0: Boolean, route: Int)(k: Accepted): Unit =
    val granted = granted0
    var go = true
    while go do
      go = false
      if closing.get then k(false)
      else if granted || sendersAt(route).get.isEmpty then
        // the decision rides INSIDE the claim: what comes back is
        // what the ring published at the position just won
        ring.pushDecidingAt(route, a, closing, void) match
          case null =>
            // full: park, and re-check afterwards in case a pop freed
            // a slot between the failed push and the registration
            val w = Waiter(() => attemptSend(a, granted0 = true, route)(k))
            enqueue(sendersAt(route), w)
            if (ring.hasRoomAt(route) || closing.get) && w.claim() then
              val _ = sendersAt(route).updateAndGet(_.filterNot(_.claimed.get))
              go = true
          case _: Mark =>
            // close landed between the open check and the claim; the
            // slot carries a void the receiver steps over
            marks.incrementAndGet(): Unit
            k(false)
            val _ = wakeOne(receivers)
          case _ =>
            k(true)
            val _ = wakeOne(receivers)
      else
        val w = Waiter(() => attemptSend(a, granted0 = true, route)(k))
        enqueue(sendersAt(route), w)
        if (ring.hasRoomAt(route) || closing.get) && w.claim() then
          val _ = sendersAt(route).updateAndGet(_.filterNot(_.claimed.get))
          go = true

  def receiveAsync(k: End => Unit): Unit =
    var go = true
    while go do
      go = false
      if ended.get then k(endAnswer)
      else
        var answered = false
        var stepped = true
        while stepped && !answered do
          stepped = false
          ring.pop() match
            case null => ()
            case m: Mark =>
              marks.decrementAndGet(): Unit
              wakeSender()
              placeEnd()
              // a void: the slot a sender won and declined. Step over
              // it and keep looking within the same call
              if m.end then
                if metEnds.incrementAndGet() >= ring.parts then
                  k(endReached()); answered = true
                else stepped = true   // another order still has elements
              else stepped = true
            case other =>
              k(Right(Some(element(other))))
              wakeSender()
              placeEnd()
              answered = true
        // the ring is spent. If a bulk receive already took the end
        // mark out and answered with elements instead, this is where
        // that mark is finally delivered -- parking here would park
        // for good, since close has already woken everyone it will
        val orphan = reached.get
        if !answered && orphan != null then
          k(endReached())
          answered = true
        if !answered then
          val w = Waiter(() => receiveAsync(k))
          enqueue(receivers, w)
          if (ring.hasReady || ended.get) && w.claim() then
            val _ = receivers.updateAndGet(_.filterNot(_.claimed.get))
            go = true

  /**
   * The bulk receive: one head CAS for the whole run.
   *
   * Voids are stepped over here as they are singly. The end mark is
   * different: a batch can carry elements AND the end, and the
   * convention is that an empty chunk IS the end, so the two cannot
   * ride out together. The mark is parked in `reached` and delivered
   * on the next call — sound because nothing but voids can follow it,
   * so no element can be stranded behind it.
   */
  private[okay] override def receiveManyAsync(max: Int)
                                             (k: Either[Throwable, Chunk[A]] => Unit): Unit =
    if ended.get then k(endAnswer.map(_ => Chunks.emptyChunk[A]))
    else
      val room = if max < ring.capacity then max else ring.capacity
      val out = ChunkBuf[A](room)
      var n = 0
      val took = ring.popMany(room):
        case m: Mark =>
          marks.decrementAndGet(): Unit
          // the last end mark is the one that ends it; the earlier
          // ones only say that THAT part is spent
          if m.end && metEnds.incrementAndGet() >= ring.parts then reached.set(m)
        case other => { out.update(n, element(other)); n += 1 }
      if n > 0 then
        var i = 0
        while i < took do { wakeSender(); i += 1 }
        placeEnd()
        k(Right(out.take(n)))
      else
        val m = reached.get
        if m != null then k(endReached().map(_ => Chunks.emptyChunk[A]))
        else
          if took > 0 then
            var i = 0
            while i < took do { wakeSender(); i += 1 }
            placeEnd()
          // nothing but voids, or nothing at all: the honest single
          // receive, its one element handed over as a chunk of one
          receiveAsync(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))

  def offer(a: A): Boolean =
    // offer never parks, so it takes its route here and now
    val r = ring.route()
    if closing.get || !sendersAt(r).get.isEmpty then false
    else
      ring.pushDecidingAt(r, a, closing, void) match
        case null => false
        case _: Mark =>
          marks.incrementAndGet(): Unit
          val _ = wakeOne(receivers); false
        case _ => { val _ = wakeOne(receivers); true }

  def close(): Unit =
    if closing.compareAndSet(false, true) then
      // a parked sender was never accepted, so refusing it is the
      // truthful answer, and it clears the way for the mark
      wakeAllSenders()
      endPending.set(true)
      placeEnd()
      wakeAll(receivers)

  /** RECORD, do not close. A failed producer does not silence a
   * healthy one: the failure rides the END of the stream, which is
   * where a receiver meets it after everything already accepted has
   * been delivered */
  def fail(e: Throwable): Unit =
    val _ = failure.compareAndSet(null, e)

  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closing.get

  /** nothing further can be delivered. Either a receiver has reached
   * the end mark — which by construction means everything ahead of it
   * was delivered — or the channel is closed, so nothing more can be
   * accepted, and nothing accepted is still outstanding */
  private[okay] def finished: Boolean =
    ended.get || (closing.get && ring.size <= marks.get)

  private[okay] def cancelSend(cb: Accepted): Unit = ()
  private[okay] def cancelReceive(k: End => Unit): Unit = ()
}

object SentinelChannel {

  /**
   * A channel over a buffer of the caller's choosing.
   *
   * The factory is POLYMORPHIC, and that is not ceremony: this channel
   * stores more than the caller's element type, because termination
   * travels as a mark through the same buffer. Only the channel knows
   * what it needs to hold, so it asks for a way to make a buffer of
   * whatever type it decides — and `Mark` stays private, which means
   * no caller can put one in and forge an end of stream.
   *
   * {{{
   * SentinelChannel.over[Int](1024)([T] => (n: Int) => Ring[T](n))
   * SentinelChannel.over[Int](0)([T] => (_: Int) => Segments[T]())
   * }}}
   */
  def over[A](capacity: Int)(newBuffer: [T] => Int => Buffer[T]): SentinelChannel[A] =
    SentinelChannel[A](newBuffer[A | Mark](capacity))
}
