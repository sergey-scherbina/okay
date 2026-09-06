package okay

/**
 * The one-shot handoff a blocking receive waits on — and the callback
 * that fills it, in ONE object.
 *
 * WHY ONE OBJECT. `CanBlock.block` allocated a slot and then a lambda
 * that closed over it, once per element, and a `receiveBlocking` that
 * found its element already buffered paid both for nothing. Counted on
 * the actor's loop (docs/benchmarks.md §17): five objects per message —
 * the slot, the lambda, the `Async.Await`, and the `Right(Some(_))` the
 * callback type wraps the element in — for a message that was already
 * there. Here the callback IS the slot, so a receive that waits
 * allocates this and nothing else, and a receive that does not wait
 * (`Channel.receiveInto` answering true) fills it without even the
 * `End` pair: `got` writes the element straight in.
 *
 * WHY THE TRY IS NOT A SEPARATE POLL. `actor-receive-offer-first` put
 * a poll before the handshake and lost 19% with an empty mailbox: a
 * failed poll is one more read of the cache line the PRODUCER writes,
 * where a failed `offer` reads the sender's own. So the try lives
 * inside `receiveInto`'s first scan — the scan the handshake would
 * have done anyway — and a miss costs exactly what it cost before.
 *
 * Platform-specific only in how a waiter is parked and signalled:
 * `CanBlock.handoff` makes the platform's subclass, `CanBlock.await`
 * parks on it. Typed on `A` throughout, so nothing here casts.
 */
abstract class Handoff[A] extends (Either[Throwable, Option[A]] => Unit):
  private var value: A = scala.compiletime.uninitialized
  private var ended = false
  private var failure: Throwable | Null = null
  /** the release fence: a reader that sees it true sees everything
   * written before it */
  @volatile var filled: Boolean = false

  /** the fast path: an element that was ready. The caller is still on
   * the stack, so no signal is needed — only the write and the fence. */
  final def got(a: A): Unit =
    value = a
    filled = true

  /** the callback path: an `End`, from a later wakeup or a synchronous
   * end/failure. Fills, fences, and signals a parked waiter if any. */
  final def apply(e: Either[Throwable, Option[A]]): Unit =
    e match
      case Right(Some(a)) => value = a
      case Right(None) => ended = true
      case Left(t) => failure = t
    filled = true
    signal()

  /** what `receiveBlocking` answers, once `filled`: the element, `None`
   * at the end, or the producer's failure thrown — as `receive`'s
   * program would have */
  final def answer: Option[A] =
    val f = failure
    if f != null then throw f.nn
    else if ended then None
    else Some(value)

  /** wake whoever `CanBlock.await` parked on this handoff, if anyone */
  protected def signal(): Unit
