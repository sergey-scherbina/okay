package okay.reactive

import java.util.concurrent.Flow
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import okay.*
import okay.given

/**
 * Reactive Streams interop, over `java.util.concurrent.Flow` — the
 * JDK's own copy of the SPI, so this module adds no dependency to the
 * artifact it ships.
 *
 * WHY IT FITS. The protocol is DEMAND: a subscriber says `request(n)`
 * and the publisher may not deliver more than has been asked for.
 * That is what a bounded channel already is, and `receiveMany(n)`
 * already means "no more than n" — so the bridge is mostly a matter
 * of turning demand into reads rather than inventing backpressure.
 *
 * WHAT THE SPEC DEMANDS, and why each is a line of code here rather
 * than a hope. The numbered rules are from the Reactive Streams
 * specification 1.0.4, which the TCK checks:
 *
 *   1.1  never deliver more than requested
 *   1.3  onSubscribe/onNext/onError/onComplete are signalled
 *        SERIALLY — never concurrently
 *   1.6  after onError or onComplete, nothing more is signalled
 *   1.7  a terminated publisher must not signal again, ever
 *   1.9  onSubscribe must be called even for an immediately failing
 *        or empty publisher
 *   2.13 a null element is a NullPointerException, not a signal
 *   3.6  cancel() after termination is a no-op
 *   3.9  request(n) with n <= 0 must signal onError with an
 *        IllegalArgumentException
 *   3.17 demand is cumulative and may reach 2^63-1; adding past that
 *        is treated as unbounded rather than as an overflow
 */
object Reactive {

  /**
   * A `Source` as a `Flow.Publisher`.
   *
   * Every subscriber gets its OWN run of the source: the source is a
   * program, and running it twice does the work twice — which is the
   * library's own re-observation contract, and the honest reading of
   * a cold publisher.
   */
  def publisher[A](source: Source[A])(using Scheduler, CanBlock): Flow.Publisher[A] =
    (subscriber: Flow.Subscriber[? >: A]) =>
      // 1.9: onSubscribe comes first, always, even if what follows is
      // an immediate failure
      val sub = Pump(source, subscriber)
      subscriber.onSubscribe(sub)
      sub.start()

  /**
   * A `Flow.Publisher` as a `Source`.
   *
   * The other direction, and the one where the protocol's demand
   * becomes our capacity literally: the subscriber requests a
   * bufferful, the elements land in a bounded channel, and every
   * element taken out asks for one more. A slow consumer therefore
   * slows the publisher — which is what backpressure means on both
   * sides of this bridge, expressed once.
   *
   * `onError` reaches the reader as a failure of the stream, after
   * everything already delivered, because that is what `Channel.fail`
   * promises: a failure is the END, not an interruption.
   */
  def source[A](p: Flow.Publisher[A], capacity: Int = 256): Source[A] =
    val c = Channel[A](capacity)
    val sub = AtomicReference[Flow.Subscription | Null](null)
    p.subscribe(new Flow.Subscriber[A] {
      def onSubscribe(s: Flow.Subscription): Unit =
        // 2.5: a second onSubscribe is cancelled rather than used --
        // two subscriptions to one subscriber is a protocol error on
        // their side, not something to paper over
        if sub.compareAndSet(null, s) then s.request(capacity.toLong)
        else s.cancel()

      def onNext(a: A): Unit = c.offer(a): Unit
      def onError(e: Throwable): Unit = { c.fail(e); c.close() }
      def onComplete(): Unit = c.close()
    })

    // DEMAND FOLLOWS CONSUMPTION, not arrival. Asking for one more
    // inside `onNext` looks equivalent and is not: outstanding demand
    // then stays at the full window while the elements pile up in the
    // buffer, so a slow reader overflows it and `offer` starts
    // dropping. Elements are lost silently, which is the worst
    // possible way to be wrong. Here a request goes out when an
    // element has been TAKEN, so demand plus buffered never exceeds
    // the window.
    //
    // In BATCHES, not one at a time: requesting per element also puts
    // the subscriber back into the publisher on every element, and a
    // publisher that emits inside `request` — the spec allows it, and
    // asks only that the recursion be bounded (3.3) — then recurses
    // as deep as the stream is long.
    val refill = math.max(1, capacity / 2)
    def go(taken: Int): Source[A] =
      okay.effect[Writer % A + Async, Option[A]](
        Async.Await[Option[A]](k => { c.receiveAsync(k); () => () })).flatMap:
        case None => okay.pure(())
        case Some(a) =>
          okay.effect[Writer % A + Async, Unit](Writer(a)).flatMap: _ =>
            if taken + 1 >= refill then
              val s = sub.get
              if s != null then s.nn.request(refill.toLong)
              go(0)
            else go(taken + 1)
    okay.pure[Writer % A + Async, Unit](()).flatMap(_ => go(0))

  /**
   * A publisher that is already in an error state: every subscriber
   * gets `onSubscribe` and then `onError`, at once.
   *
   * Terminal signals are not limited by demand — a subscriber learns
   * that a stream is broken without having asked for anything — which
   * is why this cannot be expressed as "a source whose first pull
   * throws": the pump only pulls once something is requested, and a
   * subscriber that never requests would never be told.
   */
  def failed[A](e: Throwable): Flow.Publisher[A] =
    (subscriber: Flow.Subscriber[? >: A]) =>
      val done = AtomicBoolean(false)
      subscriber.onSubscribe(new Flow.Subscription {
        // 3.6/3.9 still apply to a subscription that will never
        // deliver: cancelling is a no-op, and a bad request is an
        // error the subscriber already has
        def request(n: Long): Unit = ()
        def cancel(): Unit = ()
      })
      if done.compareAndSet(false, true) then subscriber.onError(e)

  /**
   * One subscription: one fiber draining a channel fed by the source,
   * handing elements over only as fast as demand allows.
   *
   * The serial-signal rule (1.3) is what shapes this. Signals are
   * emitted by ONE thread — the pump's own — and `request` never
   * signals; it only adds demand and wakes the pump. That is the
   * cheapest way to be correct here: no lock, and no chance of two
   * threads calling `onNext` at once.
   */
  private final class Pump[A](source: Source[A], sub: Flow.Subscriber[? >: A])
                             (using Scheduler, CanBlock) extends Flow.Subscription {

    private val demand = AtomicLong(0)
    private val cancelled = AtomicBoolean(false)
    private val terminated = AtomicBoolean(false)
    private val wake = Channel[Unit](2)

    def request(n: Long): Unit =
      if !cancelled.get then
        // 3.9: a non-positive request is a protocol violation, and
        // the publisher must say so rather than ignore it
        if n <= 0 then fail(IllegalArgumentException(
          "reactive-streams 3.9: request must be positive, was " + n))
        else
          // 3.17: demand accumulates and saturates rather than wraps
          demand.updateAndGet(d => if d + n < 0 then Long.MaxValue else d + n): Unit
          wake.offer(()): Unit

    /** 3.6: cancelling a finished subscription does nothing */
    def cancel(): Unit =
      if cancelled.compareAndSet(false, true) then wake.offer(()): Unit

    private def fail(e: Throwable): Unit =
      // 1.6/1.7: one terminal signal, ever
      if terminated.compareAndSet(false, true) then sub.onError(e)

    private def complete(): Unit =
      if terminated.compareAndSet(false, true) then sub.onComplete()

    def start(): Unit =
      summon[Scheduler].fork { () => okay.async {
        try
          val it = source.toLazyList.iterator
          var running = true
          while running do
            if cancelled.get then running = false
            else if demand.get <= 0 then
              // nothing asked for: wait to be asked rather than spin
              wake.receiveBlocking(): Unit
            else if !it.hasNext then
              running = false
              if !cancelled.get then complete()
            else
              val a = it.next()
              // 2.13: the spec forbids null elements outright
              if a == null then
                running = false
                fail(NullPointerException("reactive-streams 2.13: null element"))
              else
                demand.decrementAndGet(): Unit
                // 1.1 holds because the decrement happened first: a
                // concurrent request can only raise the ceiling
                if !cancelled.get then sub.onNext(a)
        catch case e: Throwable => if !cancelled.get then fail(e)
      }}: Unit
  }
}
