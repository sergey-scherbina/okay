package okay.resilience

import okay.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

/**
 * Hedged requests: start one attempt; if it has not answered after
 * `afterMillis`, start another, up to `max` in flight; the first
 * SUCCESS answers and every other attempt is cancelled. A failure is
 * not slowness — it starts nothing; when every attempt that was
 * started has failed, the answer is the last failure.
 *
 * This is not retry: attempts OVERLAP, so only an operation safe to
 * repeat belongs here (a read, an idempotent write). The Http adapter
 * enforces that by method; here it is the contract.
 */
object Hedge:
  def run[A](afterMillis: Long, max: Int = 2)(prog: => A ! Async)
            (using S: Scheduler, T: Timer): A ! Async =
    require(max >= 1, "hedging needs at least one attempt")
    Async.await[A] { k =>
      val fibers = AtomicReference(Vector.empty[Fiber[A]])
      val done = AtomicBoolean(false)
      val started = AtomicInteger(0)
      val failed = AtomicInteger(0)
      val timer = AtomicReference[() => Unit](() => ())

      def settle(r: Either[Throwable, A]): Unit =
        if !done.getAndSet(true) then
          timer.get()()
          fibers.get.foreach(_.cancel())
          k(r)

      def finish(r: Either[Throwable, A]): Unit = r match
        case Right(_) => settle(r)
        case Left(_) =>
          // every attempt started has failed and none is in flight: a
          // hedge now would be a retry, which this is not
          if failed.incrementAndGet() == started.get then settle(r)

      // called at most `max` times: once here, then once per timer
      // firing, and the timer is armed only while there is room
      def start(): Unit =
        if !done.get then
          val n = started.incrementAndGet()
          val f = S.fork(() => prog)
          fibers.updateAndGet(_ :+ f)
          if n < max then timer.set(T.after(afterMillis)(() => start()))
          f.onComplete(finish)

      start()
      () => settle(Left(new InterruptedException("hedge cancelled")))
    }
