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

      /**
       * Called at most `max` times: once here, then once per timer
       * firing, and the timer is armed only while there is room.
       *
       * EVERY PUBLICATION IS RE-CHECKED (hedge-start-races,
       * 2026-09-09). The `done` at the top is not enough: an attempt
       * already in flight can answer while this one is forking, and
       * `settle` cancels the fibers it can SEE and disarms the timer
       * it can see. So a fiber added after that sweep would run on
       * with nobody to stop it — for a hedged request, a duplicate
       * that outlives the answer — and a timer armed after it would
       * sit until it fired. Each is undone here, by the thread that
       * published it, and both undos are idempotent: `settle` may
       * have cancelled the same fiber, and cancelling a spent timer
       * does nothing.
       */
      def start(): Unit =
        if !done.get then
          val n = started.incrementAndGet()
          val f = S.fork(() => prog)
          fibers.updateAndGet(_ :+ f)
          if done.get then f.cancel()          // answered while we forked
          if n < max then
            val disarm = T.after(afterMillis)(() => start())
            timer.set(disarm)
            if done.get then disarm()          // answered while we armed
          f.onComplete(finish)

      start()
      () => settle(Left(new InterruptedException("hedge cancelled")))
    }
