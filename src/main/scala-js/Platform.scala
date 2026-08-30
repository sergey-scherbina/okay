package okay

import scala.concurrent.{ExecutionContext, Promise}
import scala.scalajs.js.timers

/** the timer is setTimeout; the canceller is clearTimeout */
given Timer = new:
  def after(millis: Long)(k: () => Unit): () => Unit =
    val h = timers.setTimeout(millis.toDouble)(k())
    () => timers.clearTimeout(h)

/**
 * The event loop IS the scheduler: a fiber is a tree being driven
 * through callbacks (Async.drive), no thread anywhere. cancel stops
 * the drive at its next operation — a parked Await simply never
 * resumes. There is no CanBlock on JS, so a blocking join is a
 * compile error, not a frozen loop.
 */
given Scheduler = new:
  def fork[A](prog: () => A ! Async): Fiber[A] =
    val p = Promise[A]()
    val d = Async.Drive(p)
    d(prog())
    new Fiber[A]:
      def onComplete(k: Either[Throwable, A] => Unit): Unit =
        p.future.onComplete(t => k(t.toEither))(using ExecutionContext.parasitic)
      def cancel(): Unit = d.cancel()
