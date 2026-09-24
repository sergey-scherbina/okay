package okay2

import scala.concurrent.{ExecutionContext, Promise}
import scala.scalajs.js.timers
import okay2.async.{Async, Fiber, PlatformDefaults, Scheduler, Timer}

/**
 * okay2-platform on Scala.js (okay2-cross): the EVENT LOOP is the
 * scheduler — a fiber is a tree driven through callbacks, no thread
 * anywhere — and the timer is `setTimeout`. There is NO `CanBlock`:
 * this installs `PlatformDefaults`, not `BlockingDefaults`, so a
 * blocking join is a compile error here rather than a frozen loop.
 * Run a program with `Async.runAsync` (a `Future`), join a fiber with
 * `joinAsync`.
 */
package object platform {

  val timer: Timer = new Timer {
    def after(millis: Long)(k: () => Unit): () => Unit = {
      val h = timers.setTimeout(millis.toDouble)(k())
      () => timers.clearTimeout(h)
    }
  }

  /** cancel stops the drive at its next operation — a parked Await
   * simply never resumes */
  val scheduler: Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val p = Promise[A]()
      val d = new Async.PromiseDrive[A](p)
      d.apply(prog())
      new Fiber[A] {
        def onComplete(k: Either[Throwable, A] => Unit): Unit =
          p.future.onComplete(t => k(t.toEither))(ExecutionContext.parasitic)
        def cancel(): Unit = d.cancel()
      }
    }
  }

  implicit val js: PlatformDefaults = new PlatformDefaults {
    def timer: Timer = platform.timer
    def scheduler: Scheduler = platform.scheduler
  }

  /** Node's `net`, as a byte stream */
  implicit val net: Net = NodeNet
}
