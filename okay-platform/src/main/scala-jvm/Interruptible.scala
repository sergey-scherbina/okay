package okay

/**
 * Blocking code as an `Async` operation whose CANCEL INTERRUPTS it
 * (interop-lift-cancellation).
 *
 * A lifted action — Frege's `liftIO`, Clojure's `ok/lift` — used to run
 * as one step, in place, on the fiber's own thread. Whether a cancel
 * stopped it then depended on the SCHEDULER: Loom interrupts the fiber's
 * virtual thread, but a pool-threaded scheduler (`Schedulers.drive`)
 * cannot interrupt a thread it shares, so it reported the fiber finished
 * while the action ran on in the background (measured: TestFregeCancel).
 * Here the action gets a thread of its own, and the operation's canceller
 * interrupts exactly that thread, whatever the scheduler.
 *
 * JVM only: the languages that lift (Frege, Clojure) are JVM languages,
 * and a JS event loop has nothing to interrupt.
 */
object Interruptible:

  private val threads = java.util.concurrent.Executors.newCachedThreadPool { r =>
    val t = Thread(r, "okay-lifted")
    t.setDaemon(true)
    t
  }

  /** `action` on a thread of its own; a cancel interrupts that thread */
  def await[A](action: () => A): Async[A] = Async.Await[A] { k =>
    val running = threads.submit((() =>
      k(try Right(action()) catch case e: Throwable => Left(e))): Runnable)
    () => running.cancel(true): Unit
  }
