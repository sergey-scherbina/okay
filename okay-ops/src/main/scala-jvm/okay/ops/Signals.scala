package okay.ops

import okay.*
import okay.given
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean

/**
 * The JVM half of graceful shutdown: SIGTERM and SIGINT arrive as a
 * shutdown hook, and a hook that returns lets the JVM exit — so the
 * hook must not return before the region has released the server.
 * The shape here: the MAIN thread blocks in `awaitSignal`, the hook
 * wakes it, and the hook then JOINS the main thread (bounded), which
 * keeps the JVM alive exactly as long as the release takes.
 */
object Signals:

  /**
   * Block the calling thread until the JVM is asked to stop; then
   * readiness off, `readinessDelayMillis` for endpoint removal to
   * propagate, `drain(graceMillis)`. Answers what the drain answered.
   * The caller's region releases the server after this returns.
   */
  def awaitSignal(l: Lifecycle, readinessDelayMillis: Long = 2_000, graceMillis: Long = 15_000)
                 (using Timer, CanBlock): Boolean =
    val asked = CountDownLatch(1)
    val caller = Thread.currentThread()
    install(() => asked.countDown(), caller, readinessDelayMillis + graceMillis + 5_000)
    asked.await()
    stop(l, readinessDelayMillis, graceMillis)

  /** the stop sequence itself, without the signal — what the hook
    * triggers, and what a test can call */
  def stop(l: Lifecycle, readinessDelayMillis: Long, graceMillis: Long)
          (using Timer, CanBlock): Boolean =
    l.beginDrain()
    if readinessDelayMillis > 0 then Async.run(Async.sleep(readinessDelayMillis)).runWith
    Async.run(l.drain(graceMillis)).runWith

  private val installed = AtomicBoolean(false)

  /** the hook, once per JVM: wake the waiter, then hold the JVM open
    * until the waiting thread has finished — bounded, so a release
    * that hangs cannot keep a process alive for ever */
  private def install(wake: () => Unit, waiter: Thread, holdMillis: Long): Unit =
    if installed.compareAndSet(false, true) then
      Runtime.getRuntime.addShutdownHook(Thread(() => {
        wake()
        try waiter.join(holdMillis) catch case _: InterruptedException => ()
      }, "okay-shutdown"))
