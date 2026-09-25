package okay

import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.locks.LockSupport

/**
 * PARKED WORKERS WITH BIG STACKS, reused across `Cont` stack switches
 * (specs/cont-stack.md plan stage D3; JVM and Native share it).
 *
 * WHY: the stage-A A/B (2026-09-25) found a switch on the count road
 * to be the whole of statePara's 4.9x — 27.3 µs on the base, 134.8 µs
 * switching once a run, 28.9 µs with the switch taken away. Starting a
 * thread is 33 µs by itself; the rest is the new stack's pages, cold
 * and faulted in one by one. A worker that has run a segment before
 * keeps both: its thread is started and its stack pages are touched.
 *
 * THE PROTOCOL: `run` takes an idle worker (or makes one), hands it the
 * segment, and parks until the answer or the exception is published;
 * the worker runs it, publishes, and offers itself back to the pool,
 * where it parks. At most `maxIdle` workers wait; one idle longer than
 * `idleMillis` ends, so its stack is given back to the OS — a worker
 * that ran a deep segment holds those pages until then.
 *
 * WHAT A SEGMENT SEES of its thread, since it is no longer a fresh one:
 * - the caller's context class loader, set for the segment (a fresh
 *   thread used to inherit it);
 * - NO inheritable thread-locals: a worker is made with
 *   `inheritThreadLocals = false`, so a reused worker never carries
 *   the values of whichever caller first made it. Plain thread-locals
 *   were never visible past a switch.
 * - the caller waits uninterruptibly — the worker holds the frames of
 *   the caller's own program, there is nothing to abandon — and its
 *   interrupt status is restored when the answer arrives.
 */
private[okay] object StackPool:

  private val maxIdle: Int = Integer.getInteger("okay.cont.idleWorkers", 2)
  private val idleNanos: Long = java.lang.Long.getLong("okay.cont.idleMillis", 30_000L) * 1_000_000L

  private val idle = ConcurrentLinkedDeque[Worker]()

  /** one segment's answer, published by the worker */
  private final class Handoff[R](val waiter: Thread):
    @volatile var done = false
    var out: Either[Throwable, R] | Null = null

  private final class Worker(stack: Long) extends Runnable:
    @volatile private var task: (() => Unit) | Null = null
    private val thread = new Thread(null, this, "okay-cont-stack", stack, false)
    thread.setDaemon(true)
    private var started = false

    /** called by the ONE caller that took this worker */
    def submit(t: () => Unit): Unit =
      task = t
      if !started then
        started = true
        thread.start()
      else LockSupport.unpark(thread)

    def run(): Unit =
      var live = true
      while live do
        val t = task
        if t != null then
          task = null
          t()
          if idle.size < maxIdle then idle.addFirst(this)
          else live = false
        else
          val deadline = System.nanoTime() + idleNanos
          while task == null && System.nanoTime() < deadline do
            LockSupport.parkNanos(this, deadline - System.nanoTime())
          // idle too long: leave, unless a caller took this worker in
          // the meantime (then `remove` loses the race and a task comes)
          if task == null && idle.remove(this) then live = false

  /** run `body` on a worker's stack of `stack` bytes; the answer, or
   * the exception rethrown here */
  def run[R](stack: Long)(body: () => R): R =
    val h = Handoff[R](Thread.currentThread())
    val loader = Thread.currentThread().getContextClassLoader
    val w = idle.pollFirst() match
      case null => Worker(stack)
      case w => w
    w.submit: () =>
      val self = Thread.currentThread()
      self.setContextClassLoader(loader)
      h.out = try Right(body()) catch case e: Throwable => Left(e)
      self.setContextClassLoader(null)
      h.done = true
      LockSupport.unpark(h.waiter)
    var interrupted = false
    while !h.done do
      LockSupport.park(h)
      if Thread.interrupted() then interrupted = true
    if interrupted then Thread.currentThread().interrupt()
    h.out match
      case Right(r) => r
      case Left(e) => throw e
      case null => throw IllegalStateException("okay: a Cont stack switch finished without an answer")
