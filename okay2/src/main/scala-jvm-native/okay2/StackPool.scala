package okay2

import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.locks.LockSupport

/**
 * PARKED WORKERS WITH BIG STACKS, reused across `Cont` stack switches —
 * the Scala 3 core's `StackPool` in Scala 2 (specs/cont-stack.md plan
 * stage D3; JVM and Native share it). A new thread per switch, with its
 * cold stack pages, was the whole of statePara's 4.9x on the count road
 * there; a parked worker's thread is started and its pages touched.
 * `run` takes an idle worker (or makes one), hands it the segment,
 * spins briefly then parks until the answer is published; the worker
 * runs it, publishes, spins for the next task, parks, and leaves after
 * `idleMillis`. No inherited thread-locals; the caller's context class
 * loader set for the segment; the caller's wait uninterruptible with
 * the interrupt restored.
 */
private[okay2] object StackPool {

  private val maxIdle: Int = Integer.getInteger("okay.cont.idleWorkers", 2)
  private val idleNanos: Long = java.lang.Long.getLong("okay.cont.idleMillis", 30000L) * 1000000L
  private val spinNanos: Long = java.lang.Long.getLong("okay.cont.spinMicros", 50L) * 1000L

  private val idle = new ConcurrentLinkedDeque[Worker]()

  private final class Handoff[R](val waiter: Thread) {
    @volatile var done = false
    var out: Either[Throwable, R] = null
  }

  private final class Worker(stack: Long) extends Runnable {
    @volatile private var task: () => Unit = null
    private val thread = new Thread(null, this, "okay-cont-stack", stack, false)
    thread.setDaemon(true)
    private var started = false

    def submit(t: () => Unit): Unit = {
      task = t
      if (!started) { started = true; thread.start() }
      else LockSupport.unpark(thread)
    }

    def run(): Unit = {
      var live = true
      while (live) {
        val t = task
        if (t != null) {
          task = null
          t()
          if (idle.size < maxIdle) idle.addFirst(this) else live = false
        } else {
          val spinUntil = System.nanoTime() + spinNanos
          while (task == null && System.nanoTime() < spinUntil) Thread.onSpinWait()
          val deadline = System.nanoTime() + idleNanos
          while (task == null && System.nanoTime() < deadline) LockSupport.parkNanos(this, deadline - System.nanoTime())
          if (task == null && idle.remove(this)) live = false
        }
      }
    }
  }

  def run[R](stack: Long)(body: () => R): R = {
    val h = new Handoff[R](Thread.currentThread())
    val loader = Thread.currentThread().getContextClassLoader
    val w = idle.pollFirst() match {
      case null => new Worker(stack)
      case w => w
    }
    w.submit { () =>
      val self = Thread.currentThread()
      self.setContextClassLoader(loader)
      h.out = try Right(body()) catch { case e: Throwable => Left(e) }
      self.setContextClassLoader(null)
      h.done = true
      LockSupport.unpark(h.waiter)
    }
    val spinUntil = System.nanoTime() + spinNanos
    while (!h.done && System.nanoTime() < spinUntil) Thread.onSpinWait()
    var interrupted = false
    while (!h.done) {
      LockSupport.park(h)
      if (Thread.interrupted()) interrupted = true
    }
    if (interrupted) Thread.currentThread().interrupt()
    h.out match {
      case Right(r) => r
      case Left(e) => throw e
      case null => throw new IllegalStateException("okay2: a Cont stack switch finished without an answer")
    }
  }
}
