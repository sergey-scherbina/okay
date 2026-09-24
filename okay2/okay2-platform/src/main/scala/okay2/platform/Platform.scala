package okay2.platform

import java.util.concurrent.{CompletableFuture, CompletionException, ExecutionException, ExecutorService, ForkJoinPool, ForkJoinTask}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference, AtomicReferenceArray}
import java.util.concurrent.locks.LockSupport
import okay2._
import okay2.async._

/**
 * The one-shot handoff `block` waits on. Typed on A; `filled` is the
 * release fence, so a reader that sees it true also sees `value`.
 * A plain volatile, not an AtomicBoolean: only ever SET and READ.
 */
private final class Slot[A] {
  var value: A = null.asInstanceOf[A]
  @volatile var filled = false
  @volatile var waiter: Thread = null
}

private final class BoolSlot {
  var value: Boolean = false
  @volatile var filled = false
  @volatile var waiter: Thread = null
}

object Platform {

  /**
   * The JVM can park, and on Loom parking is free: blocking IS
   * asynchrony here. The FAST PATH is the point: if `register`
   * completed the slot before it returned, no park, no permit and no
   * scheduler visit happen at all. The interrupt is read FIRST, in the
   * fast path and at the top of the loop: after a cancel, an answer
   * that arrives anyway must not become this fiber's answer.
   */
  val canBlock: CanBlock = new CanBlock {
    def block[A](register: (A => Unit) => (() => Unit)): A = {
      val slot = new Slot[A]
      val cancel = register { a =>
        slot.value = a
        slot.filled = true
        val t = slot.waiter
        if (t != null) LockSupport.unpark(t)
      }
      if (Thread.interrupted()) { cancel(); throw new InterruptedException() }
      else if (slot.filled) slot.value
      else {
        slot.waiter = Thread.currentThread()
        var out = false
        while (!out) {
          if (Thread.interrupted()) { cancel(); throw new InterruptedException() }
          else if (slot.filled) out = true
          else LockSupport.park(slot)
        }
        slot.value
      }
    }

    def handoff[A](): Handoff[A] = new ParkHandoff[A]

    def await(h: Handoff[_]): Unit =
      if (Thread.interrupted()) throw new InterruptedException()
      else if (!h.filled) h match {
        case p: ParkHandoff[_] =>
          p.waiter = Thread.currentThread()
          var out = false
          while (!out) {
            if (Thread.interrupted()) throw new InterruptedException()
            else if (p.filled) out = true
            else LockSupport.park(p)
          }
        case other => throw new IllegalStateException("a handoff not made by this CanBlock: " + other.getClass.getName)
      }

    def blockAccepted(register: Accepted => (() => Unit)): Boolean = {
      val slot = new BoolSlot
      val cancel = register { a =>
        slot.value = a
        slot.filled = true
        val t = slot.waiter
        if (t != null) LockSupport.unpark(t)
      }
      if (Thread.interrupted()) { cancel(); throw new InterruptedException() }
      else if (slot.filled) slot.value
      else {
        slot.waiter = Thread.currentThread()
        var out = false
        while (!out) {
          if (Thread.interrupted()) { cancel(); throw new InterruptedException() }
          else if (slot.filled) out = true
          else LockSupport.park(slot)
        }
        slot.value
      }
    }
  }

  /** the JVM handoff: the waiter is a parked (virtual) thread */
  private final class ParkHandoff[A] extends Handoff[A] {
    @volatile var waiter: Thread = null
    protected def signal(): Unit = {
      val t = waiter
      if (t != null) LockSupport.unpark(t)
    }
  }

  /** ONE scheduled executor thread holds every pending delay as a
   * small task; the callback runs on a fresh (virtual) thread only
   * when the delay FIRES */
  private[platform] lazy val timerWheel: java.util.concurrent.ScheduledExecutorService =
    java.util.concurrent.Executors.newSingleThreadScheduledExecutor { r =>
      val t = new Thread(r, "okay-timer")
      t.setDaemon(true)
      t
    }

  val timer: Timer = new Timer {
    def after(millis: Long)(k: () => Unit): () => Unit = {
      val fire: Runnable = () => Threads.spawn("okay-timer-fire")(() => k())
      val f = timerWheel.schedule(fire, millis, java.util.concurrent.TimeUnit.MILLISECONDS)
      () => { f.cancel(false); () }
    }
  }

  /** the default scheduler, with `-Dokay.scheduler` as an override */
  lazy val scheduler: Scheduler =
    scala.util.Try(Option(System.getProperty("okay.scheduler"))).toOption.flatten match {
      case Some("own") => Schedulers.own.build
      case Some("adaptive") => Schedulers.adaptive.build
      case Some("drive") => Schedulers.drive()
      case Some("threads") => Schedulers.threads
      case Some("loom") if Schedulers.hasVirtualThreads => Schedulers.loom
      case _ => Schedulers.auto
    }

  val net: Net = new Net {
    def connect(host: String, port: Int): NetConn ! Async = Async {
      val s = new java.net.Socket(host, port)
      s.setTcpNoDelay(true)
      new SocketConn(s)
    }
  }
}

/** fire-and-forget daemon threads: virtual where this JVM has them,
 * an ordinary daemon `Thread` otherwise */
object Threads {
  def spawn(name: String)(body: () => Unit): Unit = { val _ = spawnThread(name)(body) }

  def spawnThread(name: String)(body: () => Unit): Thread =
    if (Schedulers.hasVirtualThreads) Thread.ofVirtual().name(name).start(() => body())
    else {
      val t = new Thread(() => body(), name)
      t.setDaemon(true)
      t.start()
      t
    }
}

/**
 * The JVM schedulers. The default is Loom — one virtual thread per
 * fiber, which is what makes blocking free. For a JVM without Loom,
 * `forkJoin` runs fibers on a pool, `drive` walks the tree on pool
 * threads with no thread per fiber, `own`/`adaptive` are the
 * owned-worker scheduler with Chase-Lev deques, `threads` pays one
 * platform thread per fiber.
 */
object Schedulers {

  /** whether THIS JVM has virtual threads (JDK 21+), checked once */
  val hasVirtualThreads: Boolean = Runtime.version().feature() >= 21

  /** the right default for THIS JVM */
  def auto: Scheduler = if (hasVirtualThreads) loom else platform

  /** the pick for a JVM WITHOUT Loom: `own` with the stuck-check on and quick */
  def platform: Running = own.watched(scala.concurrent.duration.Duration(5, "ms")).build

  /** a scheduler that owns threads, so it can be stopped */
  trait Running extends Scheduler with AutoCloseable {
    def id: Int
  }

  private val ownedCount = new AtomicInteger()

  private def unwrap(e: Throwable): Throwable = e match {
    case e: CompletionException if e.getCause != null => e.getCause
    case e: ExecutionException if e.getCause != null => e.getCause
    case e => e
  }

  private def fiberOf[A](f: CompletableFuture[A], interrupt: () => Unit): Fiber[A] = new Fiber[A] {
    def onComplete(k: Either[Throwable, A] => Unit): Unit = {
      val _ = f.whenComplete((v, e) => k(if (e == null) Right(v) else Left(unwrap(e))))
    }
    def cancel(): Unit = interrupt()
  }

  /** one Loom virtual thread per fiber: blocking parks, for free */
  val loom: Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val f = new CompletableFuture[A]()
      val t = Thread.startVirtualThread { () =>
        try { val _ = f.complete(Effects.runFree(prog())) }
        catch { case e: Throwable => val _ = f.completeExceptionally(e) }
      }
      fiberOf(f, () => t.interrupt())
    }
  }

  /** a pool: cheap fibers, but a parked fiber holds a pool thread */
  def forkJoin(pool: ExecutorService = ForkJoinPool.commonPool()): Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val f = new CompletableFuture[A]()
      val task: Runnable = () =>
        try { val _ = f.complete(Effects.runFree(prog())) }
        catch { case e: Throwable => val _ = f.completeExceptionally(e) }
      val fut = pool.submit(task)
      fiberOf(f, () => { val _ = fut.cancel(true) })
    }
  }

  /** fibers as continuations on a pool — the JS shape on the JVM: no
   * thread per fiber, the tree walked by `Async.Drive` on whichever
   * pool thread picks it up; the fiber, the pool task and the promise
   * are ONE object */
  def drive(pool: ForkJoinPool = ForkJoinPool.commonPool()): Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val t = new DriveTask[A](prog)
      pool.execute(t)
      t
    }
  }

  /** the owned-worker scheduler, chosen and tuned like a queue:
   * `Schedulers.own.workers(4).build`, `.forShortTasks`, `.forLongTasks`,
   * `Schedulers.adaptive.build` (a worker more when a fiber blocks) */
  val own: Own = Own()

  /** `own` with the stuck-check on */
  val adaptive: Own = Own().watched()

  final case class Own(private val count: Int = Runtime.getRuntime.availableProcessors(),
                       private val spinRounds: Int = 64,
                       private val wakeDeeperThan: Int = 64,
                       private val helpAfterNanos: Long = 50000L,
                       private val spreadAboveNanos: Long = 1000L,
                       private val stuckAfterMillis: Long = 0L,
                       private val overflowWorkers: Int = 0) {
    def workers(n: Int): Own = copy(count = if (n < 1) 1 else n)
    def spinning(rounds: Int): Own = copy(spinRounds = if (rounds < 0) 0 else rounds)
    def wakeAbove(tasks: Int): Own = copy(wakeDeeperThan = if (tasks < 0) 0 else tasks)
    def helpAfter(nanos: Long): Own = copy(helpAfterNanos = if (nanos < 0) 0 else nanos)
    def spreadAbove(nanos: Long): Own = copy(spreadAboveNanos = if (nanos < 0) 0 else nanos)
    /** never spread: every fiber runs where it was forked */
    def forShortTasks: Own = copy(spreadAboveNanos = Long.MaxValue)
    /** spread as soon as there is anything to spread */
    def forLongTasks: Own = copy(helpAfterNanos = 0L, spreadAboveNanos = 0L)
    /** start one more worker (up to `overflow`) when work is pending
     * and nothing has completed for `after` */
    def watched(after: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(100, "ms"),
                overflow: Int = -1): Own =
      copy(stuckAfterMillis = math.max(1L, after.toMillis), overflowWorkers = if (overflow < 0) count else overflow)

    def build: Running = new Owned(count, spinRounds, wakeDeeperThan, helpAfterNanos, spreadAboveNanos, stuckAfterMillis, overflowWorkers)
  }

  private[platform] final class Owned(n: Int, spin: Int, wakeAbove: Int,
                                      helpAfterNanos: Long, spreadAboveNanos: Long,
                                      stuckAfterMillis: Long, overflow: Int) extends Running {
    val id: Int = ownedCount.incrementAndGet()
    @volatile private var stopped = false
    private var watchdog: java.util.concurrent.ScheduledFuture[_] = null

    def close(): Unit = {
      stopped = true
      val wd = watchdog
      if (wd != null) { val _ = wd.cancel(false) }
      var i = 0
      while (i < workers.length) { LockSupport.unpark(workers(i).thread); i += 1 }
    }

    private val submissions = new java.util.concurrent.ConcurrentLinkedQueue[DriveTask[_]]()
    private val submissionsSize = new AtomicInteger()
    private val awake = new AtomicInteger(0)
    private val workers: Array[Worker] = Array.tabulate(n + overflow)(i => new Worker(i))
    private val live = new AtomicInteger(n)
    private val completed = new AtomicLong()
    @volatile private var lastCompleted = -1L
    private val current = new ThreadLocal[Worker]()

    private final class Worker(val id: Int) extends Runnable {
      val deque = new Deque(256)
      @volatile var parked = false
      var ran = 0L
      var stolen = 0L
      val thread: Thread = { val t = new Thread(this, s"okay-own-${Owned.this.id}-$id"); t.setDaemon(true); t }

      def size: Int = deque.size

      def pushLocal(t: DriveTask[_]): Unit = deque.push(t)

      private def take(): DriveTask[_] = {
        val t = deque.pop()
        if (t != null) t else fromSubmissions()
      }

      def taken(): DriveTask[_] = deque.steal()

      private def steal(): DriveTask[_] = {
        val alive = live.get
        var i = 1
        while (i < alive) {
          val t = workers((id + i) % alive).taken()
          if (t != null) return t
          i += 1
        }
        fromSubmissions()
      }

      def run(): Unit = {
        current.set(this)
        val _ = awake.incrementAndGet()
        var spins = 0
        var streakStart = 0L
        var windowStart = 0L
        var windowRan = 0
        while (!stopped) {
          var t = take()
          if (t == null) { t = steal(); if (t != null) stolen += 1L }
          if (t != null) {
            spins = 0
            if (streakStart == 0L) { streakStart = System.nanoTime(); windowStart = streakStart; windowRan = 0 }
            val _ = t.exec()
            ran += 1L
            windowRan += 1
            if (stuckAfterMillis > 0L) { val _ = completed.incrementAndGet() }
            // THE HELPER RULE: wake one sleeper only when this worker has
            // been busy longer than helpAfter, still has work, AND its
            // tasks average more than spreadAbove
            if ((windowRan & 15) == 0 && size > 0) {
              val now = System.nanoTime()
              if (now - streakStart > helpAfterNanos && (now - windowStart) / windowRan > spreadAboveNanos) {
                val _ = activateNext()
              }
              windowStart = now
              windowRan = 0
            }
          } else if (spins < spin) {
            streakStart = 0L
            windowRan = 0
            spins += 1
            Thread.onSpinWait()
          } else {
            parked = true
            val _ = awake.decrementAndGet()
            if (size == 0 && submissions.isEmpty && !stopped) LockSupport.park(this)
            parked = false
            val _ = awake.incrementAndGet()
            spins = 0
          }
        }
      }
    }

    private def startWorker(i: Int): Unit = workers(i).thread.start()
    locally {
      var w0 = 0
      while (w0 < n) { startWorker(w0); w0 += 1 }
    }

    /** THE STUCK-CHECK: every `stuckAfterMillis`, if work is pending
     * and nothing has completed since the last look, wake a parked
     * worker, and only when none is parked start one more */
    if (stuckAfterMillis > 0L && overflow > 0) {
      val check: Runnable = () => {
        val pending = submissionsSize.get > 0 || {
          var any = false; var i = 0; val alive = live.get
          while (i < alive) { if (workers(i).size > 0) any = true; i += 1 }
          any
        }
        val done = completed.get
        if (pending && done == lastCompleted && !activateNext()) {
          val next = live.get
          if (next < n + overflow && live.compareAndSet(next, next + 1)) startWorker(next)
        }
        lastCompleted = done
      }
      watchdog = Platform.timerWheel.scheduleWithFixedDelay(check, stuckAfterMillis, stuckAfterMillis,
        java.util.concurrent.TimeUnit.MILLISECONDS)
    }

    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val t = new DriveTask[A](prog)
      val mine = current.get
      if (mine != null) mine.pushLocal(t)
      else {
        val _ = submissionsSize.incrementAndGet()
        val _ = submissions.offer(t)
        if (awake.get == 0 || submissionsSize.get > wakeAbove) { val _ = activateNext() }
      }
      t
    }

    private def fromSubmissions(): DriveTask[_] = {
      val t = submissions.poll()
      if (t != null) { val _ = submissionsSize.decrementAndGet() }
      t
    }

    /** wake ONE sleeping worker, wherever it sits; false when nobody was parked */
    private def activateNext(): Boolean = {
      val alive = live.get
      var i = 0
      while (i < alive) {
        val w = workers(i)
        if (w.parked) { LockSupport.unpark(w.thread); return true }
        i += 1
      }
      false
    }
  }

  /** a Chase-Lev deque: the owner pushes and pops at `bottom` with
   * plain reads and one volatile store; a thief takes at `top` with a
   * CAS. The slot is NOT cleared on steal, on purpose: a thief may be
   * reading through an array the owner has replaced in `push`'s grow,
   * and a null written there is a task lost */
  private[platform] final class Deque(initial: Int) {
    private val top = new AtomicLong(0L)
    @volatile private var bottom: Long = 0L
    @volatile private var buf = new AtomicReferenceArray[DriveTask[_]](initial)

    def size: Int = {
      val n = bottom - top.get
      if (n < 0) 0 else n.toInt
    }

    private def index(i: Long, len: Int): Int = (i & (len - 1)).toInt

    /** owner only */
    def push(t: DriveTask[_]): Unit = {
      val b = bottom
      val tp = top.get
      var a = buf
      if (b - tp >= a.length() - 1) {
        val bigger = new AtomicReferenceArray[DriveTask[_]](a.length() * 2)
        var i = tp
        while (i < b) { bigger.set(index(i, bigger.length()), a.get(index(i, a.length()))); i += 1 }
        buf = bigger
        a = bigger
      }
      a.set(index(b, a.length()), t)
      bottom = b + 1
    }

    /** owner only */
    def pop(): DriveTask[_] = {
      val a = buf
      val b = bottom - 1
      bottom = b
      val tp = top.get
      if (tp > b) { bottom = tp; null }
      else {
        val i = index(b, a.length())
        val t = a.get(i)
        if (tp < b) { a.set(i, null); t }
        else {
          val won = top.compareAndSet(tp, tp + 1)
          bottom = tp + 1
          if (won) { a.set(i, null); t } else null
        }
      }
    }

    /** any thread */
    def steal(): DriveTask[_] = {
      val tp = top.get
      val b = bottom
      if (tp >= b) null
      else {
        val a = buf
        val i = index(tp, a.length())
        val t = a.get(i)
        if (top.compareAndSet(tp, tp + 1)) t else null
      }
    }
  }

  /** listeners of a running DriveTask, a stack */
  private final class Waiters[A](val k: Either[Throwable, A] => Unit, val next: Waiters[A])

  /** one object: the pool task that walks the program, the cell its
   * answer lands in, and the Fiber a caller holds */
  private[platform] final class DriveTask[A](prog: () => A ! Async)
      extends ForkJoinTask[Unit] with Async.Drive[A] with Fiber[A] {
    private val cell = new AtomicReference[AnyRef](null)

    def exec(): Boolean = {
      try apply(prog())
      catch { case e: Throwable => fail(e) }
      true
    }
    def getRawResult(): Unit = ()
    def setRawResult(v: Unit): Unit = ()

    protected def succeed(a: A): Unit = done(Right(a))
    protected def fail(e: Throwable): Unit = done(Left(e))

    @scala.annotation.tailrec private def done(r: Either[Throwable, A]): Unit =
      cell.get match {
        case _: Either[_, _] => ()
        case cur => if (cell.compareAndSet(cur, r)) fire(cur, r) else done(r)
      }

    @scala.annotation.tailrec private def fire(w: AnyRef, r: Either[Throwable, A]): Unit =
      w match {
        case w: Waiters[A @unchecked] => w.k(r); fire(w.next, r)
        case _ => ()
      }

    @scala.annotation.tailrec def onComplete(k: Either[Throwable, A] => Unit): Unit =
      cell.get match {
        case r: Either[Throwable, A] @unchecked => k(r)
        case w: Waiters[A @unchecked] => if (!cell.compareAndSet(w, new Waiters[A](k, w))) onComplete(k)
        case _ => if (!cell.compareAndSet(null, new Waiters[A](k, null))) onComplete(k)
      }

    /** cancel ANSWERS the fiber: a join on a cancelled fiber returns */
    override def cancel(): Unit = {
      super[Drive].cancel()
      done(Left(new java.util.concurrent.CancellationException("fiber cancelled")))
    }
  }

  /** one honest platform thread per fiber */
  val threads: Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val f = new CompletableFuture[A]()
      val r: Runnable = () =>
        try { val _ = f.complete(Effects.runFree(prog())) }
        catch { case e: Throwable => val _ = f.completeExceptionally(e) }
      val t = new Thread(r)
      t.start()
      fiberOf(f, () => t.interrupt())
    }
  }
}

/**
 * Blocking code as an `Async` operation whose CANCEL INTERRUPTS it:
 * the action gets a thread of its own, and the operation's canceller
 * interrupts exactly that thread, whatever the scheduler.
 */
object Interruptible {
  private val threads = java.util.concurrent.Executors.newCachedThreadPool { r =>
    val t = new Thread(r, "okay-lifted")
    t.setDaemon(true)
    t
  }

  /** `action` on a thread of its own; a cancel interrupts that thread */
  def await[A](action: () => A): Async.Op[A] = Async.Await[A] { k =>
    val running = threads.submit((() => k(try Right(action()) catch { case e: Throwable => Left(e) })): Runnable)
    () => { val _ = running.cancel(true) }
  }
}

/** `ScopedValue`'s shape, over a `ThreadLocal` with no public `set`:
 * `where` binds `value` for `body`'s extent only and restores whatever
 * was bound before, however `body` exits */
final class Scoped[A] private (default: () => A) {
  private val local: ThreadLocal[A] = new ThreadLocal[A] {
    override def initialValue(): A = default()
  }

  def current: A = local.get()

  def where[B](value: A)(body: => B): B = {
    val prior = local.get()
    local.set(value)
    try body finally local.set(prior)
  }
}

object Scoped {
  def apply[A](default: => A): Scoped[A] = new Scoped(() => default)
}

/** the byte-stream seam: a blocking socket behind Async.Run */
trait NetConn {
  /** exactly n bytes, or a throw naming the shortfall */
  def readFully(n: Int): Array[Byte] ! Async
  def write(bytes: Array[Byte]): Unit ! Async
  def close(): Unit
}

/** truncated mid-read: the far end closed inside a frame */
final case class NetEof(wanted: Int, got: Int)
  extends RuntimeException(s"connection ended mid-read: wanted $wanted bytes, got $got")

trait Net { def connect(host: String, port: Int): NetConn ! Async }

object Net {
  def connect(host: String, port: Int)(implicit n: Net): NetConn ! Async = n.connect(host, port)
}

private final class SocketConn(sock: java.net.Socket) extends NetConn {
  private val in = new java.io.BufferedInputStream(sock.getInputStream)
  private val out = new java.io.BufferedOutputStream(sock.getOutputStream)

  def readFully(n: Int): Array[Byte] ! Async = Async {
    val buf = new Array[Byte](n)
    var at = 0
    while (at < n) {
      val r = in.read(buf, at, n - at)
      if (r < 0) throw NetEof(n, at)
      at += r
    }
    buf
  }

  def write(bytes: Array[Byte]): Unit ! Async = Async {
    out.write(bytes)
    out.flush()
  }

  def close(): Unit = sock.close()
}
