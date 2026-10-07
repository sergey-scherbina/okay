package okay


import okay.freer.*


import okay.std.*
import java.util.concurrent.{CompletableFuture, CompletionException, ExecutionException, ExecutorService, ForkJoinPool}

/**
 * The one-shot handoff `block` waits on. Typed on A, so the value
 * needs no cast on the way out; `filled` is the release fence, so a
 * reader that sees it true also sees `value` written.
 *
 * It replaces a `CompletableFuture` per call. The future was correct
 * but it is a general-purpose object: a node allocation, a Treiber
 * stack of signallers and a spin before parking, all to carry one
 * value to one waiter exactly once. The channel profile put
 * `CanBlock.block` third among leaf frames, level with the ring's own
 * CAS -- on a path where the callback usually fires SYNCHRONOUSLY,
 * inside `register`, because the element was already buffered and
 * there was never anything to wait for.
 *
 * `filled` is a plain volatile rather than an AtomicBoolean, because
 * it is only ever SET and READ — never compare-and-set — and a
 * volatile write/read has exactly the memory semantics
 * AtomicBoolean.set/get provide. The atomic was a SECOND allocation on
 * every handshake. Counted on the elementwise channel lane
 * (channel-elementwise-wakeups, N=4000): `block` runs once per element
 * and takes the fast path 4022 times in 4064, parking 42 times — so
 * the pair of objects was allocated 4000 times per operation to carry
 * a value that was already there.
 */
private final class Slot[A]:
  var value: A = scala.compiletime.uninitialized
  @volatile var filled = false
  @volatile var waiter: Thread | Null = null

/** the same one-shot handoff with the value as a primitive: a
 * `Slot[Boolean]` would box on the way in and out */
private final class BoolSlot:
  var value: Boolean = false
  @volatile var filled = false
  @volatile var waiter: Thread | Null = null

/**
 * The JVM can park, and on Loom parking is free: blocking IS
 * asynchrony here. CanBlock parks the current (ideally virtual)
 * thread until the callback fires; interruption cancels the wait.
 *
 * The FAST PATH is the point: if `register` completed the slot before
 * it returned, the value is already there and no park, no permit and
 * no scheduler visit happen at all.
 */
// TRIED AND REFUTED (adversarial-lanes, 2026-09-06): a stackless
// InterruptedException on the cancel path, on the theory that
// fillInStackTrace on a thousand parked virtual threads was the cost
// of cancelling them. A/B against the plain exception: 1082 -> 1087us
// per 1000, 1.01x, inside the bars. The cost of a cancel is the
// interrupt reaching the park and the join, not the trace.
/**
 * A worker thread that wants to be TOLD when a fiber on it blocks
 * (own-managed-blocking, specs/schedulers.md "Managed blocking"): the
 * doors below test `Thread.currentThread()` for this class on their SLOW
 * path only, right before the first park, so a fiber that never blocks
 * pays nothing and a virtual or foreign thread parks as before. The
 * `ForkJoinPool.ManagedBlocker` protocol, ours: `blocking()` before the
 * park, `unblocked()` after it, on the owner thread, in pairs.
 */
private[okay] final class ManagedWorker(val hooks: ManagedWorker.Hooks, name: String) extends Thread(hooks, name):
  /** the drive whose slice runs on this worker, if any — `DriveTask.running`
   * for our own threads, as a plain field: only this thread reads or
   * writes it, and a ThreadLocal's lookup on every slice of every fiber
   * was a third of what the slice hooks cost (spawnjoin-rise-bisect) */
  var drive: Schedulers.DriveTask[?] | Null = null
  // here rather than at the construction site: a worker makes its thread
  // in its own constructor, and the init checker (rightly) flags handing
  // a thread that holds the half-built worker to an external method there
  setDaemon(true)

private[okay] object ManagedWorker:
  /** what the worker's own loop implements: its `run`, and the two calls */
  trait Hooks extends Runnable:
    def blocking(): Unit
    def unblocked(): Unit

/** the door's side of the protocol: the current thread, told it is about
 * to block, if it is a worker that asked to be told */
private def enterBlocking(): ManagedWorker | Null =
  Thread.currentThread() match
    case w: ManagedWorker => w.hooks.blocking(); w
    case _ => null

given CanBlock = new:
  def block[A](register: (A => Unit) => (() => Unit)): A =
    val slot = Slot[A]()
    val cancel = register: a =>
      slot.value = a
      slot.filled = true          // release: value is written first
      val t = slot.waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)
    // the interrupt is read FIRST here TOO, not only in the loop
    // below (scheduler-cancel-wins). A cancel and an answer that both
    // land while the registration is still running are seen by a
    // fiber that has not looked at its interrupt yet, and the fast
    // path handed it the answer — the law's own race, forced and
    // reproduced by `cancel wins the race it is in`.
    if Thread.interrupted() then
      cancel()
      throw InterruptedException()
    else if slot.filled then slot.value   // never waited
    else
      // publish who to wake BEFORE re-reading the flag: a completer
      // that misses the waiter is one whose flag we are about to see
      slot.waiter = Thread.currentThread()
      val managed = enterBlocking()
      try
        var out = false
        while !out do
          // the interrupt is read FIRST: after a cancel, an answer that
          // arrives anyway must not become this fiber's answer. Reading
          // `filled` first let it (found by the scheduler law, 2026-09-07)
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
          else if slot.filled then out = true
          else java.util.concurrent.locks.LockSupport.park(slot)
      finally if managed != null then managed.hooks.unblocked()
      slot.value

  /** the JVM handoff: the waiter is a parked (virtual) thread, woken
   * by `LockSupport.unpark` — the same protocol as `block`'s slot,
   * without the second object */
  private final class ParkHandoff[A] extends Handoff[A]:
    @volatile var waiter: Thread | Null = null
    protected def signal(): Unit =
      val t = waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)

  def handoff[A](): Handoff[A] = ParkHandoff[A]()

  def await(h: Handoff[?]): Unit =
    // the interrupt is read before the FAST path too, as `block`
    // does: a cancel and a fill that both land before this line are
    // otherwise seen by a fiber that has not looked at its interrupt
    // (park-interrupt-order)
    if Thread.interrupted() then throw InterruptedException()
    else if !h.filled then h match
      case p: ParkHandoff[?] =>
        // publish who to wake BEFORE re-reading the flag, as `block` does
        p.waiter = Thread.currentThread()
        val managed = enterBlocking()
        try
          var out = false
          while !out do
            if Thread.interrupted() then throw InterruptedException()
            else if p.filled then out = true
            else java.util.concurrent.locks.LockSupport.park(p)
        finally if managed != null then managed.hooks.unblocked()
      case other =>
        throw IllegalStateException("a handoff not made by this CanBlock: " + other.getClass.getName)

  def blockAccepted(register: Accepted => (() => Unit)): Boolean =
    val slot = BoolSlot()
    val cancel = register: a =>
      slot.value = a
      slot.filled = true
      val t = slot.waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)
    // the same rule as `block`, which this had not been given
    // (park-interrupt-order): the interrupt is read FIRST, in the
    // fast path and at the top of the loop. Reading `filled` first
    // let an acceptance that arrived AFTER a cancel become the
    // answer of a fiber blocked on a send.
    if Thread.interrupted() then
      cancel()
      throw InterruptedException()
    else if slot.filled then slot.value
    else
      slot.waiter = Thread.currentThread()
      val managed = enterBlocking()
      try
        var out = false
        while !out do
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
          else if slot.filled then out = true
          else java.util.concurrent.locks.LockSupport.park(slot)
      finally if managed != null then managed.hooks.unblocked()
      slot.value

/** the timer: a virtual thread sleeps for the duration; cancelling
 * interrupts it out of the sleep */
/**
 * The timer: ONE scheduled executor thread holds every pending delay
 * as a small task, and the callback runs on a virtual thread only
 * when the delay FIRES. The previous shape started a virtual thread
 * per timer and slept it: a stack chunk per arm -- 5.5 KB per `ask`,
 * whose timer is armed on every call and cancelled by the reply on
 * nearly all of them (docs/benchmarks.md section 17) -- and about a
 * millisecond plus 40% over the asked sleep. A cancelled timer here
 * allocates the task and nothing else; a fired one pays the virtual
 * thread it always paid, so a callback that blocks still blocks
 * nobody but itself.
 */
private lazy val timerWheel: java.util.concurrent.ScheduledExecutorService =
  java.util.concurrent.Executors.newSingleThreadScheduledExecutor: r =>
    val t = Thread(r, "okay-timer")
    t.setDaemon(true)
    t

given Timer = new:
  def after(millis: Long)(k: () => Unit): () => Unit =
    // jdk-adaptive-scheduler, 2026-09-19: a virtual thread where this
    // JVM has one, an ordinary daemon thread where it does not --
    // Schedulers.hasVirtualThreads, checked once. Without this, a
    // fired timer on a pre-21 JDK is a NoSuchMethodError the first
    // time anything sleeps or times out, not a JDK version this file
    // fails to LOAD on (it loads fine either way -- constant-pool
    // resolution of Thread.startVirtualThread is lazy, per call site,
    // not per class; only calling it unconditionally is the problem).
    val fire: Runnable =
      if Schedulers.hasVirtualThreads then
        () => { Thread.startVirtualThread(() => k()); () }
      else
        () =>
          val t = Thread(() => k())
          t.setDaemon(true)
          t.start()
    val f = timerWheel.schedule(fire, millis, java.util.concurrent.TimeUnit.MILLISECONDS)
    () => { f.cancel(false); () }

/**
 * The JVM schedulers. The default given is `adaptive` where the JVM has
 * Loom (`auto`, since 2026-09-28): owned workers for speed, watched so a
 * fiber that blocks costs latency and not the program, and waiting work
 * past its bound spilled onto virtual threads. `Schedulers.loom` — one
 * virtual thread per fiber, blocking free — is a `given` away. For a JVM
 * without Loom, Schedulers.forkJoin runs fibers on a pool (do not park
 * long there), and Schedulers.threads pays one honest platform thread per
 * fiber.
 */
object Schedulers {

  /** whether THIS JVM has virtual threads (Loom, JEP 444, JDK 21+),
   * checked once. `Runtime.version()` is JDK 9+, safe to call on
   * anything this library runs on at all (jdk-adaptive-scheduler,
   * 2026-09-19). */
  val hasVirtualThreads: Boolean = Runtime.version().feature() >= 21

  /** the right default for THIS JVM, with no property and no `given`
   * needed to get it: `adaptive` where virtual threads exist, `platform`
   * where they don't. `given Scheduler` below is exactly this, plus
   * `-Dokay.scheduler` as an override; call `auto` directly from code
   * that wants the pick without going through either.
   *
   * ADAPTIVE, NOT LOOM, SINCE 2026-09-28 (scheduler-default-flip). The
   * re-run of the table left no performance reason for Loom: fork/join
   * from outside 0.65 of Loom's time, cancel 0.68, `parallel8` 0.67,
   * spawn/join 35x, Wrocław 1.00, blocking TCP 1.33x Loom's throughput
   * (specs/schedulers.md, "The default, re-run"). What kept Loom was a
   * livelock under `adaptive` — a channel sender spinning behind another
   * sender's waiter — fixed the same day (adaptive-merge-early-stop-
   * livelock, abrupt-sender-head-recheck), and the whole JVM family was
   * run under the new default before this line changed. On a Loom JVM
   * `adaptive` also spills waiting work past its bound onto virtual
   * threads, which is what makes it safe as a default; on 17-20 there
   * is no spill, so `platform` stays the pick there.
   *
   * ONE instance, built on first use and shared: `auto` is called by
   * the `given` and may be called by code, and a scheduler per call
   * would be a pool of threads per call. Its workers are daemon
   * threads, and nothing closes it. */
  def auto: Scheduler = if hasVirtualThreads then sharedAdaptive else platform

  /** the `adaptive` scheduler `auto` hands out where Loom exists */
  private lazy val sharedAdaptive: Running = adaptive.build

  /** the pick for a JVM WITHOUT Loom: `own` — the fastest
   * platform-thread scheduler measured here, see its doc below — with
   * the stuck-check on and quick. A default is handed to programs
   * that were written for Loom, where a fiber may sit in a raw
   * blocking call for its whole life (okay-http's `Nio` parks its
   * accept loop in `accept()`), and plain `own` is honest about not
   * surviving that: a worker inside a task counts as awake, so an
   * outside fork wakes nobody and a child the worker forked before
   * it blocked sits on its deque unsignalled. Read from a thread
   * dump on JDK 17 (own-lost-wakeup, 2026-09-20): fourteen workers,
   * one in `accept()`, thirteen parked, the test's client fiber in
   * the submission queue for good. Watched every 5 ms, that stall
   * costs a tick, not the program; the timer is one task on the
   * wheel and the check is a handful of volatile reads. */
  def platform: Running = own.watched(scala.concurrent.duration.Duration(5, "ms")).build

  /** a scheduler that owns threads, so it can be stopped. `close()`
   * lets the workers finish what they hold and then exit; a fiber
   * forked after it is a fiber nobody will run, so close a scheduler
   * only when the work that used it is done. */
  trait Running extends Scheduler with AutoCloseable:
    /** the number in this scheduler's thread names, `okay-own-<id>-<worker>` */
    def id: Int

  private val ownedCount = java.util.concurrent.atomic.AtomicInteger()

  private def unwrap(e: Throwable): Throwable = e match
    case e: CompletionException if e.getCause != null => e.getCause
    case e: ExecutionException if e.getCause != null => e.getCause
    case e => e

  private def fiberOf[A](f: CompletableFuture[A], interrupt: () => Unit): Fiber[A] = new:
    def onComplete(k: Either[Throwable, A] => Unit): Unit =
      f.whenComplete((v, e) => k(if e == null then Right(v) else Left(unwrap(e))))
      ()
    def cancel(): Unit = interrupt()
    def answered: Boolean = f.isDone
    // TRIED AND REFUTED (close-the-gaps, 2026-09-06): overriding
    // joinEither to park on `f.get()` directly instead of through
    // onComplete -> Slot -> park. Alternating A/B, three rounds,
    // medians: 19.6 -> 22.3us per 100 fork/joins, WORSE in every
    // round. CompletableFuture.get spins before it parks; the Slot
    // path parks at once, and on a virtual thread that is cheaper.

  /** one Loom virtual thread per fiber: blocking parks, for free */
  val loom: Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val f = CompletableFuture[A]()
      val t = Thread.startVirtualThread: () =>
        try { val _ = f.complete(Async.runFiber(prog())) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
      fiberOf(f, () => t.interrupt())

  /** a pool (the common fork-join by default): cheap fibers, but a
   * parked fiber holds a pool thread — prefer loom for blocking work */
  def forkJoin(pool: ExecutorService = ForkJoinPool.commonPool()): Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val f = CompletableFuture[A]()
      // an explicit Runnable: with a `() => Unit` lambda the two
      // `submit` overloads (Runnable and Callable[T]) both match
      // THE RUNNING THREAD, tracked under a monitor (supervised-waits-on-
      // failure, 2026-09-28). `Future.cancel(true)` does not interrupt a
      // running ForkJoinTask at all (`ForkJoinTask.cancel` documents that
      // the flag "has no effect"), so on the common pool a cancelled child
      // sleeping 3 s slept its 3 s: TestSupervised's ten-child scope had
      // only ever LEFT its siblings, not cancelled them, and a scope that
      // now waits for their answers read 3015 ms. The interrupt is ours,
      // delivered under `lock` so it lands in THIS task — the `finally`
      // takes the same lock before the worker moves on to an unrelated
      // task, the stale-interrupt hazard Native's pool names.
      val lock = new Object
      val runner = java.util.concurrent.atomic.AtomicReference[Thread | Null](null)
      val task: Runnable = () =>
        runner.set(Thread.currentThread())
        try { val _ = f.complete(Async.runFiber(prog())) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
        finally lock.synchronized { runner.set(null) }
      val fut = pool.submit(task)
      // cancel ANSWERS the fiber (specs/cross-platform-async.md): a task
      // cancelled while still QUEUED never runs, so nothing else would
      // complete `f` — a join waited forever and a scope waiting for the
      // child hung. A running task is interrupted and completes `f`
      // itself; one that starts in the window after this read finds `f`
      // answered and its own completion ignored (first wins).
      fiberOf(f, () => {
        val _ = fut.cancel(false)
        lock.synchronized:
          runner.get() match
            case null => val _ = f.completeExceptionally(java.util.concurrent.CancellationException("fiber cancelled"))
            case t => t.interrupt()
      })

  /** fibers as continuations on a pool — the JS shape on the JVM: no
   * thread per fiber, the program's tree walked by `Async.Drive` on
   * whichever pool thread picks it up; a parked Await costs its
   * callback and nothing else. The fiber, the pool task and the
   * promise are ONE object (`DriveTask`), the shape kyo's IOTask
   * has. Blocking joins from inside a fiber still hold the pool
   * thread, as with `forkJoin`. */
  def drive(pool: ForkJoinPool = ForkJoinPool.commonPool()): Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val t = DriveTask[A](prog)
      pool.execute(t)
      t

  /**
   * The owned-worker scheduler, chosen and tuned the way a queue is
   * (`Queues.strong.adaptive.parts(8).each(256).build`):
   *
   * {{{
   * Schedulers.own.build                          // the defaults, which adapt
   * Schedulers.own.workers(4).build               // four threads, not one per core
   * Schedulers.own.forShortTasks.build            // never spread: keep every fiber at home
   * Schedulers.own.forLongTasks.build             // spread at once: a core per fiber if there is one
   * Schedulers.adaptive.build                     // own, plus a worker when a fiber blocks
   * }}}
   *
   * `workers` platform threads, a Chase-Lev deque each; a fiber
   * forked FROM a worker lands on that worker's own end with no CAS
   * and no signal, a fiber forked from outside lands in one shared
   * submission queue that wakes a worker only when nobody is awake to
   * see it. A dry worker steals, spins `spinning` rounds, then parks.
   *
   * THE HELPER RULE is what makes one scheduler right for both shapes
   * of fork/join, and it reads the work rather than being told about
   * it: at every 16th task a worker knows how long it has been busy
   * and how many tasks that took, and it wakes one sleeper only when
   * it is past `helpAfter` AND its tasks average more than
   * `spreadAbove`. Thirty-nanosecond fibers stay home, where waking a
   * core costs more than the work (kyo's shape); microsecond fibers
   * spread over the machine (the pool's shape). Measured, one run:
   * 744 us per 10 000 fork/joins against kyo's 779 at 30 ns a fiber,
   * and 3 327 against kyo's 25 419 at 2.5 us a fiber.
   *
   * A BLOCKING call inside a fiber holds one of the `workers`
   * threads. `Schedulers.own` is for short CPU-bound fibers;
   * `Schedulers.adaptive` adds the worker that makes blocking safe,
   * and `Schedulers.loom` makes it free. `own` is never the default:
   * `auto` picks `platform` (a watched `own`) where there is no Loom.
   */
  val own: Own = Own()

  /** `own` with the stuck-check on: when work is pending and nothing
   * has completed for `stuckAfter`, one more worker is started (up to
   * `overflow`), so a fiber that blocks inside a worker costs
   * latency instead of the program. */
  val adaptive: Own = Own().watched()

  /**
   * The builder. Every knob has a default that is measured, and the
   * two presets name the two ends of the one decision this scheduler
   * makes: whether to spread a burst or keep it at home.
   */
  final case class Own(private val count: Int = Runtime.getRuntime.availableProcessors(),
                       private val spinRounds: Int = 64,
                       private val wakeDeeperThan: Int = 64,
                       private val helpAfterNanos: Long = 50000L,
                       private val spreadAboveNanos: Long = 1000L,
                       private val stuckAfterMillis: Long = 0L,
                       private val overflowWorkers: Int = 0,
                       private val monitorEveryNanos: Long = 100000L) {
    /** how many threads the scheduler owns (default: one per core) */
    def workers(n: Int): Own = copy(count = if n < 1 then 1 else n)
    /** how long a dry worker looks for work before parking */
    def spinning(rounds: Int): Own = copy(spinRounds = if rounds < 0 then 0 else rounds)
    /** how deep the submission queue may get before a sleeper is woken */
    def wakeAbove(tasks: Int): Own = copy(wakeDeeperThan = if tasks < 0 then 0 else tasks)
    /** how long a worker may be busy with work pending before it asks
     * for help (only then is the average consulted) */
    def helpAfter(nanos: Long): Own = copy(helpAfterNanos = if nanos < 0 then 0 else nanos)
    def helpAfter(d: scala.concurrent.duration.FiniteDuration): Own = helpAfter(d.toNanos)
    /** the task cost above which spreading pays; below it, waking a
     * core costs more than the work */
    def spreadAbove(nanos: Long): Own = copy(spreadAboveNanos = if nanos < 0 then 0 else nanos)
    def spreadAbove(d: scala.concurrent.duration.FiniteDuration): Own = spreadAbove(d.toNanos)

    /** never spread: every fiber runs where it was forked — the helper
     * rule off AND the monitor off. For bursts of very short fibers
     * over shared state, where one core beats many (five-way workers at
     * work 0: 5 400 ops/s kept home against 4 000 spread), and for a
     * program that wants its own ordering back */
    def forShortTasks: Own = copy(spreadAboveNanos = Long.MaxValue, monitorEveryNanos = 0L)
    /** spread as soon as there is anything to spread: a core per
     * fiber where the machine has one */
    def forLongTasks: Own = copy(helpAfterNanos = 0L, spreadAboveNanos = 0L)

    /** start one more worker (up to `overflow`, default: as many as
     * there are workers) when work is pending and nothing has
     * completed for `after` — what makes a blocking fiber survivable */
    def watched(after: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(100, "ms"),
                overflow: Int = -1): Own =
      copy(stuckAfterMillis = math.max(1L, after.toMillis), overflowWorkers = if overflow < 0 then count else overflow)

    /** how often the monitor looks for a worker stuck in one task with
     * work waiting behind it (specs/schedulers.md, "The monitor");
     * zero turns the monitor off */
    def monitorEvery(nanos: Long): Own = copy(monitorEveryNanos = if nanos < 0 then 0 else nanos)
    def monitorEvery(d: scala.concurrent.duration.FiniteDuration): Own = monitorEvery(d.toNanos)
    /** no monitor: a fiber forked inside a worker is seen only by that
     * worker's helper rule (the behaviour before own-scheduler-monitor) */
    def unmonitored: Own = monitorEvery(0L)

    def build: Running =
      Owned(count, spinRounds, wakeDeeperThan, helpAfterNanos, spreadAboveNanos, stuckAfterMillis, overflowWorkers,
        monitorEveryNanos)
  }

  private[okay] final class Owned(n: Int, spin: Int, wakeAbove: Int,
                                  helpAfterNanos: Long, spreadAboveNanos: Long,
                                  stuckAfterMillis: Long, overflow: Int,
                                  monitorEveryNanos: Long) extends Running {
    val id: Int = ownedCount.incrementAndGet()
    @volatile private var stopped = false
    private var watchdog: java.util.concurrent.ScheduledFuture[?] | Null = null

    /** stop the workers and the stuck-check. Idempotent. */
    def close(): Unit =
      stopped = true
      val wd = watchdog
      if wd != null then { val _ = wd.cancel(false) }
      val m = monitor
      if m != null then java.util.concurrent.locks.LockSupport.unpark(m)
      var i = 0
      while i < workers.length do
        java.util.concurrent.locks.LockSupport.unpark(workers(i).thread)
        i += 1
    /** what threads OUTSIDE the scheduler hand it. One queue, not one
     * per worker: a submitter that picks a random worker picks a
     * SLEEPING one most of the time and pays an unpark per task,
     * which is the JDK pool's cost and was measured as ours too
     * (1 249 -> 5 822 us when the victim became random). Here a
     * submission wakes a worker only when nobody is awake to see it,
     * or when the queue is deeper than `wakeAbove`. */
    private val submissions = java.util.concurrent.ConcurrentLinkedQueue[DriveTask[?]]()
    private val submissionsSize = java.util.concurrent.atomic.AtomicInteger()
    /** how many workers are running rather than parked */
    private val awake = java.util.concurrent.atomic.AtomicInteger(0)

    /** `n` owned workers, plus room for the ones the stuck-check
     * starts when a fiber blocks inside one */
    private val workers: Array[Worker] = Array.tabulate(n + overflow)(i => Worker(i))
    /** how many of them exist; the extras are started by the watchdog */
    private val live = java.util.concurrent.atomic.AtomicInteger(n)
    private val completed = java.util.concurrent.atomic.AtomicLong()
    @volatile private var lastCompleted = -1L
    // DEBUG-PROBE (schedulers-family): what actually happened
    private[okay] val activations = java.util.concurrent.atomic.AtomicLong()
    private[okay] val stepDowns = java.util.concurrent.atomic.AtomicLong()
    // DEBUG-PROBE (adaptive-blocking-io): tasks handed to a virtual thread
    private[okay] val spills = java.util.concurrent.atomic.AtomicLong()
    /** whether `spill` may hand work to a virtual thread: here, before the
     * workers start, so none reads it uninitialised */
    private val canSpill: Boolean = overflow > 0 && Schedulers.hasVirtualThreads
    private[okay] def stats: String =
      val sb = StringBuilder()
      var i = 0
      while i < live.get do
        val w: Worker = workers(i)
        sb.append(s"${w.id}:${w.ran}/${w.stolen} ")
        i += 1
      s"activations=${activations.get} stepDowns=${stepDowns.get} ran/stolen=[${sb.toString.trim}]"
    private[okay] def reset(): Unit =
      activations.set(0L)
      stepDowns.set(0L)
      var i = 0
      while i < workers.length do
        val w: Worker = workers(i)
        w.ran = 0L
        w.stolen = 0L
        i += 1
    private val current = ThreadLocal[Worker | Null]()

    /**
     * The owner's end and the thieves' end are different ends — a
     * Chase-Lev deque (own-deque, 2026-09-07). The owner pushes and
     * pops at `bottom` with plain reads and one volatile store; a
     * thief takes at `top` with a CAS. They contend only over the
     * last element, which is why `ForkJoinPool` does not have the
     * wall a shared queue gave us (14 054 us against the pool's
     * 2 954 on the same lane).
     *
     * Owner-only: `push`, `pop`. Any thread: `steal`, `size`.
     */

    private final class Worker(val id: Int) extends ManagedWorker.Hooks {
      /** the owner's own work: pushed and popped by this thread,
       * stolen from the other end */
      val deque = Deque(256)
      /** true from the moment the worker decides to park until it runs
       * again: what a submitter reads to wake it */
      @volatile var parked = false
      /** claimed by a `forkLong` that is waking this worker, cleared by
       * the worker once awake: `parked` stays true until the worker
       * RUNS, so two wakes in a row read the same sleeper, and a merge
       * forking two feeds woke one worker twice
       * (specs/adaptive-chunked-merge-cost.md) */
      val waking = java.util.concurrent.atomic.AtomicBoolean(false)
      var ran = 0L   // diagnostics only: plain, so the hot path has no fence
      var stolen = 0L
      val thread: Thread = ManagedWorker(this, s"okay-own-${Owned.this.id}-$id")   // a daemon
      def blocking(): Unit = Owned.this.blocking(this)
      def unblocked(): Unit = Owned.this.unblocked(this)

      /** how deep this worker is in the blocking door: owner thread only,
       * so a register that itself blocks is counted once */
      var blockDepth = 0

      /** the load the helper rule reads */
      def size: Int = deque.size

      /** the owner's own fork: onto its deque, no CAS, no signal */
      def pushLocal(t: DriveTask[?]): Unit = deque.push(t)

      /** the monitor's own record of `deque.thiefEnd` at its last look:
       * read and written by the monitor thread only */
      var seenThiefEnd = -1L

      /** owner only: its own end first, then what came from outside */
      private def take(): DriveTask[?] | Null =
        val t = deque.pop()
        if t != null then t else fromSubmissions()

      /** what a thief may take from this worker */
      def taken(): DriveTask[?] | Null = deque.steal()

      private def steal(): DriveTask[?] | Null =
        val alive = live.get
        var i = 1
        while i < alive do
          val t = workers((id + i) % alive).taken()
          if t != null then return t
          i += 1
        fromSubmissions()

      def run(): Unit =
        current.set(this)
        val _ = awake.incrementAndGet()
        // `stopped` ends the loop; a worker still finishes what it holds
        var spins = 0
        var streakStart = 0L        // when this run of work began: "am I hogging?"
        var windowStart = 0L        // the last checkpoint: "how big are the tasks NOW?"
        var windowRan = 0
        while !stopped do
          var t = take()
          if t == null then { t = steal(); if t != null then stolen += 1L }
          if t != null then
            spins = 0
            if streakStart == 0L then { streakStart = System.nanoTime(); windowStart = streakStart; windowRan = 0 }
            val _ = t.exec()
            ran += 1L
            windowRan += 1
            if stuckAfterMillis > 0L then { val _ = completed.incrementAndGet() }
            // THE HELPER RULE, in two clauses, because the fork/join
            // table has two columns. Work stays HOME while the queue
            // drains fast — that is kyo's win at 30 ns a fiber, where
            // waking a core costs more than the work. A helper is
            // woken only when this worker has been busy longer than
            // `helpAfterNanos`, still has work, AND its tasks are
            // averaging more than `spreadAboveNanos` — the pool's win
            // at 2.5 us a fiber, where a core is worth waking. The
            // average is free: the checkpoint already holds the
            // elapsed time and the count. nanoTime is read once per
            // 16 tasks, under 2 ns a task.
            if (windowRan & 15) == 0 && size > 0 then
              val now = System.nanoTime()
              // two different questions, two different spans. Have I
              // been hogging? — since the run of work began. Are the
              // tasks big enough to be worth another core? — over the
              // LAST sixteen only, because the fiber that forks a
              // burst is itself a long task and would otherwise make
              // every burst look expensive (measured: it made the
              // decision flip between iterations).
              if now - streakStart > helpAfterNanos && (now - windowStart) / windowRan > spreadAboveNanos then
                val _ = activateNext()
              windowStart = now
              windowRan = 0
          else if spins < spin then
            streakStart = 0L
            windowRan = 0
            spins += 1
            Thread.onSpinWait()
          else
            val _ = stepDowns.incrementAndGet()
            // publish "parked" and drop out of `awake` BEFORE the last
            // look at the queues: a submission that misses the flag
            // has landed in a queue we are about to see, one that sees
            // it will unpark us
            parked = true
            val _ = awake.decrementAndGet()
            if size == 0 && submissions.isEmpty && !stopped then java.util.concurrent.locks.LockSupport.park(this)
            parked = false
            waking.set(false)
            val _ = awake.incrementAndGet()
            // a worker woke: the monitor parks when nobody is awake, so
            // it may be asleep. Off the per-task path — once per park.
            if monitorParked then
              val m = monitor
              if m != null then java.util.concurrent.locks.LockSupport.unpark(m)
            spins = 0
    }

    private def startWorker(i: Int): Unit = workers(i).thread.start()

    /**
     * MANAGED BLOCKING (own-managed-blocking, specs/schedulers.md). A
     * fiber on `w` is about to park in a `CanBlock` door. The worker
     * leaves `awake` — a blocked worker sees no submission, and counting
     * it awake let an outside fork wake nobody — and when work is
     * waiting anywhere (`workPending`), ONE parked worker
     * is woken to take it; when none is parked and the scheduler has
     * overflow room (`watched`), one is started, within `n + overflow`,
     * as the stuck-check would a tick later. Plain `own` (no overflow)
     * only ever wakes: its thread count is its contract. The spare steps
     * down by parking when it runs dry, like any worker.
     */
    private def blocking(w: Worker): Unit =
      w.blockDepth += 1
      if w.blockDepth == 1 then
        val _ = awake.decrementAndGet()
        if workPending() && !activateNext() && overflow > 0 && grow(1) == 0 then spill(1, w)

    /** is work waiting anywhere — the submission queue or ANY worker's
     * deque, not only the blocking worker's: a woken worker that steals
     * one of a burst and blocks in turn must pass the wake on, or the
     * chain stops at the second link. Bounded by `live`; on the park
     * path only. */
    private def workPending(): Boolean =
      if submissionsSize.get > 0 then true
      else
        val alive = live.get
        var i = 0
        while i < alive && workers(i).size == 0 do i += 1
        i < alive

    private def unblocked(w: Worker): Unit =
      w.blockDepth -= 1
      if w.blockDepth == 0 then
        val _ = awake.incrementAndGet()
        // as a worker leaving a park does: the monitor parks when nobody
        // is awake, and a blocked worker was nobody
        if monitorParked then
          val m = monitor
          if m != null then java.util.concurrent.locks.LockSupport.unpark(m)
    var w0 = 0
    while w0 < n do { startWorker(w0); w0 += 1 }

    /**
     * THE MONITOR (own-scheduler-monitor, 2026-09-26; specs/schedulers.md,
     * "The monitor"). A fiber forked from a worker lands on its deque
     * with no signal, and the helper rule that could spread it runs
     * only every 16th completed task — so a few LONG fibers, or fibers
     * that block, sat behind a worker busy with one of them while the
     * others slept: eight 0.5 ms fibers on one thread, blocking fibers
     * reaching two workers of eight. Nothing on the per-task path can
     * know a fiber is long before it has run, so a separate thread
     * looks: every `monitorEveryNanos` it reads each worker's deque
     * (two volatile longs it already has) and calls a worker STUCK when
     * work waits there and its thief end has not moved since the last
     * look — the waiting tasks have waited a whole tick. For a stuck worker it wakes parked workers, one per waiting
     * task, and on a `watched` scheduler starts overflow workers when
     * nobody is parked. It parks itself after ~10 ms with no worker
     * awake and nothing queued, so an idle scheduler has no ticking
     * thread.
     */
    @volatile private var monitorParked = false
    private val monitor: Thread | Null =
      if monitorEveryNanos <= 0L then null
      else
        val t = Thread(() => monitorLoop(), s"okay-own-$id-monitor")
        t.setDaemon(true)
        t

    /** idle looks in a row before the monitor parks: ~10 ms at the
     * default tick. Parking at the FIRST idle look put an unpark — a
     * syscall — on the worker that woke next, once per park, and a
     * program that parks its workers between short operations paid it
     * every time (measured: +7% on sequential spawn/join). */
    private val idleLooksBeforePark: Int =
      if monitorEveryNanos <= 0L then 0 else math.max(1L, 10000000L / monitorEveryNanos).toInt

    private def monitorLoop(): Unit =
      var idle = 0
      while !stopped do
        if awake.get == 0 && submissionsSize.get == 0 then idle += 1 else idle = 0
        if idle >= idleLooksBeforePark then
          // publish "parked" BEFORE the last look, as a worker does: a
          // worker that wakes after it reads the flag and unparks us
          monitorParked = true
          if awake.get == 0 && submissionsSize.get == 0 && !stopped then
            java.util.concurrent.locks.LockSupport.park(this)
          monitorParked = false
          idle = 0
        else
          java.util.concurrent.locks.LockSupport.parkNanos(this, monitorEveryNanos)
          look()

    /** one look over every worker; bounded by `live`. Work has WAITED a
     * whole tick when the deque's thief end has not moved: it moves on
     * every steal and when the owner pops the last task, so a worker
     * turning over tiny tasks moves it constantly, and a worker working
     * down a burst — long tasks or short, blocking or not — leaves it
     * where it is. (The owner end was the first cut, and a burst of
     * 87 us tasks moved it between every two looks: never "stuck",
     * never spread.) */
    private def look(): Unit =
      val alive = live.get
      var i = 0
      while i < alive do
        val w: Worker = workers(i)
        val end = w.deque.thiefEnd
        val waiting = w.size
        if waiting > 0 && end == w.seenThiefEnd && !w.parked then
          val woken = wakeUpTo(waiting)
          if woken < waiting && overflow > 0 then
            val left = waiting - woken - grow(waiting - woken)
            if left > 0 then spill(left, w)
        w.seenThiefEnd = end
        i += 1
      // THE SUBMISSION QUEUE, by the same question
      // (adaptive-outside-long-fibers-serial, 2026-09-27). A fork from
      // outside wakes a worker only when nobody is awake, so eight long
      // fibers forked from `main` woke ONE and the other seven waited in
      // this queue while it worked: 560 ms for eight 70 ms fibers. Work
      // there has waited a whole tick when the same task is still at its
      // head — a worker taking tiny submissions changes the head
      // constantly, so they stay with the workers already awake.
      val head = submissions.peek()
      if head != null && (head eq seenHead) then
        val waiting = submissionsSize.get
        val woken = wakeUpTo(waiting)
        if woken < waiting && overflow > 0 then
          val left = waiting - woken - grow(waiting - woken)
          if left > 0 then spill(left, null)
      seenHead = head

    /** the submission queue's head at the monitor's last look: read and
     * written by the monitor thread only */
    private var seenHead: DriveTask[?] | Null = null

    /** unpark up to `k` distinct parked workers; how many were */
    private def wakeUpTo(k: Int): Int =
      val alive = live.get
      var woken = 0
      var i = 0
      while i < alive && woken < k do
        val w: Worker = workers(i)
        if w.parked then
          val _ = activations.incrementAndGet()
          java.util.concurrent.locks.LockSupport.unpark(w.thread)
          woken += 1
        i += 1
      woken

    /** start up to `k` overflow workers, never past `n + overflow`; how
     * many were */
    private def grow(k: Int): Int =
      var left = k
      var started = 0
      while left > 0 do
        val next = live.get
        if next >= n + overflow then left = 0
        else if live.compareAndSet(next, next + 1) then
          val _ = activations.incrementAndGet()
          startWorker(next)
          left -= 1
          started += 1
      started

    /**
     * SPILL AT THE BOUND (adaptive-blocking-io, 2026-09-28;
     * specs/schedulers.md "Blocking past the bound"). Every worker the
     * scheduler may own exists and work is still waiting behind blocked
     * ones: up to `k` tasks that have NOT STARTED run each on its own
     * virtual thread instead of waiting for a worker to come back. A
     * fiber that started cannot move (its stack is on the worker); one
     * that has not can run anywhere. On its virtual thread a raw socket
     * read unmounts and a door parks, so neither holds a platform thread;
     * its forks go to the submission queue (it is not a worker), and
     * after an `Await` it resumes where its answer arrives, the Drive's
     * rule on every member. Only where Loom exists and only on a
     * `watched` scheduler: plain `own` keeps its thread count. Called only
     * after `grow` came back short, so the per-task and per-fork paths
     * gain nothing. Taken from `from`'s deque first (the stuck worker's
     * waiting siblings), then the submission queue, then any deque.
     */
    private def spill(k: Int, from: Worker | Null): Unit =
      if canSpill then
        var left = k
        while left > 0 do
          val t = spillTake(from)
          if t == null then left = 0
          else
            val _ = spills.incrementAndGet()
            val _ = Thread.startVirtualThread: () =>
              val _ = t.exec()
              if stuckAfterMillis > 0L then { val _ = completed.incrementAndGet() }
            left -= 1

    private def spillTake(from: Worker | Null): DriveTask[?] | Null =
      val mine = if from != null then from.taken() else null
      if mine != null then mine
      else
        val s = fromSubmissions()
        if s != null then s
        else
          val alive = live.get
          var i = 0
          var t: DriveTask[?] | Null = null
          while t == null && i < alive do { t = workers(i).taken(); i += 1 }
          t

    if monitor != null then monitor.start()

    /** THE STUCK-CHECK (`Schedulers.adaptive`, `Schedulers.platform`).
     * A fiber that blocks inside a worker holds that thread; with every
     * worker blocked the program stops, and no policy over queues can
     * see it — the queues are not empty, they are unattended. So: every
     * `stuckAfterMillis`, if work is pending and NOTHING has completed
     * since the last look, wake a parked worker, and only when none is
     * parked start one more (up to `overflow`). Blocking then costs
     * latency rather than the program, which is what lets `own` be
     * chosen by someone who is not certain their fibers never block.
     * Off by default: it is a thread and a timer, and `Schedulers.loom`
     * is the answer when blocking is the norm rather than the exception.
     *
     * A parked worker FIRST (own-lost-wakeup, 2026-09-20). The first
     * cut only ever grew, and a stall is more often a lost wakeup than
     * a full house: a worker blocked inside a task counts as awake, so
     * an outside fork wakes nobody while the other workers sleep. Each
     * such stall then spent an overflow slot on a NEW thread while the
     * old ones slept, and once the slots were gone the next stall was
     * a hang — one blocked accept loop and a few connections were
     * enough. */
    if stuckAfterMillis > 0L && overflow > 0 then
      val check: Runnable = () =>
        val pending = submissionsSize.get > 0 || { var any = false; var i = 0; val alive = live.get
          while i < alive do { if workers(i).size > 0 then any = true; i += 1 }; any }
        val done = completed.get
        if pending && done == lastCompleted && !activateNext() then
          val next = live.get
          if next < n + overflow && live.compareAndSet(next, next + 1) then
            val _ = activations.incrementAndGet()
            startWorker(next)
          else if next >= n + overflow then spill(1, null)
        lastCompleted = done
      watchdog = timerWheel.scheduleWithFixedDelay(check, stuckAfterMillis, stuckAfterMillis,
        java.util.concurrent.TimeUnit.MILLISECONDS)

    def fork[A](prog: () => A ! Async): Fiber[A] =
      val t = DriveTask[A](prog, this)
      val mine = current.get
      if mine != null then mine.pushLocal(t)   // the owner's own end: no CAS, no signal
      else
        val _ = submissionsSize.incrementAndGet()
        submissions.offer(t)
        // a signal only when nobody would see it, or when the queue
        // has grown past what one worker should be left with
        if awake.get == 0 || submissionsSize.get > wakeAbove then { val _ = activateNext() }
      t

    /** a fiber the caller declares long (a channel's feed): forked as
     * `fork` does, then ONE parked worker woken, if there is one, to take
     * it — the thief of the forking worker's deque or of the submission
     * queue. Without it the second feed of a merge forked from outside
     * waits for the monitor to see it at the queue's head a whole tick
     * (100-200 us), which a ~200 us merge pays in full
     * (specs/adaptive-chunked-merge-cost.md). `fork` gains nothing. */
    override def forkLong[A](prog: () => A ! Async): Fiber[A] =
      val t = fork(prog)
      val _ = activateUnclaimed()
      t

    /** wake one parked worker no other `forkLong` is already waking */
    private def activateUnclaimed(): Boolean =
      val alive = live.get
      var i = 0
      while i < alive do
        val w = workers(i)
        if w.parked && w.waking.compareAndSet(false, true) then
          val _ = activations.incrementAndGet()
          java.util.concurrent.locks.LockSupport.unpark(w.thread)
          return true
        i += 1
      false

    private[okay] def fromSubmissions(): DriveTask[?] | Null =
      val t = submissions.poll()
      if t != null then { val _ = submissionsSize.decrementAndGet() }
      t

    /** wake ONE sleeping worker, wherever it sits: eligibility was
     * never the thing that made a worker help — being awake is. (The
     * first cut grew an "active prefix" and unparked only the worker
     * at its edge; a worker that had parked earlier then slept for
     * ever, and the probe found exactly that: one activation, two
     * workers running, 10 000 tasks.) False when nobody was parked —
     * what tells the stuck-check to grow instead. */
    private def activateNext(): Boolean =
      val alive = live.get
      var i = 0
      while i < alive do
        val w = workers(i)
        if w.parked then
          val _ = activations.incrementAndGet()
          java.util.concurrent.locks.LockSupport.unpark(w.thread)
          return true
        i += 1
      false
  }

  /** Hoisted out of `Owned` (it captures nothing from it) so the
   * conservation law in TestSchedulerLaws can drive it directly:
   * the deadlock it guards is a steal/grow race that a
   * whole-scheduler test can only catch by soaking
   * (own-long-join-deadlock, 2026-09-07). */
  private[okay] final class Deque(initial: Int) {
      private val top = java.util.concurrent.atomic.AtomicLong(0L)
      @volatile private var bottom: Long = 0L
      @volatile private var buf = java.util.concurrent.atomic.AtomicReferenceArray[DriveTask[?] | Null](initial)

      def size: Int =
        val n = bottom - top.get
        if n < 0 then 0 else n.toInt

      /** the thieves' end, as the monitor sees it: it moves on every
       * steal and when the owner pops the last task, never on a push */
      def thiefEnd: Long = top.get

      private def index(i: Long, len: Int): Int = (i & (len - 1)).toInt

      /** owner only */
      def push(t: DriveTask[?]): Unit =
        val b = bottom
        val tp = top.get
        var a = buf
        if b - tp >= a.length() - 1 then
          val bigger = java.util.concurrent.atomic.AtomicReferenceArray[DriveTask[?] | Null](a.length() * 2)
          var i = tp
          while i < b do { bigger.set(index(i, bigger.length()), a.get(index(i, a.length()))); i += 1 }
          buf = bigger
          a = bigger
        a.set(index(b, a.length()), t)
        bottom = b + 1

      /** owner only */
      def pop(): DriveTask[?] | Null =
        val a = buf
        val b = bottom - 1
        bottom = b
        val tp = top.get
        if tp > b then { bottom = tp; null }
        else
          val i = index(b, a.length())
          val t = a.get(i)
          if tp < b then { a.set(i, null); t }
          else
            // the last element: a thief may be taking it right now
            val won = top.compareAndSet(tp, tp + 1)
            bottom = tp + 1
            if won then { a.set(i, null); t } else null

      /** any thread.
       *
       * The slot is NOT cleared here, and that is load-bearing rather
       * than an oversight (own-long-join-deadlock, 2026-09-07). A
       * thief reads `top`, `bottom` and `buf` at three moments, so it
       * can be reading through an array the owner has already
       * replaced in `push`'s grow. Clearing the slot then writes a
       * null into an array another thief may still be reading, and
       * that thief's `top` CAS SUCCEEDS while its `a.get(i)` came
       * back null: the index is consumed and the task in it is never
       * run. One lost DriveTask is one fiber that never answers, so
       * `join` parks for ever with every worker legitimately idle —
       * measured exactly so: forked=40004 ran=40003, nothing in any
       * deque, one steal that won its CAS on a null slot.
       *
       * Canonical Chase-Lev leaves the slot for this reason. The cost
       * is that a taken task stays reachable until its slot is
       * overwritten, bounded by the buffer, and `pop` still clears
       * its own end where only the owner writes. */
      def steal(): DriveTask[?] | Null =
        val tp = top.get
        val b = bottom
        if tp >= b then null
        else
          val a = buf
          val i = index(tp, a.length())
          val t = a.get(i)
          if top.compareAndSet(tp, tp + 1) then t else null
    }

  /** listeners of a running DriveTask, a stack */
  private final class Waiters[A](val k: Either[Throwable, A] => Unit, val next: Waiters[A] | Null)

  /** one object: the pool task that walks the program, the cell its
   * answer lands in, and the Fiber a caller holds. The cell is
   * `null` (running, nobody waiting), a `Waiters` stack, or the
   * answer; the answer is written once. */
  private[okay] final class DriveTask[A](prog: () => A ! Async, home: Scheduler | Null = null)
      extends java.util.concurrent.ForkJoinTask[Unit] with Async.Drive[A] with Fiber[A]:
    private val cell = java.util.concurrent.atomic.AtomicReference[Waiters[A] | Either[Throwable, A] | Null](null)

    def exec(): Boolean =
      try apply(prog())
      catch case e: Throwable => fail(e)
      true
    def getRawResult(): Unit = ()
    def setRawResult(v: Unit): Unit = ()

    /** a late answer from one of OUR workers runs the fiber in place, as
     * every callback drive does; one from any other thread — the caller's
     * own consumer, a foreign callback — goes back to the fiber's home
     * scheduler, so that thread returns to its own work instead of doing
     * this fiber's. Measured: a consumer outside the pool freeing slots
     * of a 64-slot ring spent 31% of its time running the producers
     * (specs/adaptive-elementwise-small-ring.md). Plain `fork`, NOT
     * `forkLong`: a resume from a small ring comes every few elements,
     * and waking a sleeper each time (an unpark) made a 7-slot zip 2.7x
     * slower than `fork`, which wakes only when nobody is awake
     * (resume-late-small-ring-cost).
     *
     * SAFE FOR ORDER because the library's feeds name their route: a
     * partitioned channel used to know a producer by its thread, and a
     * producer moved by this handoff wrote its next run into another
     * part (resume-late-withdraw, TestMergeOrder red). Since
     * channel-route-per-producer a feed claims its part once and a
     * resume anywhere writes to it */
    override protected def resumeLate[X](x: X, k: X => A ! Async): Unit =
      val h = home
      val me = Thread.currentThread()
      // NOT inline when this thread is running ANOTHER fiber right now
      // (ready-merge-side-starves, 2026-09-30): a producer's `offer` woke
      // this consumer, and resumed here the consumer ran on the producer's
      // stack — a merge whose other side is always ready never parked
      // again, so the producer under it never ran again and a fold waiting
      // for its side waited for ever. Sent home instead; from a worker
      // that is `pushLocal`, no CAS and no wake, the cheap road
      // (resume-late-small-ring-cost). A callback on a worker between
      // fibers still resumes in place.
      if h == null || (me.isInstanceOf[ManagedWorker] && DriveTask.current(me) == null) then resumeHere(x, k)
      else { val _ = h.fork(() => async(resumeHere(x, k))) }

    protected def succeed(a: A): Unit = done(Right(a))
    protected def fail(e: Throwable): Unit = done(Left(e))

    @scala.annotation.tailrec private def done(r: Either[Throwable, A]): Unit =
      cell.get match
        case _: Either[?, ?] => ()
        case cur => if cell.compareAndSet(cur, r) then fire(cur, r) else done(r)

    @scala.annotation.tailrec private def fire(w: Waiters[A] | Either[Throwable, A] | Null, r: Either[Throwable, A]): Unit =
      w match
        case w: Waiters[A] @unchecked => { w.k(r); fire(w.next, r) }
        case _ => ()

    @scala.annotation.tailrec def onComplete(k: Either[Throwable, A] => Unit): Unit =
      cell.get match
        case r: Either[Throwable, A] @unchecked => k(r) // the cell only ever holds this task's own answer
        case w: Waiters[A] @unchecked => if !cell.compareAndSet(w, Waiters(k, w)) then onComplete(k)
        case null => if !cell.compareAndSet(null, Waiters(k, null)) then onComplete(k)

    /** answered: the cell holds an Either once, and only then */
    def answered: Boolean = cell.get match
      case _: Either[?, ?] => true
      case _ => false

    /** cancel ANSWERS the fiber, as every other member does: a
     * `join()` on a cancelled fiber must return rather than wait for
     * an answer that will never come. The drive stops at its next
     * operation and a late answer is ignored (`done` keeps the first
     * one), so this is the fiber's answer and nothing else can be.
     *
     * AND IT INTERRUPTS the thread running this drive's code, if one is
     * (drive-interrupts-blocking-run, 2026-09-28): Loom's cancel
     * interrupts the fiber's thread, so a use blocked in a `Run` —
     * `Thread.sleep`, a JDBC call, a `CanBlock` park — throws and its
     * bracket releases at the cancel. A drive used to stop only between
     * operations, and TestAsync's "a bracket cancelled by timeout
     * releases its resource" failed the moment `adaptive` became the
     * default. The interrupt is sent under this task's monitor while
     * `runner` names the thread, and the slice clears it under the same
     * monitor before it leaves, so it never outlives this drive's code
     * on a pooled worker (whose `park` an interrupt flag would turn into
     * a spin). */
    override def cancel(): Unit =
      super[Drive].cancel()
      synchronized:
        val t = runner
        if t != null then t.interrupt()
      done(Left(java.util.concurrent.CancellationException("fiber cancelled")))

    /** the thread running this drive's code right now, or null — set
     * by the slice, cleared by the slice, read by `cancel`; a slice of
     * ANOTHER drive nested inside one of ours on the same thread (a wake
     * resumed inline) suspends it, so our cancel never interrupts code
     * that is not ours */
    @volatile private var runner: Thread | Null = null

    override protected def sliceStarted(): AnyRef | Null =
      val me = Thread.currentThread()
      val outer = DriveTask.current(me)
      if outer != null then outer.suspend(me)
      DriveTask.setCurrent(me, this)
      runner = me
      outer

    /**
     * THE HANDSHAKE WITH `cancel`, without the monitor on the way out
     * (spawnjoin-rise-bisect: a monitor enter and exit on every slice of
     * every fiber cost `own` spawn/join most of 1.7x). `cancel` writes
     * `stopped` and THEN reads `runner`, under its monitor; a slice
     * leaving writes `runner = null` and THEN reads `stopped`. Both are
     * volatile, so they cannot both miss: either `cancel` read null and
     * sends nothing, or the slice sees the cancel — and then waits out
     * `cancel`'s critical section (an empty `synchronized`) before taking
     * the interrupt back, so an interrupt sent to `me` is never left on
     * a pooled worker. A slice never cancelled touches no monitor.
     */
    private def leave(me: Thread): Unit =
      if runner eq me then
        runner = null
        if cancelled then
          synchronized { () }
          val _ = Thread.interrupted()

    override protected def sliceEnded(token: AnyRef | Null): Unit =
      val me = Thread.currentThread()
      // our cancel's interrupt is ours to take back, not the thread's
      leave(me)
      token match
        case outer: DriveTask[?] =>
          DriveTask.setCurrent(me, outer)
          outer.resume(me)
        case _ => DriveTask.setCurrent(me, null)

    /** a nested slice starts on `me`: this drive's code is not running.
     * An interrupt our cancel already sent is taken back here, so the
     * nested code does not meet it, and `resume` sends it again */
    private def suspend(me: Thread): Unit = leave(me)

    /** the nested slice is over: this drive's code runs on `me` again,
     * and a cancel that landed meanwhile is delivered now */
    private def resume(me: Thread): Unit =
      // the same handshake the other way: `runner` written, then `stopped`
      // read — a cancel that read null is seen here and delivered by us
      if runner == null then
        runner = me
        if cancelled then me.interrupt()

  private[okay] object DriveTask:
    /** the drive whose slice is running on this thread, if any */
    val running: ThreadLocal[DriveTask[?] | Null] = new ThreadLocal[DriveTask[?] | Null]
    /** the drive running on `t` (the current thread): a field on our own
     * workers, the ThreadLocal on any other thread */
    def current(t: Thread): DriveTask[?] | Null = t match
      case w: ManagedWorker => w.drive
      case _ => running.get
    def setCurrent(t: Thread, d: DriveTask[?] | Null): Unit = t match
      case w: ManagedWorker => w.drive = d
      case _ => running.set(d)

  /** one honest platform thread per fiber: heavy, but works anywhere */
  val threads: Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val f = CompletableFuture[A]()
      val r: Runnable = () =>
        try { val _ = f.complete(Async.runFiber(prog())) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
      val t = Thread(r)
      t.start()
      fiberOf(f, () => t.interrupt())
}

/** Fire-and-forget daemon threads, adaptive the same way `Schedulers`
 * is (jdk17-adaptive-runtime): virtual where this JVM has them, an
 * ordinary daemon `Thread` otherwise. For the handful of call sites
 * across the codebase that just want "run this in the background,
 * named" and don't need a `Fiber`/`Scheduler` at all — an accept
 * loop, a tail loop, a chunked response writer. */
object Threads:
  def spawn(name: String)(body: () => Unit): Unit =
    val _ = spawnThread(name)(body)

  /** the same adaptive pick, handed back as a `Thread` for the callers
   * that need to `.join()` it (mostly test harnesses standing up a
   * throwaway socket server) rather than firing and forgetting. */
  def spawnThread(name: String)(body: () => Unit): Thread =
    if Schedulers.hasVirtualThreads then
      Thread.ofVirtual().name(name).start(() => body())
    else
      val t = Thread(() => body(), name)
      t.setDaemon(true)
      t.start()
      t

/** The default scheduler is `Schedulers.auto`: `adaptive` on a JVM that
 * HAS Loom (JDK 21+), since scheduler-default-flip (2026-09-28) — Loom
 * before that. `okay.scheduler` selects another for the A/B that prices
 * that choice (`loom`, `own`, `adaptive`, `drive`, `threads`); unset is
 * the shipped behaviour. scripts/ab-defaults.sh drives both arms.
 *
 * jdk-adaptive-scheduler (2026-09-19): on a JVM WITHOUT Loom, none of
 * that is available to ask for, property or no property — asking for
 * `loom` there (explicitly, or by leaving `okay.scheduler` unset,
 * which used to mean the same "loom" default unconditionally) now
 * degrades to `Schedulers.auto`, which is `own` on such a JVM, rather
 * than compiling fine and then throwing the first time a fiber forks.
 * `own`/`adaptive`/`drive`/`threads` need nothing JDK21-specific and
 * are always honoured as asked. */
given Scheduler =
  scala.util.Try(Option(System.getProperty("okay.scheduler"))).toOption.flatten match
    case Some("own")      => Schedulers.own.build
    case Some("adaptive") => Schedulers.adaptive.build
    case Some("drive")    => Schedulers.drive()
    case Some("threads")  => Schedulers.threads
    case Some("loom") if Schedulers.hasVirtualThreads => Schedulers.loom
    case _ => Schedulers.auto
