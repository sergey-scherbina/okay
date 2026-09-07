package okay

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
        var out = false
        while !out do
          if Thread.interrupted() then throw InterruptedException()
          else if p.filled then out = true
          else java.util.concurrent.locks.LockSupport.park(p)
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
      var out = false
      while !out do
        if Thread.interrupted() then
          cancel()
          throw InterruptedException()
        else if slot.filled then out = true
        else java.util.concurrent.locks.LockSupport.park(slot)
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
    val fire: Runnable = () => { Thread.startVirtualThread(() => k()); () }
    val f = timerWheel.schedule(fire, millis, java.util.concurrent.TimeUnit.MILLISECONDS)
    () => { f.cancel(false); () }

/**
 * The JVM schedulers. The default given is Loom — one virtual thread
 * per fiber, which is what makes blocking free. For a JVM without
 * Loom, Schedulers.forkJoin runs fibers on a pool (do not park long
 * there), and Schedulers.threads pays one honest platform thread per
 * fiber.
 */
object Schedulers {

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
        try { val _ = f.complete(prog().runWith) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
      fiberOf(f, () => t.interrupt())

  /** a pool (the common fork-join by default): cheap fibers, but a
   * parked fiber holds a pool thread — prefer loom for blocking work */
  def forkJoin(pool: ExecutorService = ForkJoinPool.commonPool()): Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val f = CompletableFuture[A]()
      // an explicit Runnable: with a `() => Unit` lambda the two
      // `submit` overloads (Runnable and Callable[T]) both match
      val task: Runnable = () =>
        try { val _ = f.complete(prog().runWith) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
      val fut = pool.submit(task)
      fiberOf(f, () => { fut.cancel(true); () })

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
   * and `Schedulers.loom` makes it free.
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
                       private val overflowWorkers: Int = 0) {
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

    /** never spread: every fiber runs where it was forked. For bursts
     * of very short fibers, and for a program that wants its own
     * ordering back */
    def forShortTasks: Own = copy(spreadAboveNanos = Long.MaxValue)
    /** spread as soon as there is anything to spread: a core per
     * fiber where the machine has one */
    def forLongTasks: Own = copy(helpAfterNanos = 0L, spreadAboveNanos = 0L)

    /** start one more worker (up to `overflow`, default: as many as
     * there are workers) when work is pending and nothing has
     * completed for `after` — what makes a blocking fiber survivable */
    def watched(after: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(100, "ms"),
                overflow: Int = -1): Own =
      copy(stuckAfterMillis = math.max(1L, after.toMillis), overflowWorkers = if overflow < 0 then count else overflow)

    def build: Running =
      Owned(count, spinRounds, wakeDeeperThan, helpAfterNanos, spreadAboveNanos, stuckAfterMillis, overflowWorkers)
  }

  private[okay] final class Owned(n: Int, spin: Int, wakeAbove: Int,
                                  helpAfterNanos: Long, spreadAboveNanos: Long,
                                  stuckAfterMillis: Long, overflow: Int) extends Running {
    val id: Int = ownedCount.incrementAndGet()
    @volatile private var stopped = false
    private var watchdog: java.util.concurrent.ScheduledFuture[?] | Null = null

    /** stop the workers and the stuck-check. Idempotent. */
    def close(): Unit =
      stopped = true
      val wd = watchdog
      if wd != null then { val _ = wd.cancel(false) }
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
    private final class Deque(initial: Int) {
      private val top = java.util.concurrent.atomic.AtomicLong(0L)
      @volatile private var bottom: Long = 0L
      @volatile private var buf = java.util.concurrent.atomic.AtomicReferenceArray[DriveTask[?] | Null](initial)

      def size: Int =
        val n = bottom - top.get
        if n < 0 then 0 else n.toInt

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

      /** any thread */
      def steal(): DriveTask[?] | Null =
        val tp = top.get
        val b = bottom
        if tp >= b then null
        else
          val a = buf
          val i = index(tp, a.length())
          val t = a.get(i)
          if top.compareAndSet(tp, tp + 1) then { a.set(i, null); t } else null
    }

    private final class Worker(val id: Int) extends Runnable {
      /** the owner's own work: pushed and popped by this thread,
       * stolen from the other end */
      val deque = Deque(256)
      /** true from the moment the worker decides to park until it runs
       * again: what a submitter reads to wake it */
      @volatile var parked = false
      var ran = 0L   // diagnostics only: plain, so the hot path has no fence
      var stolen = 0L
      val thread: Thread = { val t = Thread(this, s"okay-own-${Owned.this.id}-$id"); t.setDaemon(true); t }

      /** the load the helper rule reads */
      def size: Int = deque.size

      /** the owner's own fork: onto its deque, no CAS, no signal */
      def pushLocal(t: DriveTask[?]): Unit = deque.push(t)

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
                activateNext()
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
            val _ = awake.incrementAndGet()
            spins = 0
    }

    private def startWorker(i: Int): Unit = workers(i).thread.start()
    var w0 = 0
    while w0 < n do { startWorker(w0); w0 += 1 }

    /** THE STUCK-CHECK (`Schedulers.adaptive`). A fiber that blocks
     * inside a worker holds that thread; with every worker blocked the
     * program stops, and no policy over queues can see it — the queues
     * are not empty, they are unattended. So: every `stuckAfterMillis`,
     * if work is pending and NOTHING has completed since the last
     * look, start one more worker. Blocking then costs latency rather
     * than the program, which is what lets `own` be chosen by someone
     * who is not certain their fibers never block. Off by default: it
     * is a thread and a timer, and `Schedulers.loom` is the answer
     * when blocking is the norm rather than the exception. */
    if stuckAfterMillis > 0L && overflow > 0 then
      val check: Runnable = () =>
        val pending = submissionsSize.get > 0 || { var any = false; var i = 0; val alive = live.get
          while i < alive do { if workers(i).size > 0 then any = true; i += 1 }; any }
        val done = completed.get
        if pending && done == lastCompleted then
          val next = live.get
          if next < n + overflow && live.compareAndSet(next, next + 1) then
            val _ = activations.incrementAndGet()
            startWorker(next)
        lastCompleted = done
      watchdog = timerWheel.scheduleWithFixedDelay(check, stuckAfterMillis, stuckAfterMillis,
        java.util.concurrent.TimeUnit.MILLISECONDS)

    def fork[A](prog: () => A ! Async): Fiber[A] =
      val t = DriveTask[A](prog)
      val mine = current.get
      if mine != null then mine.pushLocal(t)   // the owner's own end: no CAS, no signal
      else
        val _ = submissionsSize.incrementAndGet()
        submissions.offer(t)
        // a signal only when nobody would see it, or when the queue
        // has grown past what one worker should be left with
        if awake.get == 0 || submissionsSize.get > wakeAbove then activateNext()
      t

    private[okay] def fromSubmissions(): DriveTask[?] | Null =
      val t = submissions.poll()
      if t != null then { val _ = submissionsSize.decrementAndGet() }
      t

    /** wake ONE sleeping worker, wherever it sits: eligibility was
     * never the thing that made a worker help — being awake is. (The
     * first cut grew an "active prefix" and unparked only the worker
     * at its edge; a worker that had parked earlier then slept for
     * ever, and the probe found exactly that: one activation, two
     * workers running, 10 000 tasks.) */
    private def activateNext(): Unit =
      val alive = live.get
      var i = 0
      while i < alive do
        val w = workers(i)
        if w.parked then
          val _ = activations.incrementAndGet()
          java.util.concurrent.locks.LockSupport.unpark(w.thread)
          return
        i += 1
  }

  /** listeners of a running DriveTask, a stack */
  private final class Waiters[A](val k: Either[Throwable, A] => Unit, val next: Waiters[A] | Null)

  /** one object: the pool task that walks the program, the cell its
   * answer lands in, and the Fiber a caller holds. The cell is
   * `null` (running, nobody waiting), a `Waiters` stack, or the
   * answer; the answer is written once. */
  private[okay] final class DriveTask[A](prog: () => A ! Async)
      extends java.util.concurrent.ForkJoinTask[Unit] with Async.Drive[A] with Fiber[A]:
    private val cell = java.util.concurrent.atomic.AtomicReference[Waiters[A] | Either[Throwable, A] | Null](null)

    def exec(): Boolean =
      try apply(prog())
      catch case e: Throwable => fail(e)
      true
    def getRawResult(): Unit = ()
    def setRawResult(v: Unit): Unit = ()

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

    /** cancel ANSWERS the fiber, as every other member does: a
     * `join()` on a cancelled fiber must return rather than wait for
     * an answer that will never come. The drive stops at its next
     * operation and a late answer is ignored (`done` keeps the first
     * one), so this is the fiber's answer and nothing else can be. */
    override def cancel(): Unit =
      super[Drive].cancel()
      done(Left(java.util.concurrent.CancellationException("fiber cancelled")))

  /** one honest platform thread per fiber: heavy, but works anywhere */
  val threads: Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val f = CompletableFuture[A]()
      val r: Runnable = () =>
        try { val _ = f.complete(prog().runWith) }
        catch case e: Throwable => { val _ = f.completeExceptionally(e) }
      val t = Thread(r)
      t.start()
      fiberOf(f, () => t.interrupt())
}

/** the default scheduler is Loom */
given Scheduler = Schedulers.loom
