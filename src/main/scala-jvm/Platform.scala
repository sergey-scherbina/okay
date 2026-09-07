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
    if slot.filled then slot.value   // never waited
    else
      // publish who to wake BEFORE re-reading the flag: a completer
      // that misses the waiter is one whose flag we are about to see
      slot.waiter = Thread.currentThread()
      var out = false
      while !out do
        if slot.filled then out = true
        else
          java.util.concurrent.locks.LockSupport.park(slot)
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
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
    if !h.filled then h match
      case p: ParkHandoff[?] =>
        // publish who to wake BEFORE re-reading the flag, as `block` does
        p.waiter = Thread.currentThread()
        while !p.filled do
          java.util.concurrent.locks.LockSupport.park(p)
          if Thread.interrupted() then throw InterruptedException()
      case other =>
        throw IllegalStateException("a handoff not made by this CanBlock: " + other.getClass.getName)

  def blockAccepted(register: Accepted => (() => Unit)): Boolean =
    val slot = BoolSlot()
    val cancel = register: a =>
      slot.value = a
      slot.filled = true
      val t = slot.waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)
    if slot.filled then slot.value
    else
      slot.waiter = Thread.currentThread()
      var out = false
      while !out do
        if slot.filled then out = true
        else
          java.util.concurrent.locks.LockSupport.park(slot)
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
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
   * A scheduler that OWNS its threads — kyo's shape, the prototype
   * (schedulers-family, 2026-09-07). `workers` platform threads, a
   * queue each; the unit is `DriveTask`: no thread per fiber, a
   * parked Await costs its callback.
   *
   * The policy, from kyo's profile: a fiber forked FROM a worker goes
   * onto that worker's own queue (kyo's `Worker.current`), so a
   * program that forks 10 000 short children runs them where it is,
   * in order, with no signal — kyo's 79 ns per fork/join is that,
   * not a faster fiber. A fiber forked from outside goes to one of
   * the ACTIVE workers, `0 until active`, drawn at random; the
   * next worker is activated only when the chosen one is more than
   * `wakeAbove` deep. Parallelism grows with queued depth, not with
   * task count: the JDK pool wakes a worker for nearly every task
   * (`scan → signalWork → unpark`, 9 % of its profile, 22 % scanning)
   * and for fibers of tens of nanoseconds that spreading IS the
   * cost. A dry worker steals from the active ones, spins `spin`
   * rounds, then parks — deactivating itself if it is the top one.
   *
   * A BLOCKING call inside a fiber holds one of the `workers`
   * threads — this is for short, CPU-bound fibers; `loom` is the one
   * that makes blocking free.
   */
  def own(workers: Int = Runtime.getRuntime.availableProcessors(), spin: Int = 64, wakeAbove: Int = 64): Scheduler =
    Own(workers, spin, wakeAbove)

  private[okay] final class Own(n: Int, spin: Int, wakeAbove: Int) extends Scheduler {
    private val workers: Array[Worker] = Array.tabulate(n)(i => Worker(i))
    private val active = java.util.concurrent.atomic.AtomicInteger(1)
    private val current = ThreadLocal[Worker | Null]()

    private final class Worker(val id: Int) extends Runnable {
      val queue = java.util.concurrent.ConcurrentLinkedQueue[DriveTask[?]]()
      val size = java.util.concurrent.atomic.AtomicInteger()
      /** true from the moment the worker decides to park until it runs
       * again: what a submitter reads to wake it */
      @volatile var parked = false
      val thread: Thread = { val t = Thread(this, s"okay-own-$id"); t.setDaemon(true); t }

      def enqueue(t: DriveTask[?]): Unit =
        val _ = size.incrementAndGet()
        queue.offer(t)
        if parked then java.util.concurrent.locks.LockSupport.unpark(thread)

      private def take(): DriveTask[?] | Null =
        val t = queue.poll()
        if t != null then { val _ = size.decrementAndGet() }
        t

      private def steal(): DriveTask[?] | Null =
        val top = active.get
        var i = 1
        while i < top do
          val w = workers((id + i) % top)
          if w.id != id then
            val t = w.take()
            if t != null then return t
          i += 1
        null

      def run(): Unit =
        current.set(this)
        var spins = 0
        while true do
          var t = take()
          if t == null then t = steal()
          if t != null then
            spins = 0
            val _ = t.exec()
          else if spins < spin then
            spins += 1
            Thread.onSpinWait()
          else
            // the top active worker steps down before it parks, so a
            // submitter stops choosing it; worker 0 never steps down
            val top = active.get
            if id == top - 1 && id > 0 then { val _ = active.compareAndSet(top, id) }
            // publish "parked" BEFORE the last look at the queue: an
            // enqueue that misses the flag has landed in the queue we
            // are about to see, one that sees it will unpark us
            parked = true
            if queue.isEmpty then java.util.concurrent.locks.LockSupport.park(this)
            parked = false
            spins = 0
    }

    workers.foreach(_.thread.start())

    def fork[A](prog: () => A ! Async): Fiber[A] =
      val t = DriveTask[A](prog)
      choose().enqueue(t)
      t

    /** the caller's own worker; else an active one at random, and the
     * next worker activated when that one is over `wakeAbove` deep */
    private def choose(): Worker =
      val mine = current.get
      if mine != null then return mine
      val top = active.get
      val w = workers(java.util.concurrent.ThreadLocalRandom.current().nextInt(top))
      if w.size.get > wakeAbove && top < n && active.compareAndSet(top, top + 1) then
        val next = workers(top)
        java.util.concurrent.locks.LockSupport.unpark(next.thread)
        next
      else w
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

    override def cancel(): Unit = super[Drive].cancel()

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
