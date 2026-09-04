package okay

/**
 * Native parks OS threads: that is the platform's ability, so
 * CanBlock is wait/notify and a fiber is one honest thread (no Loom
 * here, no CompletableFuture in the javalib — a hand-rolled cell).
 */
given CanBlock = new:
  def block[A](register: (A => Unit) => (() => Unit)): A =
    val lock = new Object
    var done = false
    var v: Option[A] = None
    val cancel = register { a =>
      lock.synchronized:
        v = Some(a)
        done = true
        lock.notifyAll()
    }
    try
      lock.synchronized:
        while !done do lock.wait()
    catch case e: Throwable => { cancel(); throw e }
    v.get   // done implies Some: the callback wrote it under the lock

  def blockAccepted(register: Accepted => (() => Unit)): Boolean =
    val lock = new Object
    var done = false
    var v = false
    val cancel = register: a =>
      lock.synchronized:
        v = a
        done = true
        lock.notifyAll()
    try
      lock.synchronized:
        while !done do lock.wait()
    catch case e: Throwable => { cancel(); throw e }
    v

/** the timer: a thread sleeps for the duration; cancelling
 * interrupts it out of the sleep */
given Timer = new:
  def after(millis: Long)(k: () => Unit): () => Unit =
    val t = Thread(() =>
      try { Thread.sleep(millis); k() }
      catch case _: InterruptedException => ())
    t.start()
    () => t.interrupt()

/**
 * Native schedulers (specs/cross-platform-async.md,
 * native-scheduler-pool). `threads` is the original one-per-fiber
 * design and stays the default: a fiber that genuinely blocks (a
 * CanBlock form — still real OS parking here, there is no Loom) on
 * a SHARED pool thread can starve every worker at once, so `pool`
 * is opt-in until a consumer sizes it for a workload that does not
 * park. What made a pool unsafe before — a waiting Channel held an
 * OS thread asleep — is gone (channel-callback): waiting is in
 * queues now, so ordinary fiber work never blocks a pool thread,
 * only an explicit park does.
 */
object Schedulers {

  /** one OS thread per fiber: heavy, but a blocking fiber never
   * competes with others for a shared worker */
  val threads: Scheduler = new:
    def fork[A](prog: () => A ! Async): Fiber[A] =
      val cell = FiberCell[A]()
      val t = Thread(() =>
        try cell.complete(Right(prog().runWith))
        catch case e: Throwable => cell.complete(Left(e)))
      t.start()
      new Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit = cell.subscribe(k)
        def cancel(): Unit = t.interrupt()

  /** a fixed pool of worker threads pulling fiber-start tasks from a
   * hand-rolled queue (no java.util.concurrent collection assumed
   * on Native's javalib) — fibers become cheap; a park still costs
   * a whole worker for as long as it parks */
  def pool(size: Int = 4): Scheduler = new:
    private val q = TaskQueue()
    for _ <- 0 until math.max(1, size) do
      val t = Thread(() => while true do q.take().run())
      t.setDaemon(true)
      t.start()

    def fork[A](prog: () => A ! Async): Fiber[A] =
      val cell = FiberCell[A]()
      val task = new Task(() =>
        try cell.complete(Right(prog().runWith))
        catch case e: Throwable => cell.complete(Left(e)))
      q.offer(task)
      new Fiber[A]:
        def onComplete(k: Either[Throwable, A] => Unit): Unit = cell.subscribe(k)
        // best effort (Fiber.cancel's own contract): a task still
        // queued is simply skipped when its turn comes; a task
        // already RUNNING is interrupted through the worker that
        // is currently running it — precisely THAT task, never a
        // later, unrelated one the same worker picks up next
        def cancel(): Unit = task.cancel()
}

/** the default scheduler is one thread per fiber */
given Scheduler = Schedulers.threads

/** one queued unit of work, cancellable while queued or running —
 * the runner reference is set only for the task actually executing
 * on it, so a stale cancel() can never reach a later task */
private final class Task(body: () => Unit):
  @volatile private var cancelled = false
  @volatile private var runner: Thread = null

  def run(): Unit =
    if !cancelled then
      runner = Thread.currentThread()
      if !cancelled then body()

  def cancel(): Unit =
    cancelled = true
    val r = runner
    if r != null then r.interrupt()

/** a plain FIFO queue, hand-rolled: workers block in wait() (a real
 * park — pool workers are meant to be few and long-lived, not the
 * thing native-scheduler-pool is optimizing away) */
private final class TaskQueue:
  private val lock = new Object
  private val q = scala.collection.mutable.Queue.empty[Task]

  def offer(task: Task): Unit = lock.synchronized:
    q.enqueue(task)
    lock.notify()

  def take(): Task = lock.synchronized:
    while q.isEmpty do lock.wait()
    q.dequeue()

/** one result, many subscribers (stm-sessions, specs/stm.md): a
 * `TRef[State]` instead of a hand-rolled `synchronized` cell — the
 * mutation and "who gets notified" decision are one `modify`, and
 * firing the callbacks OUTSIDE it (never inside `f`, which may run
 * more than once) is exactly the shape `TRef.modify`'s own doc
 * comment names: "the Channel returns its callbacks this way" */
private final class FiberCell[A]:
  private case class State(result: Option[Either[Throwable, A]] = None,
                           subs: List[Either[Throwable, A] => Unit] = Nil)
  private val cell = TRef(State())

  def complete(r: Either[Throwable, A]): Unit =
    val toRun = cell.modify { s =>
      if s.result.isDefined then (s, Nil) else (State(Some(r)), s.subs)
    }
    toRun.foreach(_(r))

  def subscribe(k: Either[Throwable, A] => Unit): Unit =
    val now = cell.modify { s =>
      s.result match
        case done @ Some(_) => (s, done)
        case None => (s.copy(subs = k :: s.subs), None)
    }
    now.foreach(k)
