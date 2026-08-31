package okay

import java.util.concurrent.{CompletableFuture, CompletionException, ExecutionException, ExecutorService, ForkJoinPool}

/**
 * The JVM can park, and on Loom parking is free: blocking IS
 * asynchrony here. CanBlock parks the current (ideally virtual)
 * thread on a future; interruption cancels the wait.
 */
given CanBlock = new:
  def block[A](register: (A => Unit) => (() => Unit)): A =
    val f = CompletableFuture[A]()
    val cancel = register(a => { f.complete(a); () })
    try f.get()
    catch case e: Throwable => { cancel(); throw e }

/** the timer: a virtual thread sleeps for the duration; cancelling
 * interrupts it out of the sleep */
given Timer = new:
  def after(millis: Long)(k: () => Unit): () => Unit =
    val t = Thread.startVirtualThread: () =>
      try { Thread.sleep(millis); k() }
      catch case _: InterruptedException => ()
    () => t.interrupt()

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
