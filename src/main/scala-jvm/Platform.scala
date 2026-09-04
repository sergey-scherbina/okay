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
 */
private final class Slot[A]:
  var value: A = scala.compiletime.uninitialized
  val filled = java.util.concurrent.atomic.AtomicBoolean(false)
  @volatile var waiter: Thread | Null = null

/** the same one-shot handoff with the value as a primitive: a
 * `Slot[Boolean]` would box on the way in and out */
private final class BoolSlot:
  var value: Boolean = false
  val filled = java.util.concurrent.atomic.AtomicBoolean(false)
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
given CanBlock = new:
  def block[A](register: (A => Unit) => (() => Unit)): A =
    val slot = Slot[A]()
    val cancel = register: a =>
      slot.value = a
      slot.filled.set(true)          // release: value is written first
      val t = slot.waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)
    if slot.filled.get then slot.value   // never waited
    else
      // publish who to wake BEFORE re-reading the flag: a completer
      // that misses the waiter is one whose flag we are about to see
      slot.waiter = Thread.currentThread()
      var out = false
      while !out do
        if slot.filled.get then out = true
        else
          java.util.concurrent.locks.LockSupport.park(slot)
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
      slot.value

  def blockAccepted(register: Accepted => (() => Unit)): Boolean =
    val slot = BoolSlot()
    val cancel = register: a =>
      slot.value = a
      slot.filled.set(true)
      val t = slot.waiter
      if t != null then java.util.concurrent.locks.LockSupport.unpark(t.nn)
    if slot.filled.get then slot.value
    else
      slot.waiter = Thread.currentThread()
      var out = false
      while !out do
        if slot.filled.get then out = true
        else
          java.util.concurrent.locks.LockSupport.park(slot)
          if Thread.interrupted() then
            cancel()
            throw InterruptedException()
      slot.value

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
