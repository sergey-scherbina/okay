package okay

/**
 * Asynchrony, Loom-style: on virtual threads blocking IS asynchrony,
 * so the whole effect is one operation — run this (possibly blocking)
 * computation. No callbacks, no scheduler, no fiber runtime of our
 * own: a program runs on a virtual thread (spawn), a blocked
 * operation parks that thread for free, and concurrency is just more
 * virtual threads (par, race). The handler answers every operation by
 * executing it — exactly tail-resumptive, so run is a relay, at relay
 * speed.
 */
enum Async[+A]:
  /** a suspended (possibly blocking) computation */
  case Run[A](run: () => A) extends Async[A]

/** suspend a (possibly blocking) computation as an operation */
inline def async[A](a: => A): A ! Async = effect(Async.Run(() => a))

/** execute the operation on the current (ideally virtual) thread */
given Handler[Async] = new:
  def handle[A](e: Async[A]): A = e match
    case Async.Run(f) => f()

/**
 * A fiber: a computation already running on its own thread of
 * control. join parks the caller until the answer is ready; cancel
 * requests interruption (best effort — the computation must be
 * interruptible to notice).
 */
trait Fiber[A]:
  /** park until finished, then the answer */
  def join(): A

  /** request interruption */
  def cancel(): Unit

/**
 * The scheduler: how a computation gets its own thread of control.
 * The default given is Loom — one virtual thread per fiber, which is
 * what makes blocking free. For a JVM without Loom, Scheduler.forkJoin
 * runs fibers on a pool (do not block long there), and
 * Scheduler.threads pays one honest platform thread per fiber.
 */
trait Scheduler:
  def fork[A](a: () => A): Fiber[A]

object Scheduler {

  import java.util.concurrent.{CompletableFuture, ExecutorService, ForkJoinPool}

  /** one Loom virtual thread per fiber: blocking parks, for free */
  val loom: Scheduler = new:
    def fork[A](a: () => A): Fiber[A] =
      val f = CompletableFuture[A]()
      val t = Thread.startVirtualThread: () =>
        try f.complete(a())
        catch case e: Throwable => f.completeExceptionally(e)
      new Fiber[A]:
        def join(): A = f.join()
        def cancel(): Unit = t.interrupt()

  /** a pool (the common fork-join by default): cheap fibers, but a
   * blocked fiber holds a pool thread — prefer loom for blocking work */
  def forkJoin(pool: ExecutorService = ForkJoinPool.commonPool()): Scheduler = new:
    def fork[A](a: () => A): Fiber[A] =
      val f = pool.submit(() => a())
      new Fiber[A]:
        def join(): A = f.get()
        def cancel(): Unit = { f.cancel(true); () }

  /** one honest platform thread per fiber: heavy, but works anywhere */
  val threads: Scheduler = new:
    def fork[A](a: () => A): Fiber[A] =
      val f = CompletableFuture[A]()
      val r: Runnable = () =>
        try f.complete(a())
        catch case e: Throwable => f.completeExceptionally(e)
      val t = Thread(r)
      t.start()
      new Fiber[A]:
        def join(): A = f.join()
        def cancel(): Unit = t.interrupt()
}

/** the default scheduler is Loom */
given Scheduler = Scheduler.loom

object Async {

  import !.*
  import java.util.concurrent.CompletableFuture

  /** handle by executing each operation in place, forwarding the effects F */
  def run[A, F[+_]](prog: A ! Async + F): A ! F =
    relay[A, A, Async, F](prog)(pure(_)):
      [X, Y] => e => e match
        case Run(f) => Cont.Pure(f())

  /** run the program on its own fiber (a virtual thread, by default) */
  def spawn[A](prog: => A ! Async)(using S: Scheduler): Fiber[A] =
    S.fork(() => prog.runWith)

  /** both, each on its own fiber */
  def par[A, B](a: => A ! Async, b: => B ! Async)(using Scheduler): (A, B) ! Async =
    async:
      val (fa, fb) = (spawn(a), spawn(b))
      (fa.join(), fb.join())

  /** park for the duration (a virtual thread parks for free) */
  inline def sleep(millis: Long): Unit ! Async = async(Thread.sleep(millis))

  /** the answer within the duration, or None; the loser is cancelled */
  def timeout[A](millis: Long)(prog: => A ! Async)(using Scheduler): Option[A] ! Async =
    race(prog.map(Some(_)), sleep(millis).map(_ => None))

  /** the first of the two to finish; the loser is cancelled */
  def race[A](a: => A ! Async, b: => A ! Async)(using S: Scheduler): A ! Async =
    async:
      val f = CompletableFuture[A]()
      val (fa, fb) = (spawn(a), spawn(b))
      S.fork(() => f.complete(fa.join()))
      S.fork(() => f.complete(fb.join()))
      val r = f.join()
      fa.cancel()
      fb.cancel()
      r
}
