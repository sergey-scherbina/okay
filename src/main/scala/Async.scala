package okay

/**
 * Asynchrony, cross-platform (specs/cross-platform-async.md): programs
 * stay in the effect world — `A ! Async` composes by flatMap,
 * non-blocking by construction. The effect has two operations: Run, a
 * suspended (possibly blocking) computation — blocking is a PLATFORM
 * ability, Loom-style on the JVM where parking a virtual thread is
 * free; and Await, the universal callback-form suspension every
 * platform has. Blocking exists only at the run boundary and only
 * under CanBlock evidence — on JS the same programs run through the
 * event loop by runAsync, and a blocking join is a compile error, not
 * a runtime hang.
 */
enum Async[+A]:
  /** a suspended (possibly blocking — a JVM/Native ability) computation */
  case Run[A](run: () => A) extends Async[A]

  /** the universal, callback-form suspension: register a continuation
   * (timers, I/O completions, promise adapters); the handler parks for
   * it where parking exists, the event-loop runner just waits */
  case Await[A](register: (A => Unit) => Unit) extends Async[A]

/** suspend a (possibly blocking) computation as an operation */
inline def async[A](a: => A): A ! Async = effect(Async.Run(() => a))

/** suspend on a callback registration (works on every platform) */
inline def await[A](register: (A => Unit) => Unit): A ! Async =
  effect(Async.Await(register))

/**
 * Evidence that this platform can park a thread of control until a
 * callback fires. Given on JVM and Native; absent on JS, so every
 * blocking door (Handler[Async], Fiber.join, Async.run) is closed
 * there by the compiler.
 */
trait CanBlock:
  /** park the current thread until the registered callback fires */
  def block[A](register: (A => Unit) => Unit): A

/** the platform timer: run a callback after the duration (a sleeping
 * virtual thread on the JVM, setTimeout on JS) */
trait Timer:
  def after(millis: Long)(k: () => Unit): Unit

/** execute each operation on the current (ideally virtual) thread;
 * an Await parks it until the callback fires */
given (using cb: CanBlock): Handler[Async] = new:
  def handle[A](e: Async[A]): A = e match
    case Async.Run(f) => f()
    case Async.Await(reg) => cb.block(reg)

/**
 * A fiber: a computation already running on its own thread of
 * control. The cross-platform surface is completion and cancellation;
 * the blocking join is derived from CanBlock evidence, so it exists
 * exactly where parking does.
 */
trait Fiber[A]:
  /** call k when finished — the universal observation */
  def onComplete(k: Either[Throwable, A] => Unit): Unit

  /** request cancellation (best effort — the computation must be
   * interruptible, or between operations, to notice) */
  def cancel(): Unit

  /** park until finished, then the answer */
  def join()(using CanBlock): A = joinEither().fold(e => throw e, identity)

  /** park until finished; a failure as a value */
  def joinEither()(using cb: CanBlock): Either[Throwable, A] = cb.block(onComplete)

/**
 * The scheduler: how a program gets its own thread of control. It
 * takes the PROGRAM, not a computed answer — that is what lets the
 * event loop be a scheduler too. The default given is Loom on the
 * JVM (one virtual thread per fiber), the event loop on JS, one OS
 * thread per fiber on Native.
 */
trait Scheduler:
  def fork[A](prog: () => A ! Async): Fiber[A]

object Async {

  import !.*
  import java.util.concurrent.atomic.AtomicReference
  import scala.concurrent.{Future, Promise}

  /** handle by executing each operation in place, forwarding the
   * effects F; an Await parks (hence the evidence) */
  def run[A, F[+_]](prog: A ! Async + F)(using cb: CanBlock): A ! F =
    relay[A, A, Async, F](prog)(pure(_)):
      [X, Y] => e => e match
        case Run(f) => Cont.Pure(f())
        case Await(reg) => Cont.Pure(cb.block(reg))

  /**
   * The universal terminal: drive the tree through callbacks — Run
   * operations execute in place, an Await parks nothing, the
   * registered callback re-enters the drive. On JS this IS the event
   * loop runner; on the JVM it is a non-blocking alternative to run.
   */
  def runAsync[A](prog: A ! Async): Future[A] =
    val p = Promise[A]()
    drive(prog, p, () => false)
    p.future

  /** the drive-state handshake: who continues after an Await —
   * whoever loses the exchange (the callback may fire during
   * registration, on this thread or another) */
  private final class Got(val x: Any)
  private object Moved

  private[okay] def drive[A](prog: A ! Async, p: Promise[A],
                             cancelled: () => Boolean): Unit =
    var cur = prog
    var looping = !cancelled()
    while looping do
      looping = false
      try
        cur.fold[Unit](a => { p.trySuccess(a); () })([X] => e => k =>
          e match
            case Run(f) =>
              cur = k(f())
              looping = !cancelled()
            case Await(reg) =>
              val cell = AtomicReference[AnyRef](null)
              reg { x =>
                if !cell.compareAndSet(null, Got(x)) then
                  if !cancelled() then drive(k(x), p, cancelled)
              }
              cell.getAndSet(Moved) match
                case g: Got =>
                  cur = k(g.x.asInstanceOf[X])
                  looping = !cancelled()
                case _ => () // the callback continues the drive later
        )
      catch case e: Throwable => { p.tryFailure(e); () }

  /** run the program on its own fiber (a virtual thread by default on
   * the JVM, the event loop on JS) */
  def spawn[A](prog: => A ! Async)(using S: Scheduler): Fiber[A] =
    S.fork(() => prog)

  /** both, each on its own fiber; a child failure propagates (which
   * is why par parks — a callback par needs an error channel in
   * Await, an open box of the spec) */
  def par[A, B](a: => A ! Async, b: => B ! Async)
               (using Scheduler, CanBlock): (A, B) ! Async =
    async:
      val (fa, fb) = (spawn(a), spawn(b))
      (fa.join(), fb.join())

  /** park for the duration — an Await on the platform timer */
  def sleep(millis: Long)(using T: Timer): Unit ! Async =
    await(k => T.after(millis)(() => k(())))

  /** the answer within the duration, or None; the loser is cancelled */
  def timeout[A](millis: Long)(prog: => A ! Async)
                (using Scheduler, Timer): Option[A] ! Async =
    race(prog.map(Some(_)), sleep(millis).map(_ => None))

  /** the first of the two to FINISH; both losers are cancelled, and a
   * failing contender never wins (as ever) — cross-platform, by
   * completion callbacks, no parking anywhere */
  def race[A](a: => A ! Async, b: => A ! Async)(using Scheduler): A ! Async =
    await: k =>
      val won = java.util.concurrent.atomic.AtomicBoolean(false)
      val (fa, fb) = (spawn(a), spawn(b))
      def finish(r: Either[Throwable, A]): Unit = r match
        case Right(v) =>
          if !won.getAndSet(true) then
            fa.cancel(); fb.cancel()
            k(v)
        case Left(_) => ()
      fa.onComplete(finish)
      fb.onComplete(finish)
}
