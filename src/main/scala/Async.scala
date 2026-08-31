package okay

/**
 * Asynchrony, cross-platform (specs/cross-platform-async.md): programs
 * stay in the effect world — `A ! Async` composes by flatMap,
 * non-blocking by construction. The effect has two operations: Run, a
 * suspended (possibly blocking) computation — blocking is a PLATFORM
 * ability, Loom-style on the JVM where parking a virtual thread is
 * free; and Await, the universal callback-form suspension every
 * platform has. An Await's callback carries an ERROR CHANNEL (a
 * failure is a value on the wire, not a throw into nowhere) and its
 * registration answers with a CANCELLER (unregistering the timer or
 * the I/O completion is part of cancellation). Blocking exists only
 * at the run boundary and only under CanBlock evidence — on JS the
 * same programs run through the event loop by runAsync, and a
 * blocking join is a compile error, not a runtime hang.
 */
enum Async[+A]:
  /** a suspended (possibly blocking — a JVM/Native ability) computation */
  case Run[A](run: () => A) extends Async[A]

  /** the universal, callback-form suspension: register a continuation
   * (timers, I/O completions, promise adapters) and answer with the
   * canceller that unregisters it. The callback's Left is the error
   * channel: it fails the whole program at this operation. */
  case Await[A](register: (Either[Throwable, A] => Unit) => (() => Unit)) extends Async[A]

/** The class IS the whole identity here: `Async` has no parameter but
 * its (erased) answer type, so splitting a row on it is a TOTAL test
 * and there is nothing for the compiler to warn about — which is
 * exactly what this instance says, once, instead of letting it warn
 * "cannot be checked at runtime" at thirty-four use sites. */
given TypeableK[Async] = typeableK(classOf[Async[?]])

/** suspend a (possibly blocking) computation as an operation */
inline def async[A](a: => A): A ! Async = effect(Async.Run(() => a))

/** suspend on a callback registration (works on every platform) —
 * the simple form: success only, nothing to unregister */
inline def await[A](register: (A => Unit) => Unit): A ! Async =
  effect(Async.Await(k => { register(a => k(Right(a))); () => () }))

/**
 * Evidence that this platform can park a thread of control until a
 * callback fires. Given on JVM and Native; absent on JS, so every
 * blocking door (Handler[Async], Fiber.join, Async.run) is closed
 * there by the compiler.
 */
trait CanBlock:
  /** park the current thread until the registered callback fires; if
   * the park itself fails (interruption), the registration is
   * cancelled on the way out */
  def block[A](register: (A => Unit) => (() => Unit)): A

/** the platform timer: run a callback after the duration (a sleeping
 * virtual thread on the JVM, setTimeout on JS); the answer cancels */
trait Timer:
  def after(millis: Long)(k: () => Unit): () => Unit

/** execute each operation on the current (ideally virtual) thread;
 * an Await parks it until the callback fires */
given (using cb: CanBlock): Handler[Async] = new:
  def handle[A](e: Async[A]): A = e match
    case Async.Run(f) => f()
    case Async.Await(reg) => cb.block(reg).fold(e => throw e, identity)

/**
 * A fiber: a computation already running on its own thread of
 * control. The cross-platform surface is completion and cancellation;
 * the blocking join is derived from CanBlock evidence, so it exists
 * exactly where parking does; joinAsync is the effect-world join —
 * an Await, good on every platform.
 */
trait Fiber[A]:
  /** call k when finished — the universal observation */
  def onComplete(k: Either[Throwable, A] => Unit): Unit

  /** request cancellation (best effort — the computation must be
   * interruptible, or between operations, to notice) */
  def cancel(): Unit

  /** join as an operation: awaits the fiber, fails if it failed */
  def joinAsync: A ! Async = Async.await(k => { onComplete(k); () => () })

  /** park until finished, then the answer */
  def join()(using CanBlock): A = joinEither().fold(e => throw e, identity)

  /** park until finished; a failure as a value */
  def joinEither()(using cb: CanBlock): Either[Throwable, A] =
    cb.block(k => { onComplete(k); () => () })

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
  import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
  import scala.concurrent.{Future, Promise}

  /** the full callback form: an error channel in, a canceller out */
  def await[A](register: (Either[Throwable, A] => Unit) => (() => Unit)): A ! Async =
    effect(Await(register))

  /** handle by executing each operation in place, forwarding the
   * effects F; an Await parks (hence the evidence) */
  def run[A, F[+_]](prog: A ! Async + F)(using cb: CanBlock): A ! F =
    relay[A, A, Async, F](prog)(pure(_)):
      [X, Y] => e => e match
        case Run(f) => Cont.Pure(f())
        case Await(reg) => Cont.Pure(cb.block(reg).fold(e => throw e, identity))

  /**
   * The universal terminal: drive the tree through callbacks — Run
   * operations execute in place, an Await parks nothing, the
   * registered callback re-enters the drive. On JS this IS the event
   * loop runner; on the JVM it is a non-blocking alternative to run.
   */
  def runAsync[A](prog: A ! Async): Future[A] =
    val p = Promise[A]()
    Drive(p)(prog)
    p.future

  /** the callback may fire during registration, on this thread or
   * another: whoever loses the atomic exchange continues the drive */
  private final class Got(val x: Any)
  private object Moved

  /**
   * One driving of one tree: a while-loop while answers arrive
   * synchronously, a re-entry from the callback when they do not.
   * cancel() stops the drive at its next operation AND unregisters a
   * parked Await (the canceller the registration answered with).
   */
  private[okay] final class Drive[A](p: Promise[A]) {
    @volatile private var stopped = false
    @volatile private var unregister: () => Unit = () => ()

    def cancel(): Unit =
      stopped = true
      unregister()

    def apply(prog: A ! Async): Unit =
      var cur = prog
      var looping = !stopped
      while looping do
        looping = false
        try
          cur.fold[Unit](a => { p.trySuccess(a); () })([X] => e => k =>
            e match
              case Run(f) =>
                cur = k(f())
                looping = !stopped
              case Await(reg) =>
                val cell = AtomicReference[AnyRef](null)
                val cancelReg = reg { r =>
                  if !cell.compareAndSet(null, Got(r)) then
                    if !stopped then r match
                      case Right(x) => apply(k(x))
                      case Left(e) => { p.tryFailure(e); () }
                }
                cell.getAndSet(Moved) match
                  case g: Got =>
                    g.x.asInstanceOf[Either[Throwable, X]] match
                      case Right(x) =>
                        cur = k(x)
                        looping = !stopped
                      case Left(e) => { p.tryFailure(e); () }
                  case _ =>
                    unregister = cancelReg
                    if stopped then cancelReg()
          )
        catch case e: Throwable => { p.tryFailure(e); () }
  }

  /** run the program on its own fiber (a virtual thread by default on
   * the JVM, the event loop on JS) */
  def spawn[A](prog: => A ! Async)(using S: Scheduler): Fiber[A] =
    S.fork(() => prog)

  /** both, each on its own fiber — by completion callbacks, no
   * parking, every platform; a child failure fails the pair and
   * cancels the sibling */
  def par[A, B](a: => A ! Async, b: => B ! Async)(using Scheduler): (A, B) ! Async =
    await: k =>
      val (fa, fb) = (spawn(a), spawn(b))
      val done = AtomicBoolean(false)
      def fail(other: Fiber[?])(e: Throwable): Unit =
        if !done.getAndSet(true) then
          other.cancel()
          k(Left(e))
      fa.onComplete:
        case Right(x) => fb.onComplete:
          case Right(y) => if !done.getAndSet(true) then k(Right((x, y)))
          case Left(e) => fail(fa)(e)
        case Left(e) => fail(fb)(e)
      () => { fa.cancel(); fb.cancel() }

  /** park for the duration — an Await on the platform timer; the
   * timer's own canceller serves cancellation */
  def sleep(millis: Long)(using T: Timer): Unit ! Async =
    await(k => T.after(millis)(() => k(Right(()))))

  /** the answer within the duration, or None; the loser is cancelled */
  def timeout[A](millis: Long)(prog: => A ! Async)
                (using Scheduler, Timer): Option[A] ! Async =
    race(prog.map(Some(_)), sleep(millis).map(_ => None))

  /** the first of the two to SUCCEED; both losers are cancelled. A
   * failing contender does not win — but if both fail, the race
   * fails with the later error (nothing left to wait for). */
  def race[A](a: => A ! Async, b: => A ! Async)(using Scheduler): A ! Async =
    await: k =>
      val (fa, fb) = (spawn(a), spawn(b))
      val won = AtomicBoolean(false)
      val alive = AtomicInteger(2)
      def finish(r: Either[Throwable, A]): Unit = r match
        case Right(v) =>
          if !won.getAndSet(true) then
            fa.cancel(); fb.cancel()
            k(Right(v))
        case Left(e) =>
          if alive.decrementAndGet() == 0 && !won.getAndSet(true) then
            k(Left(e))
      fa.onComplete(finish)
      fb.onComplete(finish)
      () => { fa.cancel(); fb.cancel() }
}
