package okay

import scala.annotation.implicitNotFound

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
enum Async[+A] derives Effect:
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
/** a computation that PARKS a thread, as a first-class VALUE
 * (specs/context-functions.md, ctx-blocking): storable, composable,
 * and only an edge HOLDING the capability can force it — the
 * platform practice the seams already follow, named as a type */
type Blocking[A] = CanBlock ?=> A

@implicitNotFound("no CanBlock capability in scope.\nBlocking parks a thread, so it must be GRANTED, not assumed: take it through the door\n(a `Blocking[A]` = `CanBlock ?=> A` parameter, or `using CanBlock`), and the runtime installs\nit where parking is safe (a virtual thread on the JVM). JS has no blocking — restructure with Async.")
/**
 * The answer to a send: was the element taken?
 *
 * A dedicated type rather than `Boolean => Unit`, because
 * `scala.Function1` is specialised on Int, Long, Float and Double and
 * NOT on Boolean — so every acceptance answer went through
 * `Function1.apply(Object)` and boxed a `java.lang.Boolean`. It was
 * 8% of the leaf samples on the elementwise channel path, spent
 * entirely on carrying one bit. A single abstract method taking a
 * primitive boxes nothing, and a lambda still reads the same at the
 * call site.
 */
trait Accepted:
  def apply(accepted: Boolean): Unit

trait CanBlock:
  /** park the current thread until the registered callback fires; if
   * the park itself fails (interruption), the registration is
   * cancelled on the way out */
  def block[A](register: (A => Unit) => (() => Unit)): A

  /** the same wait, for the one answer that is a primitive: `block`
   * is generic, so its slot and its callback both box a Boolean.
   * This one carries the bit as a bit. */
  def blockAccepted(register: Accepted => (() => Unit)): Boolean

  /** a handoff this platform can park on — the callback that is its
   * own slot (see `Handoff`); `receiveBlocking` makes one per call */
  def handoff[A](): Handoff[A]

  /** park until the handoff is filled. Returns at once if it already
   * is — a `receiveInto` that answered the end synchronously has filled
   * it before this is called. Interruption is rethrown. */
  def await(h: Handoff[?]): Unit

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
    PromiseDrive(p)(prog)
    p.future

  /** the callback may fire during registration, on this thread or
   * another: whoever loses the atomic exchange continues the drive */
  private final class Got[X](val x: Either[Throwable, X])
  private object Moved

  /**
   * One driving of one tree: a while-loop while answers arrive
   * synchronously, a re-entry from the callback when they do not.
   * cancel() stops the drive at its next operation AND unregisters a
   * parked Await (the canceller the registration answered with).
   */
  private[okay] trait Drive[A] {
    /** where the answer goes — a Promise on JS, the task's own cell
     * on the JVM (`Schedulers.DriveTask`, one object for fiber, task
     * and promise) */
    protected def succeed(a: A): Unit
    protected def fail(e: Throwable): Unit
    @volatile private var stopped = false
    @volatile private var unregister: () => Unit = () => ()

    def cancel(): Unit =
      stopped = true
      unregister()

    /**
     * A direct loop over the tree's cases, the shape `runFree` and
     * Stm's runner have: the rotation and the `Bind(Pure, f)` step as
     * in `Free.fold`, the operation dispatched by a method call.
     * `fold` with a polymorphic handler value did the same work with a
     * closure built per operation and the answer threaded back through
     * it — measured at 12.6 us of a 4000-bind chain on the JVM
     * (docs/benchmarks.md §18b/§18c).
     */
    def apply(prog: A ! Async): Unit =
      var cur: A ! Async = prog
      var looping = !stopped
      try
        while looping do
          looping = false
          cur match
            case Free.Pure(a) => succeed(a)
            case Free.Bind(Free.Bind(a, f), g) =>
              cur = Free.Bind(a, f(_).flatMap(g))
              looping = !stopped
            case Free.Bind(Free.Pure(a), f) =>
              cur = f(a)
              looping = !stopped
            case Free.Bind(Free.Inject(e), f) =>
              val next = op(e, f)
              if next != null then
                cur = next
                looping = !stopped
            case Free.Inject(e) =>
              val next = op(e, Free.Pure(_))
              if next != null then
                cur = next
                looping = !stopped
      catch case e: Throwable => fail(e)

    /** one operation: the continuation to drive next when the answer
     * came synchronously, null when the drive parked on a callback
     * (which re-enters `apply`) or the program finished here */
    private def op[X](e: Async[X], k: X => A ! Async): (A ! Async) | Null = e match
      case Run(f) => k(f())
      case Await(reg) =>
        // the cell holds the answer, the "moved on" marker, or nothing:
        // typed, so what comes out is the operation's Either
        val cell = AtomicReference[Got[X] | Moved.type | Null](null)
        val cancelReg = reg { r =>
          if !cell.compareAndSet(null, Got(r)) then
            if !stopped then r match
              case Right(x) => apply(k(x))
              case Left(e) => fail(e)
        }
        cell.getAndSet(Moved) match
          case g: Got[X] =>
            g.x match
              case Right(x) => k(x)
              case Left(e) => { fail(e); null }
          case _ =>
            unregister = cancelReg
            if stopped then cancelReg()
            null
  }

  /** the Drive that answers into a Promise (JS's scheduler, `toFuture`) */
  private[okay] final class PromiseDrive[A](p: Promise[A]) extends Drive[A] {
    protected def succeed(a: A): Unit = { val _ = p.trySuccess(a) }
    protected def fail(e: Throwable): Unit = { val _ = p.tryFailure(e) }
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

  /** the program's failure as DATA: it runs on its own fiber, and
   * whatever it threw arrives as a Left instead of unwinding this
   * program — the effect-world try/catch, one fiber, every platform.
   * Cancelling the Await cancels the fiber. */
  def attempt[A](prog: => A ! Async)(using Scheduler): Either[Throwable, A] ! Async =
    await: k =>
      val f = spawn(prog)
      f.onComplete(r => k(Right(r)))
      () => f.cancel()

  /** park for the duration — an Await on the platform timer; the
   * timer's own canceller serves cancellation */
  def sleep(millis: Long)(using T: Timer): Unit ! Async =
    await(k => T.after(millis)(() => k(Right(()))))

  /**
   * the answer within the duration, or None; the loser is cancelled.
   *
   * NOT a `race` against `sleep` (timeout-masks-failure, 2026-09-09):
   * a race lets a failing contender lose without ending the race, so
   * a program that failed AT ONCE came out as `None` after the whole
   * duration with its exception replaced by a timeout — a breaker's
   * refusal under a deadline became a 504 after five seconds. The
   * law: the FIRST outcome of the program, of either kind, settles
   * the timeout, and only the timer answers None. So the program's
   * own failure comes through the error channel at once, the timer
   * is unregistered, and the fiber is cancelled when the timer wins.
   */
  def timeout[A](millis: Long)(prog: => A ! Async)
                (using S: Scheduler, T: Timer): Option[A] ! Async =
    await: k =>
      val f = spawn(prog)
      val done = AtomicBoolean(false)
      val timer = T.after(millis): () =>
        if !done.getAndSet(true) then
          f.cancel()
          k(Right(None))
      f.onComplete: r =>
        if !done.getAndSet(true) then
          timer()
          k(r.map(Some(_)))
      () => { if !done.getAndSet(true) then { timer(); f.cancel() } }

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
