package okay

import scala.annotation.implicitNotFound
import scala.annotation.tailrec

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
   * channel: it fails the whole program at this operation.
   *
   * `poll`, when given, answers what `register` would answer AT ONCE,
   * or null when it would have to register. A runner that has other
   * work to do asks it instead of registering (`ReadyMerge`,
   * poll-then-park, specs/ready-merge.md); every other runner ignores
   * it, and a wrapper that changes the answer (a guard, a recovery)
   * drops it — an operation without a poll is registered, as always. */
  case Await[A](register: (Either[Throwable, A] => Unit) => (() => Unit),
                poll: (() => (Either[Throwable, A] | Null)) | Null = null) extends Async[A]

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
    case Async.Await(reg, _) => cb.block(reg).fold(e => throw e, identity)

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

  // `Failing[Async]` belongs to Async's implicit scope even though the
  // implementation lives in its own file.
  export AsyncFailing.given

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
        case Await(reg, _) => Cont.Pure(cb.block(reg).fold(e => throw e, identity))

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
  /**
   * A CANCEL SCOPE A RUNNING PROGRAM OPENS WITH ITS DRIVE
   * (ready-merge-cancel-under-consumer-ops, 2026-09-27). The callback
   * drive, cancelled, stops at the next operation and knows only what
   * that operation and its last parked Await can release. A program
   * that holds registrations it made ITSELF — `mergeReady`'s parked
   * sources, living inside a consumer's continuation that the drive
   * will never call — is invisible there. So a program says `enter`
   * (an `Async.Run` the drive recognises by class) and the drive keeps
   * the scope until `exit`; a cancel — between operations, while
   * parked, or a program that ENDS with the scope still open (a
   * consumer that stopped early) — runs its `release`. `release` may run
   * more than once and from any thread, so it must be idempotent. On a
   * blocking handler (Loom's `Async.run`) the markers are empty `Run`s:
   * there a cancel is an interrupt, seen at the next wait, whose own
   * canceller does the releasing.
   */
  final class CancelScope(release: () => Unit):
    private[okay] def released(): Unit = release()
    /** the operation that opens the scope */
    def enter[F[+_]]: Unit ! Async + F = okay.effect(Run(Enter(this)))
    /** the operation that closes it */
    def exit[F[+_]]: Unit ! Async + F = okay.effect(Run(Exit(this)))

  /** the two markers share a class, so the drive's `Run` arm asks ONE
   * class test of every operation (+1.5% on a pure-Run chain with two) */
  private[okay] sealed abstract class ScopeMark(val scope: CancelScope, val entering: Boolean) extends (() => Unit):
    def apply(): Unit = ()
  private[okay] final class Enter(s: CancelScope) extends ScopeMark(s, true)
  private[okay] final class Exit(s: CancelScope) extends ScopeMark(s, false)

  private[okay] trait Drive[A] {
    /** where the answer goes — a Promise on JS, the task's own cell
     * on the JVM (`Schedulers.DriveTask`, one object for fiber, task
     * and promise) */
    protected def succeed(a: A): Unit
    protected def fail(e: Throwable): Unit
    @volatile private var stopped = false
    @volatile private var unregister: () => Unit = () => ()
    /** the open cancel scopes (CancelScope), newest first. A volatile
     * field changed under the drive's own monitor, not an
     * AtomicReference: that was 24 B on EVERY fiber's drive for a list
     * almost every fiber leaves empty, and a scope opens and closes
     * rarely (once per `mergeReady` run) */
    @volatile private var scopes: List[CancelScope] = Nil

    private def marked(m: ScopeMark): Unit = synchronized {
      scopes = if m.entering then m.scope :: scopes else scopes.filterNot(_ eq m.scope)
    }
    /** every open scope released — idempotent releases, so a second call
     * (the loop's own stop after a cancel from outside) is harmless and
     * catches what was registered in between */
    private def releaseScopes(): Unit =
      scopes.foreach(s => try s.released() catch case _: Throwable => ())

    def cancel(): Unit =
      stopped = true
      unregister()
      releaseScopes()

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
          // the rotation is `Free.resume`'s, so this loop is three
          // cases and turns once per OPERATION rather than once per
          // node. The `stopped` check therefore no longer falls
          // between two rotation steps — which changes nothing a
          // canceller can observe: rotating reassociates nodes and
          // runs no user code, and the check that matters, the one
          // before the next operation, is exactly where it was.
          (cur.resume: @unchecked) match
            case Free.Return(a) =>
              // a scope still open at the END was never exited: its
              // program stopped early (a consumer that took what it
              // needed) — release what it holds
              releaseScopes()
              succeed(a)
            case Free.Bind(Free.Inject(e), f) =>
              val next = op(e, f)
              if next != null then
                cur = next
                looping = !stopped
                if !looping then { discontinue(cur); releaseScopes() }
            case Free.Inject(e) =>
              val next = op(e, Free.Return(_))
              if next != null then
                cur = next
                looping = !stopped
                if !looping then { discontinue(cur); releaseScopes() }
      catch case e: Throwable => { releaseScopes(); fail(e) }

    /**
     * CANCELLED BETWEEN TWO OPERATIONS (drive-discontinue, 2026-09-26):
     * the rest of the program is dropped here, and a Resource scope
     * inside it would drop its finalizers with it. The next operation
     * is the LEFTMOST node of the tree, reached by descending `Bind`s
     * without rotating them or calling any continuation, so no user
     * code runs. If a scope guarded it, it releases. A `Return`, or a
     * `Delay` whose thunk is user code, has nothing reachable, and a
     * scope that has already finished has already released.
     */
    @tailrec private def discontinue(p: Free[Async, ?]): Unit = p match
      case Free.Bind(a, _) => discontinue(a)
      case Free.Inject(Run(d: Discontinue)) => d.discontinue()
      case Free.Inject(Await(d: Discontinue, _)) => d.discontinue()
      case _ => ()

    /** one operation: the continuation to drive next when the answer
     * came synchronously, null when the drive parked on a callback
     * (which re-enters `apply`) or the program finished here */
    private def op[X](e: Async[X], k: X => A ! Async): (A ! Async) | Null = e match
      case Run(f) =>
        f match
          case m: ScopeMark => marked(m)
          case _ => ()
        k(f())
      case Await(reg, _) =>
        // the cell holds the answer, the "moved on" marker, or nothing:
        // typed, so what comes out is the operation's Either
        val cell = AtomicReference[Got[X] | Moved.type | Null](null)
        val cancelReg = reg { r =>
          if !cell.compareAndSet(null, Got(r)) then
            if !stopped then r match
              case Right(x) => apply(k(x))
              case Left(e) => { releaseScopes(); fail(e) }
        }
        cell.getAndSet(Moved) match
          case g: Got[X] =>
            g.x match
              case Right(x) => k(x)
              case Left(e) => { releaseScopes(); fail(e); null }
          case _ =>
            unregister = cancelReg
            if stopped then cancelReg()
            null
  }

  /** the Drive that answers into a Promise (JS's scheduler, `toFuture`) */
  private[okay] final class PromiseDrive[A](p: Promise[A]) extends Drive[A] {
    protected def succeed(a: A): Unit = { val _ = p.trySuccess(a) }
    protected def fail(e: Throwable): Unit = { val _ = p.tryFailure(e) }
    /** cancel ANSWERS the fiber (specs/cross-platform-async.md,
     * supervised-waits-on-failure): a stopped drive parked in an Await
     * never resumes, so nothing else would ever settle this promise —
     * a join on the fiber waited forever, and a scope waiting for its
     * cancelled child would hang. A late real answer is ignored, as
     * `trySuccess`/`tryFailure` already say. */
    override def cancel(): Unit =
      super.cancel()
      val _ = p.tryFailure(java.util.concurrent.CancellationException("fiber cancelled"))
  }

  /** run the program on its own fiber (a virtual thread by default on
   * the JVM, the event loop on JS) */
  def spawn[A](prog: => A ! Async)(using S: Scheduler): Fiber[A] =
    S.fork(() => prog)

  /**
   * AN OPEN SUPERVISED SCOPE (Ox's `supervised`, as an effect).
   *
   * `par` supervises exactly two, and `Par.traverse` supervises none
   * -- it spawns a flat sequence and joins in order, which its own
   * header says does not cancel the siblings of a leaf that failed.
   * Measured 2026-09-18: nine siblings sleeping 3 s were all waited
   * for. That is honest for a traverse and wrong for a scope.
   *
   * This is the scope: fork as many children as you like, wherever
   * you like, and the SCOPE owns them.
   *
   *   supervised: n ?=>
   *     val a = n.fork(fetchUser)
   *     val b = n.fork(fetchOrders)
   *     direct { !a.joinAsync + !b.joinAsync }
   *
   * THE GUARANTEE, and it is the same one Ox sells:
   *   - the scope does not finish while a child is still running;
   *   - the FIRST failure -- a child's or the body's -- cancels every
   *     other child and leaves the scope with that error;
   *   - cancellation is best effort, as everywhere else here: a child
   *     must be interruptible, or between operations, to notice.
   *
   * Callbacks, not parking, so it runs on every platform -- the same
   * reason `par` is written this way.
   */
  final class Nursery private[okay] (S: Scheduler):
    private val live = AtomicInteger(0)
    private val kids = AtomicReference(List.empty[Fiber[?]])
    private val failed = AtomicBoolean(false)
    private val idle = AtomicReference[(() => Unit) | Null](null)
    private[okay] var onFirstFailure: Throwable => Unit = _ => ()

    /** fork a child into this scope */
    def fork[B](p: => B ! Async): Fiber[B] =
      val _ = live.incrementAndGet()
      val f = Async.spawn(p)(using S)
      val _ = kids.updateAndGet(f :: _)
      f.onComplete:
        case Left(e) =>
          if !failed.getAndSet(true) then onFirstFailure(e)
          settle()
        case Right(_) => settle()
      // A CHILD FORKED AFTER THE SCOPE FAILED is cancelled as it joins:
      // `cancelAll` reaches only the kids it saw, and the body may still
      // be forking (supervision-shapes-race, 2026-09-23 — such a child
      // was never cancelled; a test waited 5 s for it). `cancelAll` sets
      // `failed` BEFORE reading `kids`, and this reads `failed` AFTER
      // joining `kids`, so one of the two always sees the other.
      if failed.get() then f.cancel()
      f

    private def settle(): Unit =
      if live.decrementAndGet() == 0 then fireIdle()

    private def fireIdle(): Unit =
      val cb = idle.getAndSet(null)
      if cb != null then cb()

    /** run cb once no child is running (at once if none ever was) */
    private[okay] def whenIdle(cb: () => Unit): Unit =
      if live.get() == 0 then cb()
      else
        idle.set(cb)
        // a child may have finished between the check and the set
        if live.get() == 0 then fireIdle()

    private[okay] def cancelAll(): Unit =
      failed.set(true)
      kids.get().foreach(_.cancel())

  /** @see [[Nursery]] */
  def supervised[A](body: Nursery ?=> A ! Async)(using S: Scheduler): A ! Async =
    await: k =>
      val n = Nursery(S)
      val settled = AtomicBoolean(false)
      val first = AtomicReference[Throwable | Null](null)
      def done(r: Either[Throwable, A]): Unit =
        if !settled.getAndSet(true) then k(r)

      // THE FAILURE ANSWER WAITS FOR THE CHILDREN, as the success answer
      // does (supervised-waits-on-failure, 2026-09-28): the header's
      // "the scope does not finish while a child is still running" used
      // to hold only on the Right path — a failure cancelled the children
      // and answered at once, and a child the cancel reached late (its
      // own drive delivers it a moment after `cancelAll`) was still
      // running when the scope had already answered. Safe because cancel
      // ANSWERS the fiber on every platform (specs/cross-platform-async.md):
      // `whenIdle` fires once each child's answer, cancelled or not, has
      // come in. The FIRST failure is the scope's: a body that fails after
      // a child did finds `first` taken and registers nothing, and a body
      // that SUCCEEDS after a child failed reads `first` when idle fires,
      // so the later registration cannot turn the answer into a Right.
      def failWith(e: Throwable): Unit =
        if first.compareAndSet(null, e) then
          n.cancelAll()
          n.whenIdle(() => done(Left(e)))

      n.onFirstFailure = failWith

      val main = spawn(body(using n))
      main.onComplete:
        case Left(e) => failWith(e)
        case Right(a) => n.whenIdle(() => done(first.get() match
          case null => Right(a)
          case e => Left(e)))
      () =>
        n.cancelAll()
        main.cancel()

  /**
   * Both, each on its own fiber — by completion callbacks, no
   * parking, every platform. EITHER side's failure fails the pair at
   * once and cancels the sibling.
   *
   * THE WORD "EITHER" IS THE FIX (par-right-failure-waits, BUGS.md).
   * The two completions used to be registered in a NEST: `fb`'s
   * callback was installed inside `fa`'s Right branch, so while the
   * left side ran, nobody was listening to the right one. The answer
   * was still correct, only late, which is why it rode through every
   * test — measured 2026-09-17, the same failure in the two orders
   * came back after 0.0007 s and 3.017 s, and the healthy sibling ran
   * to completion instead of being cancelled.
   *
   * The failure watch is now registered on BOTH sides up front, and
   * the pairing stays nested for the success road, where it costs
   * nothing and needs no cell to hold the first answer. A fiber takes
   * several subscribers on every platform (a waiter list on the JVM's
   * DriveTask, `whenComplete` on a CompletableFuture, `subscribe` on
   * Native's cell, a Future callback on JS), and a callback
   * registered on an ALREADY finished fiber fires at once — both
   * checked before relying on them. `done` keeps the first answer, so
   * a side that fails after the other already did is ignored.
   */
  def par[A, B](a: => A ! Async, b: => B ! Async)(using Scheduler): (A, B) ! Async =
    await: k =>
      val (fa, fb) = (spawn(a), spawn(b))
      val done = AtomicBoolean(false)
      def fail(other: Fiber[?])(e: Throwable): Unit =
        if !done.getAndSet(true) then
          other.cancel()
          k(Left(e))
      // the right side's FAILURE, watched from the start rather than
      // from whenever the left side happens to finish
      fb.onComplete:
        case Left(e) => fail(fa)(e)
        case Right(_) => ()
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
