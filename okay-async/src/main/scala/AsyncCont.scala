package okay

import okay.core.Eff
import okay.one.Cont
import okay.one.step.Step as OStep
import okay.one.step.Step.step
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import okay.Effects.machine.{effect, handle, value, run as runAt}
import okay.core.{pure, defer}

/**
 * ASYNC ON THE ONE MONAD (cont-first-module; okay-core's since 2026-10-14): the same two operations, `Async.Run` and
 * `Async.Await`, performed in a program of `A ! R` (`import okay.*`) instead of the classic tree. The effect is not
 * redefined — an operation is a value of `Async[X]` on both encodings — only its words and its two handlers:
 *
 *  - `blocking`, an `Answering` handler: each operation answered IN PLACE, no capture — a Run executed, an Await
 *    parked (hence the evidence, as the classic's `Async.run`);
 *  - `runAsync`, the callback drive: the program STEPPED (okay-core's `Step`) — Async the one effect left, every
 *    operation comes to the top with the rest as data (`Resume`, multi-shot, from any thread). The drive executes a
 *    Run and runs the rest at once; an Await keeps the rest until the callback re-enters it.
 *
 * And the two bridges to the classic, per program: `toClassic` (each stop of the machine an operation of a
 * classic program) and `fromClassic` (a classic program as one Await of the machine).
 */
object AsyncCont:

  /** suspend a (possibly blocking) computation as an operation */
  def async[A](a: => A): Op[Async, A] = effect(Async.Run(() => a))

  /** suspend on a callback registration — success only, nothing to unregister */
  def await[A](register: (A => Unit) => Unit): Op[Async, A] =
    effect(Async.Await[A](k => { register(a => k(Right(a))); () => () }))

  /** the full callback form: an error channel in, a canceller out */
  def awaitEither[A](register: (Either[Throwable, A] => Unit) => (() => Unit)): Op[Async, A] =
    effect(Async.Await(register))

  /** each operation executed in place, an Await parked: `p.handle(AsyncCont.blocking)` */
  def blocking[A](using cb: CanBlock, w: Wait, p: Pause): okay.core.Answering[Async, A, A] = new okay.core.Answering[Async, A, A]:
    def ret(a: A): A = a
    def value[X](op: Async[X]): X = op match
      case Async.Run(f) => f()
      case Async.Await(reg, poll) => Async.pollThenBlock(reg, poll).fold(e => throw e, identity)

  /** the blocking terminal: a program with Async its last effect, to its value */
  def run[A](p: Eff[Async +: okay.core.Pure, A])(using CanBlock, Wait, Pause): A = p.handle(blocking[A]).value

  /** the callback terminal: never parks a thread; the future completes when the program answers, or fails at
   * the first throw, failed Await or failed step */
  def runAsync[A](p: Eff[Async +: okay.core.Pure, A]): Future[A] = runAsyncCancellable(p).future

  /** the callback terminal with its cancellation door (async-cancel), as the classic's: `cancel` stops the drive
   * at its next operation, unregisters a pending Await, releases the open cancel scopes and fails the future
   * with a `CancellationException`; idempotent */
  def runAsyncCancellable[A](p: Eff[Async +: okay.core.Pure, A]): Async.Running[A] =
    val promise = Promise[A]()
    val d = Drive(promise)
    try d.loop(p.step) catch case NonFatal(e) => promise.tryFailure(e)
    Async.Running(promise.future, () => d.cancel())

  /** the program run as a unit of its own, its failure a value: `Left` for a throw, a failed Await or a cancel
   * of its own; cancelling the program this is part of cancels it too (the Await's canceller) */
  def attempt[A](p: Eff[Async +: okay.core.Pure, A]): Op[Async, Either[Throwable, A]] =
    awaitEither[Either[Throwable, A]] { k =>
      val running = runAsyncCancellable(p)
      running.future.onComplete(t => k(Right(t.toEither)))(using scala.concurrent.ExecutionContext.parasitic)
      () => running.cancel()
    }

  // ---- fibers (async-fibers): the platform's own schedulers, the machine's drive on each

  /** a program on its own fiber, by the platform's `Scheduler` (a virtual thread on the JVM, the event loop on
   * JS, a thread on Native): the fiber runs ONE operation of the classic's, an Await whose registration starts the
   * machine's cancellable drive — so the program's synchronous part runs on the fiber's own thread of control,
   * and cancelling the fiber unregisters the Await, which cancels the drive */
  def spawn[A](p: Eff[Async +: okay.core.Pure, A])(using S: Scheduler): Fiber[A] =
    S.fork(() => Async.await[A] { k =>
      val running = runAsyncCancellable(p)
      running.future.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
      () => running.cancel()
    })

  /** `spawn` as an operation: the fiber starts when the program gets here */
  def fork[A](p: Eff[Async +: okay.core.Pure, A])(using Scheduler): Op[Async, Fiber[A]] = async(spawn(p))

  /** join a fiber as an operation: its answer, or its failure at this operation */
  def join[A](f: Fiber[A]): Op[Async, A] = awaitEither[A](k => { f.onComplete(k); () => () })

  /** park for the duration on the platform's timer; the timer's canceller serves cancellation */
  def sleep(millis: Long)(using T: Timer): Op[Async, Unit] = awaitEither[Unit](k => T.after(millis)(() => k(Right(()))))

  /** both, each on its own fiber, paired; the first failure fails the pair and cancels the other (the classic's
   * `par`, supervised) */
  def par[A, B](a: Eff[Async +: okay.core.Pure, A], b: Eff[Async +: okay.core.Pure, B])(using Scheduler): Op[Async, (A, B)] =
    awaitEither[(A, B)] { k =>
      val (fa, fb) = (spawn(a), spawn(b))
      val done = AtomicBoolean(false)
      def fail(other: Fiber[?])(e: Throwable): Unit =
        if !done.getAndSet(true) then { other.cancel(); k(Left(e)) }
      fb.onComplete:
        case Left(e) => fail(fa)(e)
        case Right(_) => ()
      fa.onComplete:
        case Right(x) => fb.onComplete:
          case Right(y) => if !done.getAndSet(true) then k(Right((x, y)))
          case Left(e) => fail(fa)(e)
        case Left(e) => fail(fb)(e)
      () => { fa.cancel(); fb.cancel() }
    }

  /** the first of the two to SUCCEED, the loser cancelled; if both fail, the later failure (the classic's) */
  def race[A](a: Eff[Async +: okay.core.Pure, A], b: Eff[Async +: okay.core.Pure, A])(using Scheduler): Op[Async, A] =
    awaitEither[A] { k =>
      val (fa, fb) = (spawn(a), spawn(b))
      val won = AtomicBoolean(false)
      val alive = java.util.concurrent.atomic.AtomicInteger(2)
      def finish(r: Either[Throwable, A]): Unit = r match
        case Right(v) => if !won.getAndSet(true) then { fa.cancel(); fb.cancel(); k(Right(v)) }
        case Left(e) => if alive.decrementAndGet() == 0 && !won.getAndSet(true) then k(Left(e))
      fa.onComplete(finish)
      fb.onComplete(finish)
      () => { fa.cancel(); fb.cancel() }
    }

  /** the answer within the duration, or None, the program cancelled; its own failure comes through at once
   * (the classic's `timeout`, timeout-masks-failure) */
  def timeout[A](millis: Long)(p: Eff[Async +: okay.core.Pure, A])(using Scheduler, Timer): Op[Async, Option[A]] =
    awaitEither[Option[A]] { k =>
      val f = spawn(p)
      val done = AtomicBoolean(false)
      val timer = summon[Timer].after(millis): () =>
        if !done.getAndSet(true) then { f.cancel(); k(Right(None)) }
      f.onComplete: r =>
        if !done.getAndSet(true) then { timer(); k(r.map(Some(_))) }
      () => { if !done.getAndSet(true) then { timer(); f.cancel() } }
    }

  /** the operations that open and close a cancel scope (`Async.CancelScope`): the drive keeps the scope while it
   * is open and releases it if the program is cancelled, fails, or ends with it still open; on the blocking
   * handler they are empty Runs, as on the classic's */
  def enter(scope: Async.CancelScope): Op[Async, Unit] = effect(Async.Run(Async.Enter(scope)))
  def exit(scope: Async.CancelScope): Op[Async, Unit] = effect(Async.Run(Async.Exit(scope)))

  /** A MACHINE PROGRAM AS A CLASSIC ONE, for code still on the tree (the Scala 2 facade, a classic caller): each
   * operation the machine stops at becomes the same operation in the classic program, the rest of the machine
   * its continuation — so the classic handler in force (blocking, a drive, a fiber's) answers it, and nothing
   * runs before the classic program is run */
  def toClassic[A](p: Eff[Async +: okay.core.Pure, A]): okay.freer.![A, Async] =
    okay.freer.Free.delay(() => classic(p.step))

  /** A CLASSIC PROGRAM AS ONE OPERATION OF THE MACHINE: driven by the classic callback drive, its answer the
   * Await's — so it never parks a thread the machine's handler did not choose to park */
  def fromClassic[A](p: => okay.freer.![A, Async]): Op[Async, A] =
    awaitEither[A] { k =>
      val running = Async.runAsyncCancellable(p)
      running.future.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
      () => running.cancel()
    }

  /** the row of a program of the machine's Async: the one effect left */
  type R = Async +: okay.core.Pure

  /** `toClassic`'s road: the program stepped, each operation an operation of the classic program, the rest its
   * continuation — so the classic handler in force (blocking, a drive, a fiber's) answers it */
  private def classic[A](s: OStep[R, A]): okay.freer.![A, Async] = s match
    case OStep.Done(a) => okay.freer.pure(a)
    case OStep.Failed(e) => throw e
    case y: OStep.Yield[R, A, e, x] => y.in match
      case _: okay.one.row.In.Here[f, t] =>
        okay.freer.effect(y.op).flatMap(v => okay.freer.Free.delay(() => classic(y.resume(v))))
      case th: okay.one.row.In.There[f, g, t] => okay.one.row.In.absurd(th.in)

  /**
   * THE DRIVE ANSWERS IN PLACE WHAT IT CAN (drive-in-place): a Run is executed where the program stops at it and
   * the rest run at once, in this loop. Only an Await still pending stops the loop: the drive keeps the rest for
   * the callback and parks. The callback may fire during the registration, on this thread or another: whoever
   * comes SECOND to the flag continues — the loop in place, or the callback by re-entering `loop` with the rest
   * resumed, a run of the machine of its own. The answer is written before the callback's turn at the flag, so
   * the one that comes second reads it.
   */
  private final class Drive[A](promise: Promise[A]):
    @volatile private var stopped = false
    /** the pending Await's canceller */
    @volatile private var unregister: () => Unit = () => ()
    /** the open cancel scopes, newest first; changed under the drive's monitor */
    @volatile private var scopes: List[Async.CancelScope] = Nil

    private def marked(m: Async.ScopeMark): Unit = synchronized {
      scopes = if m.entering then m.scope :: scopes else scopes.filterNot(_ eq m.scope)
    }
    private def releaseScopes(): Unit = scopes.foreach(s => try s.released() catch case NonFatal(_) => ())

    def cancel(): Unit =
      val now = synchronized { val was = stopped; stopped = true; !was }
      if now then
        unregister()
        releaseScopes()
        promise.tryFailure(java.util.concurrent.CancellationException("cancelled")): Unit

    private def cancelled: Throwable = java.util.concurrent.CancellationException("cancelled")

    /** the loop: the program stepped to its next Await still pending, or to its end — a loop, not a frame */
    def loop(s0: OStep[R, A]): Unit =
      var s: OStep[R, A] | Null = s0
      while s != null do s = next(s)

    /** one stop of the program: the step after it, or `null` — the end, or an Await parked */
    private def next(s: OStep[R, A]): OStep[R, A] | Null = s match
      case OStep.Done(a) =>
        // a scope still open at the end was never exited: its program stopped early — released
        releaseScopes(); promise.trySuccess(a); null
      case OStep.Failed(e) =>
        releaseScopes(); promise.tryFailure(e); null
      case y: OStep.Yield[R, A, e, x] => y.in match
        case th: okay.one.row.In.There[f, g, t] => okay.one.row.In.absurd(th.in)
        case _: okay.one.row.In.Here[f, t] =>
          val op: Async[x] = y.op
          op match
            case Async.Run(f) =>
              if stopped then y.resume.fail(cancelled)
              else
                f match
                  case m: Async.ScopeMark => marked(m)
                  case _ => ()
                try y.resume(f()) catch case NonFatal(err) => y.resume.fail(err)
            case Async.Await(reg, poll) =>
              if stopped then y.resume.fail(cancelled)
              else
                // ONE poll, no wait (drive-poll-then-park, as the classic's drive)
                val now = if poll == null then null else poll()
                if now != null then answered[x](now, y.resume)
                else
                  val got = AtomicReference[Either[Throwable, x] | Null](null)
                  val second = AtomicBoolean(false)
                  val cancelReg = reg { r =>
                    got.set(r)
                    if second.getAndSet(true) && !stopped then loop(answered[x](r, y.resume))
                  }
                  if !second.getAndSet(true) then
                    unregister = cancelReg
                    if stopped then cancelReg()
                    null
                  else answered[x](got.get.nn, y.resume)

    /** the rest resumed with the callback's answer: its value, or its failure raised where the Await was */
    private def answered[X](r: Either[Throwable, X], k: OStep.Resume[X, R, A]): OStep[R, A] = r match
      case Right(x) => k(x)
      case Left(e) => k.fail(e)
