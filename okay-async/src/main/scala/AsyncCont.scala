package okay

import okay.core.Eff
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}
import okay.Effects.machine.{effect, handle, value}

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
    val d = Async.PromiseDrive[A](promise)
    d.applyKernel(p)
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

  /** the row of a program of the machine's Async: the one effect left */
  type R = Async +: okay.core.Pure
