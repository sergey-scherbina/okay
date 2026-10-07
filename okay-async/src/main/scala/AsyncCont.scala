package okay

import okay.cont.{Answering, Cap, Clause, Cont, Has, Machine, Reaches, Root, Top}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
 * ASYNC ON THE MACHINE (cont-first-module): the same two operations, `Async.Run` and `Async.Await`, performed
 * in a program of the machine's `A ! R` (`import okay.*`) instead of the classic tree. The effect is not
 * redefined — an operation is a value of `Async[X]` on both encodings — only its words and its two handlers:
 *
 *  - `blocking`, an `Answering` handler: each operation answered IN PLACE, no capture — a Run executed, an Await
 *    parked (hence the evidence, as the classic's `Async.run`);
 *  - `runAsync`, the callback drive: Async is the OUTERMOST handler, a clause at the top level (`Root`), so every
 *    operation stops the machine with the rest as a program at the top — the `generate` pattern. The drive
 *    executes a Run and runs the rest at once; an Await keeps the rest until the callback re-enters it.
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
  def blocking[A](using cb: CanBlock, w: Wait, p: Pause): Answering[Async, A, A] = new Answering[Async, A, A]:
    def ret(a: A): A = a
    def value[X](op: Async[X]): X = op match
      case Async.Run(f) => f()
      case Async.Await(reg, poll) => Async.pollThenBlock(reg, poll).fold(e => throw e, identity)

  /** the blocking terminal: a program with Async its last effect, to its value */
  def run[A](p: A ! (Async +: Pure))(using CanBlock, Wait, Pause): A = p.handle(blocking[A]).value

  /** the callback terminal: never parks a thread; the future completes when the program answers, or fails at
   * the first throw, failed Await or failed step */
  def runAsync[A](p: A ! (Async +: Pure)): Future[A] =
    val promise = Promise[A]()
    val d = Drive(promise)
    d.loop(top(p, d.clause, Driven.Done(_)))
    promise.future

  /** A MACHINE PROGRAM AS A CLASSIC ONE, for code still on the tree (the Scala 2 facade, a classic caller): each
   * operation the machine stops at becomes the same operation in the classic program, the rest of the machine
   * its continuation — so the classic handler in force (blocking, a drive, a fiber's) answers it, and nothing
   * runs before the classic program is run */
  def toClassic[A](p: A ! (Async +: Pure)): okay.freer.![A, Async] =
    okay.freer.Free.delay(() => classic(top(p, stops[A], Step.Done(_))))

  private def classic[A](t: Top[Step[A]]): okay.freer.![A, Async] = Machine.value(t) match
    case Step.Done(a) => okay.freer.pure(a)
    case Step.At(op, k) => okay.freer.effect(op).flatMap(x => okay.freer.Free.delay(() => classic(k(x))))

  /** A CLASSIC PROGRAM AS ONE OPERATION OF THE MACHINE: driven by the classic callback drive, its answer the
   * Await's — so it never parks a thread the machine's handler did not choose to park */
  def fromClassic[A](p: => okay.freer.![A, Async]): Op[Async, A] =
    awaitEither[A] { k =>
      val running = Async.runAsyncCancellable(p)
      running.future.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
      () => running.cancel()
    }

  /** where the program stands: its answer, or stopped at an operation with the rest as a program at the top */
  private enum Step[+A]:
    case Done(a: A)
    case At[X, A](op: Async[X], k: X => Top[Step[A]]) extends Step[A]

  /** `toClassic`'s clause, at the top: every operation stops the machine with its continuation, for the classic
   * handler in force to answer */
  private def stops[A]: Clause[Async, EmptyTuple, Step[A]] = new Clause[Async, EmptyTuple, Step[A]]:
    def apply[X](op: Async[X], k: X => Top[Step[A]]): Top[Step[A]] = Cont.Return(Step.At(op, k))

  private def top[A, Ans](p: A ! (Async +: Pure), clause: Clause[Async, EmptyTuple, Ans], done: A => Ans): Top[Ans] =
    okay.cont.handle[Async, A, Ans](done)(clause)(using Root): inner ?=>
      p.run(using inner, Has.HCons(Cap.Reaching(Reaches.here[Async, Ans, inner.type]), Has.HNil[inner.type]()))

  /** where the drive stands when the machine stops: the program's answer, or parked at an Await whose callback
   * will continue it */
  private enum Driven[+A]:
    case Done(a: A)
    case Parked

  /**
   * THE DRIVE ANSWERS IN PLACE WHAT IT CAN (drive-in-place). Its clause returns `k(x)` as its whole body for a Run
   * (`x` its value) and for an Await whose callback fired during the registration — and a clause that returns the
   * last `k(x)` is answered WHERE THE OPERATION WAS, the stack as it stands, nothing captured (the machine's own
   * tail-resumption, `Captured.tail`). Only an Await still pending stops the machine: the clause keeps `k` for the
   * callback and answers `Parked`. The callback may fire during the registration, on this thread or another:
   * whoever comes SECOND to the flag continues — the clause in place, or the callback by re-entering `loop` with
   * `k(x)`, a program at the top. The answer is written before the callback's turn at the flag, so the clause
   * that comes second reads it.
   */
  private final class Drive[A](promise: Promise[A]):
    val clause: Clause[Async, EmptyTuple, Driven[A]] = new Clause[Async, EmptyTuple, Driven[A]]:
      def apply[X](op: Async[X], k: X => Top[Driven[A]]): Top[Driven[A]] = op match
        case Async.Run(f) => k(f())
        case Async.Await(reg, _) =>
          val got = AtomicReference[Either[Throwable, X] | Null](null)
          val second = AtomicBoolean(false)
          reg { r =>
            got.set(r)
            if second.getAndSet(true) then resume(r, k)
          }: Unit
          if !second.getAndSet(true) then Cont.Return(Driven.Parked)
          else got.get.nn match
            case Right(x) => k(x)
            case Left(e) => throw e

    def loop(t: Top[Driven[A]]): Unit =
      try
        Machine.value(t) match
          case Driven.Done(a) => promise.trySuccess(a): Unit
          case Driven.Parked => ()
      catch case NonFatal(e) => promise.tryFailure(e): Unit

    /** a late answer: the rest run from the callback's frame — itself a loop until the next pending Await */
    private def resume[X](r: Either[Throwable, X], k: X => Top[Driven[A]]): Unit = r match
      case Right(x) => loop(k(x))
      case Left(e) => promise.tryFailure(e): Unit
