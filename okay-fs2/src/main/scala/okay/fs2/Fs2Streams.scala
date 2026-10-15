package okay.fs2

import okay.{!, %, +, Async, Source, Stage, Take, Walk}
import okay.std.Writer
import _root_.cats.effect.IO
import _root_.cats.effect.std.Queue
import _root_.cats.effect.unsafe.IORuntime
import _root_.fs2.{Chunk, Pull, Stream}

/**
 * EFFECTFUL okay streams in fs2 and back (specs/fs2-effectful.md).
 * Fs2Interop.scala crosses a PURE `Chunks` out and an IO stream in as
 * `Chunks`; this crosses the asynchronous shapes: a `Source` (a program
 * that tells its elements and awaits between them), a `Stage` (one that
 * also asks for input), and an fs2 pipe over a `Source`.
 */
object Fs2Streams:

  /**
   * A `Source` as an fs2 stream in ANY cats-effect `F` — `IO`, or an
   * okay program itself (`CatsEffect.Program`, okay-cats). A tell is an
   * element; an `Async.Run` is `F.delay`; an `Async.Await` is `F.async`
   * whose finalizer is the operation's canceller, so an interrupted fs2
   * stream unregisters what the source was waiting on. Lazy, and
   * stack-safe: the walk continues inside `Pull.flatMap`, which fs2 runs.
   */
  def toFs2[F[_], W](s: Source[W])(using F: _root_.cats.effect.kernel.Async[F]): Stream[F, W] =
    // the walk as okay-cats' `Walking`: the Writer walked among the source's effects, the Async operations met on
    // the way walked outside it
    type Row = Writer % W + Async
    def go(p: Source[W]): Pull[F, W, Unit] = outer(Walk.view(Walk.next[Writer % W, Unit, Row](p)))
    def outer(st: Walk.Step[Async, Walk.Step[Writer % W, Unit, Row], Async]): Pull[F, W, Unit] = st match
      case Walk.Done(inner) => inner match
        case Walk.Done(_) => Pull.done
        case o: Walk.Op[Writer % W, x, Unit, Row] => tell(o)
      case o: Walk.Op[Async, x, Walk.Step[Writer % W, Unit, Row], Async] =>
        async(o.op).flatMap(x => outer(Walk.view(o.resume(x))))
    // matching the constructor gives the told value and the answer type Unit, without a cast
    def tell[X](o: Walk.Op[Writer % W, X, Unit, Row]): Pull[F, W, Unit] = (o.op: Writer[W, X]) match
      case Writer.Say(w) => Pull.output1(w).flatMap(_ => go(o.resume(())))
    def async[X](e: Async[X]): Pull[F, W, X] = e match
      case Async.Run(f) => Pull.eval(F.delay(f()))
      case Async.Await(register, _) =>
        Pull.eval(F.async[X](cb => F.delay { val cancel = register(cb); Some(F.delay(cancel())) }))
    Stream.suspend(go(s).stream)

  /**
   * An fs2 IO stream as a `Source`, parking NO thread: the fs2 side
   * offers chunks into a bounded queue on its own fiber (offer suspends
   * that fiber when the queue is full — fs2's backpressure, untouched),
   * and the okay side takes by an `Await` on the take. The fs2 fiber is
   * held in a `CancelScope`, so cancelling the okay side, or leaving it
   * early, cancels the fs2 stream too.
   */
  def fromFs2[A](s: Stream[IO, A], capacity: Int = 64)(using IORuntime): Source[A] =
    def go(q: Queue[IO, Option[Chunk[A]]]): Source[A] =
      awaitIO(q.take).at[Writer % A + Async].flatMap {
        case None => okay.pure[Unit, Writer % A + Async](())
        case Some(ch) => tellAll(ch.toList).flatMap(_ => go(q))
      }
    def tellAll(xs: List[A]): Unit ! Writer % A + Async = xs match
      case Nil => okay.pure(())
      case x :: rest => Writer.tell(x).at[Writer % A + Async].flatMap(_ => tellAll(rest))
    for
      q <- awaitIO(Queue.bounded[IO, Option[Chunk[A]]](capacity)).at[Writer % A + Async]
      fib <- awaitIO(s.chunks.evalMap(c => q.offer(Some(c))).compile.drain.guarantee(q.offer(None)).start)
        .at[Writer % A + Async]
      scope = Async.CancelScope(() => fib.cancel.unsafeRunAndForget())
      _ <- scope.enter.at[Writer % A + Async]
      _ <- go(q)
      _ <- scope.exit.at[Writer % A + Async]
    yield ()

  /** an IO as one `Async.Await` on its future, cancelling the IO when
   * the operation is cancelled */
  private def awaitIO[X](io: IO[X])(using rt: IORuntime): X ! Async =
    Async.await[X] { k =>
      val (done, cancel) = io.unsafeToFutureCancelable()
      done.onComplete(t => k(t.toEither))(using scala.concurrent.ExecutionContext.parasitic)
      () => { val _ = cancel() }
    }

  /**
   * A `Stage` as an fs2 `Pipe`, in any `F`: `Take.Await` pulls ONE
   * element from the input (`uncons1`), so the stage reads exactly as
   * much as it asks for — an infinite input is fine — and a tell is an
   * output element. Stack-safe for the reason `toFs2` is.
   */
  def toPipe[F[_], I, O](st: Stage[I, O, Unit]): _root_.fs2.Pipe[F, I, O] = in =>
    // walked as `toFs2` is: the Take among the stage's effects, the Writer met on the way outside it
    type Row = Take % I + Writer % O
    def go(p: Stage[I, O, Unit], in: Stream[F, I]): Pull[F, O, Unit] = outer(Walk.view(Walk.next[Take % I, Unit, Row](p)), in)
    def outer(s: Walk.Step[Writer % O, Walk.Step[Take % I, Unit, Row], Writer % O], in: Stream[F, I]): Pull[F, O, Unit] =
      s match
        case Walk.Done(inner) => inner match
          case Walk.Done(_) => Pull.done
          case o: Walk.Op[Take % I, x, Unit, Row] => o.op match
            case Take.Await() => in.pull.uncons1.flatMap {
              case Some((i, rest)) => go(o.resume(Some(i)), rest)
              case None => go(o.resume(None), Stream.empty)
            }
        case o: Walk.Op[Writer % O, x, Walk.Step[Take % I, Unit, Row], Writer % O] => tell(o, in)
    def tell[X](o: Walk.Op[Writer % O, X, Walk.Step[Take % I, Unit, Row], Writer % O], in: Stream[F, I]): Pull[F, O, Unit] =
      (o.op: Writer[O, X]) match
        case Writer.Say(w) => Pull.output1(w).flatMap(_ => outer(Walk.view(o.resume(())), in))
    Stream.suspend(go(st, in).stream)

  /** an fs2 pipe in an okay pipeline: the `Source` out to fs2, through
   * the pipe, and back as a `Source` */
  def through[I, O](src: Source[I])(pipe: _root_.fs2.Pipe[IO, I, O])(using IORuntime): Source[O] =
    fromFs2(pipe(toFs2[IO, I](src)))
