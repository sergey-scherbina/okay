package okay.foreign

import okay.{!, %, +, Row as OkRow, Take, Writer, effect, pure, split}
import okay.!.*
import okay.codec.Schema
import scala.annotation.tailrec

/**
 * A far-side object a program HOLDS until the program's scope ends
 * (foreign-source-early-stop): a source's iterator. `Hold` when it is taken,
 * `Let` when its owner released it itself; `Foreign.releasing` releases, at
 * the end, whatever is still held — what a consumer that stopped early left.
 */
enum Holding[+A] derives okay.Effect:
  case Hold(ref: Handle) extends Holding[Unit]
  case Let(ref: Handle) extends Holding[Unit]
  /** a stream the far side drives, open until it ends or is let go; the
   * scope CANCELS what is still open (foreign-mux-duplex part 3) */
  case HoldStream(stream: Long) extends Holding[Unit]
  case LetStream(stream: Long) extends Holding[Unit]

/**
 * A Python function, or a method of a held object, as an okay STAGE over
 * chunks (foreign-streaming, specs/foreign-highlevel.md stage 6):
 *
 * {{{
 * through(source.plus[ForeignEval])(Foreign.stage[Double, Double]("model:predict", chunk = 256))
 * }}}
 *
 * The stage pulls up to `chunk` elements, calls the function ONCE with
 * them as a list, and tells every element of the list it answers — any
 * length, so it filters or expands as well as maps. The next pull happens
 * after the answer: a slow model back-pressures its source, and one
 * message crosses per chunk, not per element. The input's end flushes a
 * partial chunk.
 *
 * STATEFUL transformations are a held object (foreign-object-handles):
 * `ref.stage[I, O]("step", finish = Some("flush"))` calls `step` per chunk
 * and `flush` once at the end, for what the object still holds.
 *
 * Pull-driven by design rather than a Python generator running beside
 * okay: a generator suspended on its input would need a second thread in
 * the shim, and a stage that owns the pull is what okay's `through`
 * already composes.
 */
object PyStream:

  /** the row of a Python stage: okay's stage row, plus the calls */
  type Row[I, O] = Take % I + (Writer % O + ForeignEval)

  /** the row of a far-side SOURCE: its elements told, plus the calls, plus
   * the iterator it holds — so a source runs only inside
   * `Foreign.releasing`, which gives the iterator back however the
   * consumer ended (foreign-source-early-stop) */
  type SourceRow[O] = Writer % O + (ForeignEval + Holding)

  /** a source's row once its scope has released what it held */
  type Released[O] = Writer % O + ForeignEval

  /**
   * THE SCOPE of a program holding far-side objects (foreign-source-early-stop):
   * the program runs, and when it ends every object it still holds is
   * released — by the same handler as every other call, so a pool routes it,
   * a supervisor sees it and a journal records it. It exists because a
   * consumer that stops early (`through` with a stage that has had enough)
   * simply drops the source's residual: nothing in the source runs again,
   * so only a scope around the whole consumption can know it is over. A
   * JVM exception ending the program releases nothing: a release is a call,
   * and a program that threw has no next step to make it.
   */
  def releasing[A, O](p: A ! SourceRow[O]): A ! Released[O] =
    holding[A, Released[O]](p)

  /** the walk behind `releasing`, over any row that holds and calls */
  private[okay] def holding[A, F[+_]](p: A ! Holding + F)(using ev: OkRow.Sub[ForeignEval, F]): A ! F =
    // what is held: refs to release, streams to cancel
    final case class Held(refs: List[Handle], streams: List[Long]):
      def apply(h: Holding[?]): Held = h match
        case Holding.Hold(r) => copy(refs = r :: refs)
        case Holding.Let(r) => copy(refs = refs.filterNot(_ == r))
        case Holding.HoldStream(s) => copy(streams = s :: streams)
        case Holding.LetStream(s) => copy(streams = streams.filterNot(_ == s))
    def perform(op: ForeignEval[Any]): Unit ! F = effect[F, Any](ev(op)).map(_ => ())
    def releaseAll(held: Held): Unit ! F =
      val ops = held.streams.map(ForeignEval.Cancel(_)) ++ held.refs.map(ForeignEval.Release(_))
      okay.!.each(ops)(op => perform(op))
    def again(d: Int)(held: Held)(x: A ! Holding + F): A ! F = loop(d)(held)(x)
    // the walk as a frame of the machine (handle-frames-loops): what is held is its state, released at the end
    def frame(held: Held)(x: A ! Holding + F): okay.Shift.U[F, A] =
      okay.HandleFrames.stateful[Holding, Held, A, A, F](summon[okay.TypeableK[Holding]], (held, a) => releaseAll(held).map(_ => a))(
        (held, op, resume) => resume(held(op.asInstanceOf[Holding[?]]), ()))(held, x)
    @tailrec def loop(d: Int)(held: Held)(x: A ! Holding + F): A ! F =
      (x.resumeRun: @unchecked) match
        case Return(a) => releaseAll(held).map(_ => a)
        case Inject(e) => split[Holding, F](e) {
            case h @ Holding.Hold(_) => releaseAll(held(h)).map(_ => ()): A ! F
            case h @ Holding.Let(_) => releaseAll(held(h)): A ! F
            case h @ Holding.HoldStream(_) => releaseAll(held(h)): A ! F
            case h @ Holding.LetStream(_) => releaseAll(held(h)): A ! F
          } { e => Inject(e).flatMap(a => releaseAll(held).map(_ => a)) }
        case Bind(Inject(e), k) => split[Holding, F](e) {
            case h @ Holding.Hold(_) => loop(d)(held(h))(k(()))
            case h @ Holding.Let(_) => loop(d)(held(h))(k(()))
            case h @ Holding.HoldStream(_) => loop(d)(held(h))(k(()))
            case h @ Holding.LetStream(_) => loop(d)(held(h))(k(()))
          } { e => Inject(e).flatMap(y => again(d)(held)(k(y))) }
        case y => loop(d)(held)(okay.HandleFrames.shallow(y, d))
    okay.HandleFrames.run[A, F](d => loop(d)(Held(Nil, Nil))(p), frame(Held(Nil, Nil))(p))

  /**
   * A STREAM THE FAR SIDE DRIVES (foreign-mux-duplex part 3): `fn` of `args`
   * sends chunks of `O` (lists) from its own code as it makes them, up to
   * `credit` ahead of what this side has taken — the far side runs ahead, a
   * cursor fills its buffer while the consumer works, and the credit bounds
   * how much of that waits in memory. Its end, or a failure by name, ends
   * the source; a consumer that stops early cancels it, through the scope
   * (`releasing`) every source runs in. Needs a far side on a multiplexed
   * wire (Go, Rust); elsewhere the stream is refused by name.
   */
  private[okay] def driven[O: Schema](fn: String, args: Vector[Value], credit: Int, id: () => Long,
                                      input: Option[Iterator[Value]] = None)
                                     (using shape: Shape): Unit ! SourceRow[O] =
    type R = SourceRow[O]
    require(credit >= 1, "a stream's credit is at least one chunk")
    def tellAll(os: Vector[O]): Unit ! R =
      okay.!.each(os)(o => effect[R, Unit](Writer(o)))
    def loop(s: Long): Unit ! R =
      effect[R, Either[Condition, Option[Value]]](ForeignEval.Pull(s)).flatMap {
        case Left(c) => effect[R, Unit](Holding.LetStream(s)).flatMap(_ => throw Failed(c))
        case Right(None) => effect[R, Unit](Holding.LetStream(s))
        case Right(Some(v)) => shape.decode[Vector[O]](v) match
          case Left(c) => effect[R, Unit](Holding.LetStream(s)).flatMap(_ => throw Failed(c))
          case Right(os) => tellAll(os).flatMap(_ => loop(s))
      }
    pure[R, Unit](()).flatMap { _ =>
      val s = id()
      effect[R, Either[Condition, Unit]](ForeignEval.Stream(s, fn, args, credit, input)).flatMap {
        case Left(c) => throw Failed(c)
        case Right(()) => effect[R, Unit](Holding.HoldStream(s)).flatMap(_ => loop(s))
      }
    }

  /**
   * A FAR-SIDE SOURCE (foreign-one-mux, specs/foreign-one.md stage 5): an
   * iterator the far side OWNS — reading a file, a cursor, a generator —
   * pulled one chunk at a time, with no operation of its own on the wire:
   * `open` holds the iterator (a `call … held`), `next` is a call per chunk,
   * and `release` drops it. The next chunk is asked for when the last has
   * been told downstream, so a slow consumer back-pressures the far side,
   * and neither side holds more than a chunk. `ended` says which refusal is
   * the iterator's END (Python's `StopIteration`) and `empty` which answer
   * is (R's NULL); any other refusal ends the source naming it. The handle
   * is released on the end and on a failure; a consumer that stops pulling
   * early leaves it held until its worker ends (stateful-early-stop).
   */
  private[okay] def pulled[O: Schema](open: ForeignEval[Either[Condition, Value]],
                                      next: Handle => ForeignEval[Either[Condition, Value]],
                                      ended: Condition => Boolean,
                                      empty: Value => Boolean)(using shape: Shape): Unit ! SourceRow[O] =
    type R = SourceRow[O]
    // released by the source itself at its end or its failure; `Let` first,
    // so the scope does not release it a second time
    def release(r: Handle): Unit ! R =
      effect[R, Unit](Holding.Let(r)).flatMap(_ => effect[R, Unit](ForeignEval.Release(r)))
    def tellAll(os: Vector[O]): Unit ! R =
      okay.!.each(os)(o => effect[R, Unit](Writer(o)))
    def loop(r: Handle): Unit ! R =
      effect[R, Either[Condition, Value]](next(r)).flatMap {
        case Left(c) if ended(c) => release(r)
        case Left(c) => release(r).flatMap(_ => throw Failed(c))
        case Right(v) if empty(v) => release(r)
        case Right(v) => shape.decode[Vector[O]](v) match
          case Left(c) => release(r).flatMap(_ => throw Failed(c))
          case Right(os) => tellAll(os).flatMap(_ => loop(r))
      }
    effect[R, Either[Condition, Value]](open).flatMap {
      case Right(Value.Ref(r)) => effect[R, Unit](Holding.Hold(r)).flatMap(_ => loop(r))
      case Right(other) => throw Failed(Condition("WireError", s"a source's open answered $other, not a held iterator"))
      case Left(c) => throw Failed(c)
    }

  /** a call answered a condition: the stage cannot tell a `Left`, so it
   * ends, naming what Python said */
  final class Failed(val condition: Condition)
    extends RuntimeException(s"okay.foreign stage: ${condition.kind}: ${condition.message}")

  private[okay] def chunked[I: ToPy, O: Schema](
      chunk: Int,
      call: Vector[Value] => ForeignEval[Either[Condition, Value]],
      finish: Option[ForeignEval[Either[Condition, Value]]])(using shape: Shape): Unit ! Row[I, O] =
    require(chunk >= 1, "okay.foreign stage: a chunk holds at least one element")
    type R = Row[I, O]

    def tellAll(answer: Either[Condition, Value]): Unit ! R =
      answer.flatMap(shape.decode[Vector[O]](_)) match
        case Left(c) => throw Failed(c)
        case Right(os) => okay.!.each(os)(o => effect[R, Unit](Writer(o)))

    def end: Unit ! R = finish match
      case None => pure(())
      case Some(op) => effect[R, Either[Condition, Value]](op).flatMap(tellAll)

    def flush(buf: Vector[Value], more: Boolean): Unit ! R =
      effect[R, Either[Condition, Value]](call(buf)).flatMap(tellAll)
        .flatMap(_ => if more then fill(Vector.empty) else end)

    def fill(buf: Vector[Value]): Unit ! R =
      if buf.size == chunk then flush(buf, more = true)
      else effect[R, Option[I]](Take.Await()).flatMap {
        case Some(i) => fill(buf :+ ToPy(i))
        case None => if buf.isEmpty then end else flush(buf, more = false)
      }

    fill(Vector.empty)
