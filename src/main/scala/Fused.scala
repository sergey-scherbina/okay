package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * STAGE 0 of specs/handler-fusion.md: the hand-written fused loops.
 *
 * A row's continuation-aware handlers run ONE AT A TIME today —
 * `State.handle(s)(Writer.run(p))` walks the program twice, and on
 * the first walk every State operation is rebuilt (`Effect(e)
 * .flatMap(k)`: a Bind and a closure) for the second walk to find.
 * These two loops walk ONCE, with a product accumulator, and are the
 * measurement gate the spec puts before any generic `Fused.run`: if a
 * loop written by hand does not clear 1.3x over the nested runners,
 * no generic machinery is worth building on top of it.
 *
 * Nothing here is generic on purpose. The shape is `Writer.foldWith`'s
 * and `State.handle`'s — one `@tailrec` match over the three head
 * forms `resume` establishes — with both accumulators threaded through
 * the loop as VALUES. Not cells: a forwarded or captured continuation
 * must be re-runnable (multi-shot), so the state it closed over has
 * to be the state at capture time (State.handle says the same).
 *
 * The row's order is the meaning. `stateWriter` over
 * `State % S + Writer % W` answers what `Writer.run(State.handle(s)(p))`
 * answers, reshaped to the row's layout `((S, Vector[W]), A)`; with no
 * abort or choice in the row the other nesting answers the same
 * values, and TestFused asserts both. `throwsStateWriter` handles
 * Throws OUTERMOST: an abort discards the state and the log, as
 * `runEither(State.handle(s)(Writer.run(p)))` does — that is the
 * nesting whose Throws pass is pure forwarding, i.e. the one that
 * pays the most for nothing when the program never raises.
 */
object Fused {

  /** `State % S + Writer % W`, one pass; the log in order */
  def stateWriter[S, W, A](s: S)(p: A ! (State % S + Writer % W))
                          (using TypeableK[State % S]): ((S, Vector[W]), A) = {
    @tailrec def loop(s: S, w: Vector[W])(x: A ! (State % S + Writer % W)): ((S, Vector[W]), A) =
      (x.resume: @unchecked) match
        case Pure(a) => ((s, w), a)
        case Effect(e) => <|>[State % S, Writer % W](e) match
          // matching the constructor refines the answer type (State's
          // to S, Writer's to Unit), so the value answered is typed,
          // not asserted — the same refinement the runners rely on
          case Left(State.Get()) => ((s, w), s)
          case Left(State.Set(s2)) => ((s2, w), s2)
          case Right(Writer.Say(v)) => ((s, w :+ v), ())
        case Bind(Effect(e), k) => <|>[State % S, Writer % W](e) match
          case Left(State.Get()) => loop(s, w)(k(s))
          case Left(State.Set(s2)) => loop(s2, w)(k(s2))
          case Right(Writer.Say(v)) => loop(s, w :+ v)(k(()))
    loop(s, Vector.empty)(p)
  }

  /**
   * `Throws % E + State % S + Writer % W`, one pass, Throws outermost:
   * a raise ends the program with `Left(e)` and nothing else survives.
   * Tail-recursive throughout — an abort DISCARDS the continuation, so
   * it needs no shift; it is the one non-resumptive shape a tail loop
   * can still hold.
   */
  def throwsStateWriter[E, S, W, A](s: S)(p: A ! (Throws % E + State % S + Writer % W))
                                   (using TypeableK[Throws % E], TypeableK[State % S])
  : Either[E, ((S, Vector[W]), A)] = {
    type Row = Throws % E + State % S + Writer % W
    @tailrec def loop(s: S, w: Vector[W])(x: A ! Row): Either[E, ((S, Vector[W]), A)] =
      (x.resume: @unchecked) match
        case Pure(a) => Right(((s, w), a))
        // the split tests ONE signature and takes the rest by exclusion,
        // so a three-effect row is split twice, single effect first
        case Effect(e) => <|>[Throws % E, State % S + Writer % W](e) match
          case Left(Throws(err)) => Left(err)
          case Right(e) => <|>[State % S, Writer % W](e) match
            case Left(State.Get()) => Right(((s, w), s))
            case Left(State.Set(s2)) => Right(((s2, w), s2))
            case Right(Writer.Say(v)) => Right(((s, w :+ v), ()))
        case Bind(Effect(e), k) => <|>[Throws % E, State % S + Writer % W](e) match
          case Left(Throws(err)) => Left(err)
          case Right(e) => <|>[State % S, Writer % W](e) match
            case Left(State.Get()) => loop(s, w)(k(s))
            case Left(State.Set(s2)) => loop(s2, w)(k(s2))
            case Right(Writer.Say(v)) => loop(s, w :+ v)(k(()))
    loop(s, Vector.empty)(p)
  }
}
