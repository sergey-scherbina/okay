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

  /** `State % S + Writer % W`, one pass; the log in order. Over
   * `split` (stage A, split-without-either): no Either and no Option
   * per operation — the `<|>` form it replaced is in history.tsv as
   * `fusedSWr` at 149 312 B/op against this one's 122 641. */
  def stateWriter[S, W, A](s: S)(p: A ! (State % S + Writer % W))
                               (using TypeableK[State % S]): ((S, Vector[W]), A) = {
    @tailrec def loop(s: S, w: Vector[W])(x: A ! (State % S + Writer % W)): ((S, Vector[W]), A) =
      (x.resume: @unchecked) match
        case Pure(a) => ((s, w), a)
        // a RETURNING arm ascribes the outer answer inside the branch:
        // the constructor refines A (to S, to Unit) in there, and the
        // ascription is where the refined value meets the loop's type —
        // otherwise R is inferred from the branches as `S | Unit`
        case Effect(e) => split[State % S, Writer % W](e) {
            case State.Get() => ((s, w), s): ((S, Vector[W]), A)
            case State.Set(s2) => ((s2, w), s2): ((S, Vector[W]), A)
          } { case Writer.Say(v) => ((s, w :+ v), ()): ((S, Vector[W]), A) }
        case Bind(Effect(e), k) => split[State % S, Writer % W](e) {
            case State.Get() => loop(s, w)(k(s))
            case State.Set(s2) => loop(s2, w)(k(s2))
          } { w0 =>
            // `Say` is Writer's ONLY constructor, but under a Bind the
            // answer type is existential and the exhaustivity checker
            // reports the match incomplete on `Say(_)` itself — the
            // same claim `resume`'s `@unchecked` makes, at the same
            // kind of site
            (w0: @unchecked) match
              case Writer.Say(v) => loop(s, w :+ v)(k(()))
          }
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
        case Effect(e) => split[Throws % E, State % S + Writer % W](e) {
            case Throws(err) => Left(err): Either[E, ((S, Vector[W]), A)]
          } { e => split[State % S, Writer % W](e) {
            case State.Get() => Right(((s, w), s)): Either[E, ((S, Vector[W]), A)]
            case State.Set(s2) => Right(((s2, w), s2)): Either[E, ((S, Vector[W]), A)]
          } { case Writer.Say(v) => Right(((s, w :+ v), ())): Either[E, ((S, Vector[W]), A)] } }
        case Bind(Effect(e), k) => split[Throws % E, State % S + Writer % W](e) {
            case Throws(err) => Left(err)
          } { e => split[State % S, Writer % W](e) {
            case State.Get() => loop(s, w)(k(s))
            case State.Set(s2) => loop(s2, w)(k(s2))
          } { w0 => (w0: @unchecked) match
                case Writer.Say(v) => loop(s, w :+ v)(k(())) } }
    loop(s, Vector.empty)(p)
  }
}
