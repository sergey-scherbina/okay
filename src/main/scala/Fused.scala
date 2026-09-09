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

  // ---- STAGE B (handler-fusion-eff): the composite handler over a
  // Control carrier, no tree between the program and its answer.
  //
  // MEASURED AND REFUTED (2026-09-09, specs/handler-fusion.md Stage B):
  // on the same 1 000-op right-nested program the Free tree walked by
  // `stateWriter` above is the FASTEST and the LEANEST of the four —
  // 13.7 us / 122 641 B/op — against the handler-passing program at
  // Func 16.0 / 184 665, at Cont 18.0 / 200 673, and `Eff` with this
  // composite 23.5 / 297 897. A Free node (Inject + Bind + one closure)
  // is cheaper than the closure pair every CPS bind allocates, and its
  // tail-recursive runner beats closure invocation. "No tree" was the
  // premise; the tree was never the cost. Kept as the measured
  // refutation, with its agreement laws (TestFused) green.

  /** the accumulator of `State % S + Writer % W`, threaded through the
   * ANSWER TYPE the way PState threads its state: the answer is a
   * function of the accumulator, get/set/tell are shifts that pass a
   * new one to the continuation, and `runEff` applies the initial one */
  type Acc[S, W] = (S, Vector[W])
  type Answer[S, W, A] = Acc[S, W] => (Acc[S, W], A)

  /**
   * The composite interpreter for `State % S + Writer % W` at any
   * Control carrier, assembled `inline`: one `split` (stage A), each
   * branch a `shift` whose captured continuation is called with the
   * new accumulator. The row's meaning is the row's order, as in the
   * Free loops. No tree exists between an `Eff` program and this: the
   * program IS the function of its handler.
   */
  inline def stateWriterInterp[C[_, _, _], S, W, A](using TypeableK[State % S])
  : Interpr[State % S + Writer % W, C, Answer[S, W, A]] =
    val C = Control[C]
    // `shift[X, …]`, not `[S, …]`: inside the branch the constructor has
    // refined X (to S, to Unit), and an abstract carrier C is invariant, so
    // the shift must be typed at X for the branches to meet at C[X, R, R]
    [X] => e => split[State % S, Writer % W](e) {
        case State.Get() => C.shift[X, Answer[S, W, A], Answer[S, W, A]](k => acc => k(acc._1)(acc))
        case State.Set(s2) => C.shift[X, Answer[S, W, A], Answer[S, W, A]](k => acc => k(s2)((s2, acc._2)))
      } { case Writer.Say(v) => C.shift[X, Answer[S, W, A], Answer[S, W, A]](k => acc => k(())((acc._1, acc._2 :+ v))) }

  /** an `Eff` program over the row, run ONCE with the composite: the
   * answer of `stateWriter` (`((S, Vector[W]), A)`), no Free tree */
  inline def runEff[S, W, A](s: S)(m: Eff[State % S + Writer % W, A])
                            (using TypeableK[State % S]): ((S, Vector[W]), A) =
    (m[Answer[S, W, A]](stateWriterInterp[Cont, S, W, A]) / (a => acc => (acc, a)))((s, Vector.empty))

  /** the same for a program written directly against a Control
   * carrier (`def prog[C](h: Interpr[Row, C, R]): C[A, R, R]`), at Func
   * or Cont: the fully fused road staged-effects.md measured */
  inline def runCtrl[C[_, _, _], S, W, A](s: S)
      (inline prog: Interpr[State % S + Writer % W, C, Answer[S, W, A]] => C[A, Answer[S, W, A], Answer[S, W, A]])
      (using TypeableK[State % S]): ((S, Vector[W]), A) =
    val C = Control[C]
    (C./(prog(stateWriterInterp[C, S, W, A]))(a => acc => (acc, a)))((s, Vector.empty))
}
