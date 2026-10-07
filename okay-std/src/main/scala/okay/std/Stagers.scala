package okay.std

import okay.freer.*
import okay.freer.given

import okay.freer.Stager.{Acc, Answer}

/** the stagers of okay-std's effects (were `Stager.StateWriter`, `Stager.All`) */
object Stagers:
  /** the canonical stage: `State % S + Writer % W`, the row every
   * fusion number in this repository was taken on. A user's row is an
   * object of this shape — the arms are the handler's, one per
   * constructor, with the accumulator threaded as a value (never a
   * cell: a captured continuation must be re-runnable). */
  final class StateWriter[S, W, A] extends Stager[State % S + Writer % W, Answer[S, W, A]]:
    type Row = State % S + Writer % W
    type R = Answer[S, W, A]

    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case State.Get() => Handled.shift[Row, R, X](k => st => k(st._1)(st))
        case State.Update(f) => Handled.shift[Row, R, X](k => st => { val (b, s2) = f(st._1); k(b)((s2, st._2)) })
        case Writer.Say(v) => Handled.shift[Row, R, X](k => st => k(())((st._1, st._2 :+ v)))

    /** run a block from an initial state: the state and the log in
     * the row's layout, then the answer */
    def run(s: S)(p: Handled[Row, R, A]): ((S, Vector[W]), A) =
      Handled.run(p)(a => acc => (acc, a))((s, Vector.empty))

  /** specs/direct-stagers.md: every effect a staged block can hold,
   * in ONE layout — the environment a plain argument (it never
   * changes), the state and the log threaded as a value, the error as
   * the answer's Left. A subrow instantiates the slots it does not
   * use with `Unit` (a state or environment never read) and `Nothing`
   * (a log or error nothing constructs): the arms for those members
   * are in the match and never chosen. Reader + State + Writer +
   * Throws is the whole of what a direct block is written over when
   * it is not Async — Async is a suspension, not an arm. */
  final class All[E, S, W, Err, A] extends Stager[Reader % E + State % S + Writer % W + Throws % Err, All.Answer[E, S, W, Err, A]]:
    type Row = Reader % E + State % S + Writer % W + Throws % Err
    type R = All.Answer[E, S, W, Err, A]

    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Reader.Ask() => Handled.shift[Row, R, X](k => env => acc => k(env)(env)(acc))
        case Reader.Asks(f) => Handled.shift[Row, R, X](k => env => acc => k(f(env))(env)(acc))
        case State.Get() => Handled.shift[Row, R, X](k => env => acc => k(acc._1)(env)(acc))
        case State.Update(f) => Handled.shift[Row, R, X](k => env => acc => { val (b, s2) = f(acc._1); k(b)(env)((s2, acc._2)) })
        case Writer.Say(v) => Handled.shift[Row, R, X](k => env => acc => k(())(env)((acc._1, acc._2 :+ v)))
        // the early end: the continuation is dropped, the answer is the error
        case Throws(e) => Handled.shift[Row, R, X](_ => _ => acc => (acc, Left(e)))

    /** run a block in an environment from an initial state: the state
     * and the log at the end — or at the raise — then the answer or
     * the error; `run` IS the block's catch */
    def run(env: E, s: S)(p: Handled[Row, R, A]): ((S, Vector[W]), Either[Err, A]) =
      Handled.run(p)(a => _ => acc => (acc, Right(a)))(env)((s, Vector.empty))

  object All:
    type Answer[E, S, W, Err, A] = E => Acc[S, W] => (Acc[S, W], Either[Err, A])

  // ---- the singles: the same arms with the tuple removed (a
  // one-effect block pays for one effect; the benchmark says how much
  // the tuple was — specs/direct-stagers.md, Results)

  final class Reading[E, A] extends Stager[Reader % E, E => A]:
    type Row = Reader % E
    type R = E => A
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Reader.Ask() => Handled.shift[Row, R, X](k => env => k(env)(env))
        case Reader.Asks(f) => Handled.shift[Row, R, X](k => env => k(f(env))(env))
    def run(env: E)(p: Handled[Row, R, A]): A = Handled.run(p)(a => _ => a)(env)

  final class Stateful[S, A] extends Stager[State % S, S => (S, A)]:
    type Row = State % S
    type R = S => (S, A)
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case State.Get() => Handled.shift[Row, R, X](k => s => k(s)(s))
        case State.Update(f) => Handled.shift[Row, R, X](k => s => { val (b, s2) = f(s); k(b)(s2) })
    def run(s: S)(p: Handled[Row, R, A]): (S, A) = Handled.run(p)(a => s => (s, a))(s)

  final class Logging[W, A] extends Stager[Writer % W, Vector[W] => (Vector[W], A)]:
    type Row = Writer % W
    type R = Vector[W] => (Vector[W], A)
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Writer.Say(v) => Handled.shift[Row, R, X](k => log => k(())(log :+ v))
    def run(p: Handled[Row, R, A]): (Vector[W], A) = Handled.run(p)(a => log => (log, a))(Vector.empty)

  final class Failing[Err, A] extends Stager[Throws % Err, Either[Err, A]]:
    type Row = Throws % Err
    type R = Either[Err, A]
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X] =
      inline op match
        case Throws(e) => Handled.shift[Row, R, X](_ => Left(e))
    def run(p: Handled[Row, R, A]): Either[Err, A] = Handled.run(p)(Right(_))
