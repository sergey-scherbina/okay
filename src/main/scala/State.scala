package okay

import scala.annotation.tailrec
import okay.!.*

enum State[S, +A] {
  case Get() extends State[S, S]
  case Set(s: S) extends State[S, S]
}

extension [A](a: A)
  def state[S]: A ! State % S = pure(a)

object State {
  inline def get[S]: S ! State % S = effect(Get())
  inline def set[S](s: S): S ! State % S = effect(Set(s))
  inline def run[S, A](s: S)(a: A ! State % S): (S, A) = !.run(handle(s)(a))

  def handle[S, A, F[+_]](s: S)(a: A ! State % S + F): (S, A) ! F = {
    def _loop(s: S)(x: A ! State % S + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = x.resume match
      case Pure(a) => Pure((s, a))
      case Effect(e) => <|>[State[S, *], F](e) match
        case Left(Get()) => Pure((s, s))
        case Left(Set(s)) => Pure((s, s))
        case Right(e) => Effect(e).map((s, _))
      case Bind(Effect(e), k) => <|>[State[S, *], F](e) match
        case Left(Get()) => loop(s)(k(s))
        case Left(Set(s)) => loop(s)(k(s))
        case Right(e) => Effect(e).flatMap(x => _loop(s)(k(x)))

    loop(s)(a)
  }

  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get; _ <- set(n + 1) yield (n, a) +: xs
}

/**
 * Parameterised (type-changing) state, founded on the continuation
 * paramonad: a computation of A that changes the state TYPE from S to
 * S2, with the final answer R, is Cont[A, S2 => R, S => R] — the state
 * is threaded by the answer type, get and set are shifts, and Cont's
 * flatMap already composes the transitions S -> S2 -> S3 (typestate).
 * Unlike the State effect above, whose handler loop is tail-recursive,
 * running costs a stack frame per operation — for typed protocols,
 * not for long loops.
 */
object PState {
  /** read the state, leaving its type unchanged */
  inline def get[S, R]: Cont[S, S => R, S => R] = shift(k => s => k(s)(s))

  /** write a state of a possibly different type; the old state is the value */
  inline def set[S, S2, R](s2: S2): Cont[S, S2 => R, S => R] = shift(k => s => k(s)(s2))

  /** run from an initial state to (final state, value) */
  def run[S, S2, A](s: S)(m: Cont[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / (a => s2 => (s2, a)))(s)
}
