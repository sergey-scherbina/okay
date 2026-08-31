package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * The State effect: the signature is fixed at one state type S, and
 * both operations answer with the (current or new) state. For a state
 * that changes its TYPE mid-program, see PState below.
 */
enum State[S, +A] {
  /** read the current state */
  case Get() extends State[S, S]

  /** replace the state, answering with the new one */
  case Set(s: S) extends State[S, S]
}

/** a value as a stateful computation */
extension [A](a: A)
  inline def state[S]: A ! State % S = pure(a)

object State {
  /** the current state */
  inline def get[S]: S ! State % S = effect(Get())

  /** replace the state */
  inline def set[S](s: S): S ! State % S = effect(Set(s))

  /** run from an initial state to (final state, value) */
  inline def run[S, A](s: S)(a: A ! State % S): (S, A) = !.run(handle(s)(a))

  /**
   * the handler: a bespoke tail-recursive loop that threads the state
   * through itself. It cannot be a relay handler — the answer-
   * polymorphic ∀Y shape has nowhere to hold s — and a mutable cell
   * would break the purity of the residual tree, so the loop is the
   * honest form. A forwarded F-effect suspends with the current state
   * captured immutably, which keeps the residual re-runnable.
   */
  def handle[S, A, F[+_]](s: S)(a: A ! State % S + F): (S, A) ! F = {
    def _loop(s: S)(x: A ! State % S + F): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = (x.resume: @unchecked) match
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

  /** number the elements of a sequence, as a State program */
  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from):
    seq.foldLeft(Seq[(Long, A)]().state[Long]): (c, a) =>
      for xs <- c; n <- get; _ <- set(n + 1) yield (n, a) +: xs
}

/**
 * Parameterised (type-changing) state, founded on the continuation
 * paramonad: a computation of A that changes the state TYPE from S to
 * S2, with the final answer R, is Cont[A, S2 => R, S => R] — the state
 * is threaded by the answer type, get and set are shifts, and Cont's
 * flatMap already composes the transitions S -> S2 -> S3 (typestate:
 * the compiler enforces the protocol order). Unlike the State effect
 * above, whose handler loop is tail-recursive, running costs a stack
 * frame per operation, and it measures ~1.7x slower on the same
 * workload (HandlerBenchmark) — the typed protocol is what you buy.
 */
object PState {
  /** read the state, leaving its type unchanged */
  inline def get[S, R]: Cont[S, S => R, S => R] = shift(k => s => k(s)(s))

  /** write a state of a possibly different type; the old state is the value */
  inline def set[S, S2, R](s2: S2): Cont[S, S2 => R, S => R] = shift(k => s => k(s)(s2))

  /** run from an initial state to (final state, value) */
  inline def run[S, S2, A](s: S)(m: Cont[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / (a => s2 => (s2, a)))(s)
}

/** by class only: `Get()`/`Set(s)` carry no trace of S in the type,
 * so a row may hold ONE State — see typeableKByClass */
given stateK[S]: TypeableK[State % S] = typeableKByClass(classOf[State[?, ?]])
