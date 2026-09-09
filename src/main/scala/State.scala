package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * The State effect: the signature is fixed at one state type S, and
 * both operations answer with the (current or new) state. For a state
 * that changes its TYPE mid-program, see PState below.
 */
/**
 * PARAMETERISED, so the derived test is by CLASS only: the operations
 * carry no runtime trace of S, and a row may therefore hold ONE
 * State. Two — `State % Int + State % String` — misroute, loudly
 * (TestRowIdentity): the first handler answers both asks and the
 * second continuation gets a ClassCastException, rather than a
 * plausible wrong answer.
 */
enum State[S, +A] derives okay.Effect {
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

  /**
   * apply f to the state. Get and set are what it is, and saying so
   * once is worth it: a `modify` spelt out is two operations with a
   * name in between that never means anything.
   *
   * It answers the NEW state, as both operations do — the file's one
   * convention, and worth keeping over the statement-shaped `Unit`
   * other libraries return: a caller who wants unit writes
   * `.map(_ => ())`, and one who wants the state would otherwise have
   * to ask for it again.
   */
  inline def modify[S](f: S => S): S ! State % S = get[S].flatMap(s => set(f(s)))

  /**
   * a transition that ANSWERS something computed from the old state:
   * `f` sees the state and returns what to answer and what to leave
   * behind.
   *
   * `modify` cannot do this. It answers the new state, and what a
   * caller usually wants back is something the write destroys — the
   * value that WAS there. Spelt out, that is get, then set the
   * modified store, then answer out of the store already read; here
   * it is one step, and the answer type is whatever `f`'s first
   * component is.
   */
  inline def update[S, B](f: S => (B, S)): B ! State % S =
    get[S].flatMap { s => val (b, next) = f(s); set(next).map(_ => b) }

  /**
   * both states — what it was and what it is.
   *
   * The three answer three different questions, and the cost is the
   * reason there are three rather than one:
   *
   *   modify(f) : S          the new state — the common case, and it
   *                          allocates nothing
   *   update(f) : B          anything computed from the OLD state, in
   *                          one pass over it: the form that answers
   *                          what the write is about to destroy
   *   swap(f)   : (S, S)     both, for when the caller wants to
   *                          compare them
   *
   * `swap` is `update` with a pair for an answer, and a pair per call
   * is why `modify` does not simply answer one: most callers want the
   * new state or nothing at all, and they should not pay for a tuple
   * to say so.
   */
  inline def swap[S](f: S => S): (S, S) ! State % S =
    update(s => { val next = f(s); ((s, next), next) })

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

    // `split`, not `<|>` (split-without-either): the two branches
    // beta-reduce into this match, no Either per operation. A
    // returning arm ascribes the loop's answer inside the branch,
    // where the constructor has refined the answer type to S.
    @tailrec def loop(s: S)(x: A ! State % S + F): (S, A) ! F = (x.resume: @unchecked) match
      case Pure(a) => Pure((s, a))
      case Effect(e) => split[State[S, *], F](e) {
          case Get() => Pure((s, s)): (S, A) ! F
          case Set(s) => Pure((s, s)): (S, A) ! F
        } { e => Effect(e).map((s, _)) }
      case Bind(Effect(e), k) => split[State[S, *], F](e) {
          case Get() => loop(s)(k(s))
          case Set(s) => loop(s)(k(s))
        } { e => Effect(e).flatMap(x => _loop(s)(k(x))) }

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

