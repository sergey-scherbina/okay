package okay2

import scala.annotation.unused

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}
import Split.split

/**
 * The State effect: the signature is fixed at one state type S, and
 * both operations answer with the (current or new) state.
 *
 * A PARAMETERISED signature is a Row CLASS — `State[S]` — with its
 * operations in the companion, so that a row reads `State[Int] +
 * Writer[String] + Produce` with no parentheses: Scala 2 gives every
 * infix type operator one precedence, so `A + B % C` would be
 * `(A + B) % C`. `State % S` is the same type, by the `%` alias.
 * The test is by CLASS only: a row may hold ONE State.
 */
sealed trait State[S] extends Row { type Op[+A] = State.Op[S, A] }

object State {
  sealed trait Op[S, +A]
  /** read the current state */
  final case class Get[S]() extends Op[S, S]
  /** replace the state, answering with the new one */
  final case class Set[S](s: S) extends Op[S, S]

  implicit def effect[S]: Effect[State[S]] = Effect.of[State[S]]

  /** the current state */
  def get[S]: S ! State[S] = Free.inject[State[S], S](Get())

  /** replace the state */
  def set[S](s: S): S ! State[S] = Free.inject[State[S], S](Set(s))

  /** apply f to the state; answers the NEW state, as both operations do */
  def modify[S](f: S => S): S ! State[S] = get[S].flatMap(s => set(f(s)))

  /** a transition that ANSWERS something computed from the old state */
  def update[S, B](f: S => (B, S)): B ! State[S] =
    get[S].flatMap { s => val (b, next) = f(s); set(next).map(_ => b) }

  /** both states — what it was and what it is */
  def swap[S](f: S => S): (S, S) ! State[S] =
    update[S, (S, S)](s => { val next = f(s); ((s, next), next) })

  /** run from an initial state to (final state, value) */
  def run[S, A](s: S)(a: Free[State[S], A]): (S, A) = Effects.run(handleAt[S, A, Pure](s)(a.plus[Pure]))

  /** the handler, for a program whose row mentions `State[S]` ANYWHERE:
   * the row is an intersection, so scalac infers the rest `R` itself
   * (stage 8) — the parameter spelled with `Free`, not `!`/`+`, which
   * scalac would not look through to solve `R` */
  def handle[S, A, R <: Row](s: S)(a: Free[State[S] with R, A])(implicit @unused d: Distinct[State[S] with R]): (S, A) ! R =
    handleAt[S, A, R](s)(a)

  /**
   * the handler at its own shape: a bespoke tail-recursive loop that
   * threads the state through itself. A forwarded F-effect suspends
   * with the current state captured immutably, which keeps the
   * residual re-runnable.
   */
  def handleAt[S, A, F <: Row](s: S)(a: Free[State[S] with F, A]): (S, A) ! F = {
    def _loop(s: S)(x: Free[State[S] with F, A]): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: Free[State[S] with F, A]): (S, A) ! F = Free.resume(x) match {
      case Return(a) => Return((s, a))
      // a lone operation is a Bind with a pure continuation (package.scala)
      case Inject(e) => loop(s)(Bind(Inject[State[S] + F, A](e), (x: A) => Return[State[S] + F, A](x)))
      case Bind(Inject(e), k) =>
        split[State[S], F, Any, Either[(S, A ! (State[S] + F)), (S, A) ! F]](e) {
          case Get() => Left((s, k(s)))
          case Set(s2) => Left((s2, k(s2)))
        } { e => Right(Inject[F, Any](e).flatMap(x => _loop(s)(k(x)))) } match {
          case Left((s2, next)) => loop(s2)(next)
          case Right(done) => done
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(s)(a)
  }

  /**
   * A program over a PART of the state, run over the whole: every `Get`
   * becomes a read of the whole through `look`, every `Set` a read, a
   * `put` and a write; the rest of the row passes through untouched.
   * Two functions rather than a lens, as in the Scala 3 core, where this
   * is what keeps the core free of optics: okay2-optics gives back the
   * lens spelling (`State.zoom(lens)(prog)`).
   */
  def zoomWith[S, A, X, R <: Row](look: S => A, put: A => S => S)(p: Free[State[A] with R, X])(implicit @unused d: Distinct[State[A] with R]): Free[State[S] with R, X] =
    zoomAt[S, A, X, R](look, put)(p)

  /** `zoomWith` at its own shape, the rest of the row named */
  def zoomAt[S, A, X, F <: Row](look: S => A, put: A => S => S)(p: Free[State[A] with F, X]): Free[State[S] with F, X] = {
    def readPart: Free[State[S] with F, A] = get[S].map(look)
    def writePart(a: A): Free[State[S] with F, A] = get[S].flatMap(s => set(put(a)(s))).map(_ => a)

    def loop(x: Free[State[A] with F, X]): Free[State[S] with F, X] = Free.resume(x) match {
      case Return(v) => Return(v)
      // a lone operation is a Bind with a pure continuation (package.scala)
      case Inject(e) => loop(Bind(Inject[State[A] + F, X](e), (v: X) => Return[State[A] + F, X](v)))
      case Bind(Inject(e), k) =>
        split[State[A], F, Any, Free[State[S] with F, X]](e) {
          case Get() => readPart.flatMap(a => loop(k(a)))
          case Set(a) => writePart(a).flatMap(v => loop(k(v)))
        } { e => Inject[F, Any](e).flatMap(v => loop(k(v))) }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(p)
  }

  /** number the elements of a sequence, as a State program */
  def index[A](seq: Seq[A], from: Long = 0): (Long, Seq[(Long, A)]) = run(from) {
    seq.foldLeft(pure[State[Long], Seq[(Long, A)]](Seq.empty)) { (c, a) =>
      for { xs <- c; n <- get[Long]; _ <- set(n + 1) } yield (n, a) +: xs
    }
  }
}

/**
 * Parameterised (type-changing) state, founded on the continuation
 * monad: a computation of A that changes the state TYPE from S to S2,
 * with the final answer R, is Cont[A, S2 => R, S => R] — the state is
 * threaded by the answer type, get and set are shifts, and Cont's
 * flatMap composes the transitions (typestate: the compiler enforces
 * the protocol order).
 */
object PState {
  /** read the state, leaving its type unchanged */
  def get[S, R]: Cont[S, S => R, S => R] = shift[S, S => R, S => R](k => s => k(s)(s))

  /** write a state of a possibly different type; the old state is the value */
  def set[S, S2, R](s2: S2): Cont[S, S2 => R, S => R] = shift[S, S2 => R, S => R](k => s => k(s)(s2))

  /** a typestate transition read as a two-parameter carrier in its
   * state, `L[A, B] = Cont[X, B => R, A => R]` — what okay2-optics
   * gives a `Strong` instance, so a lens zooms it (the Scala 3 core's
   * `PState.Zooming`, a type lambda there) */
  type Zooming[X, R] = { type L[A, B] = Cont[X, B => R, A => R] }

  /** run from an initial state to (final state, value) */
  def run[S, S2, A](s: S)(m: Cont[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / ((a: A) => (s2: S2) => (s2, a)))(s)
}
