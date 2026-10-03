package okay2

import scala.annotation.unused

import scala.annotation.tailrec
import Free.{Return, Inject, Bind}

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
  /** transition the state, answering from the OLD one: `f` gives the answer and the new state. Every write is
   * this (state-get-update, the Scala 3 core's twin): `set` and `modify` build one, so the signature is two
   * operations */
  final case class Update[S, B](f: S => (B, S)) extends Op[S, B]

  /** `set`'s transition, as DATA: two sets of one value are equal operations, and it reads as `Update(Put(5))`
   * where a lambda would print its address */
  final case class Put[S](s: S) extends (S => (S, S)) {
    def apply(old: S): (S, S) = (s, s)
    // Function1's own toString would win over the case class's
    override def toString: String = s"Put($s)"
  }

  /** `modify`'s transition, as data: equal when its function is the same one */
  final case class Modified[S](f: S => S) extends (S => (S, S)) {
    def apply(old: S): (S, S) = { val n = f(old); (n, n) }
    override def toString: String = s"Modified($f)"
  }

  implicit def effect[S]: Effect[State[S]] = Effect.of[State[S]]

  /** the current state */
  def get[S]: S ! State[S] = Free.inject[State[S], S](Get())

  /** replace the state, answering with the new one */
  def set[S](s: S): S ! State[S] = Free.inject[State[S], S](Update[S, S](Put(s)))

  /** apply f to the state, as ONE operation; answers the NEW state */
  def modify[S](f: S => S): S ! State[S] = Free.inject[State[S], S](Update[S, S](Modified(f)))

  /** a transition that ANSWERS something computed from the old state, as one operation */
  def update[S, B](f: S => (B, S)): B ! State[S] = Free.inject[State[S], B](Update(f))

  /** both states — what it was and what it is */
  def swap[S](f: S => S): (S, S) ! State[S] =
    update[S, (S, S)](s => { val next = f(s); ((s, next), next) })

  /** the handler as a value, level 1: `p.handle(State(0))` answers `(final state, value)` */
  def apply[S](s: S): Handler[State[S], Handler.Pair[S]#L] = new Handler.Full[State[S], Any, Handler.Pair[S]#L, Handler.Nothing] {
    def run[A, F <: Row](p: Free[State[S] with F, A])(implicit @unused ev: A <:< Any, @unused d: Distinct[State[S] with F], @unused n: Handler.Nothing[F]): (S, A) ! F =
      handleAt[S, A, F](s)(p)
  }

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
    // the split as a pattern: a State operation is the loop's own tail
    // call, with nothing allocated for the step (okay2-handler-allocs)
    val Mine = Split.at[State[S]]

    def _loop(s: S)(x: Free[State[S] with F, A]): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: Free[State[S] with F, A]): (S, A) ! F = Free.resume(x) match {
      case Return(a) => Return((s, a))
      // a lone operation is a Bind with a pure continuation (package.scala)
      case Inject(e) => loop(s)(Bind(Inject[State[S] + F, A](e), (x: A) => Return[State[S] + F, A](x)))
      case Bind(Inject(Mine(op)), k) => op match {
        case Get() => loop(s)(k(s))
        case Update(f) => val (b, s2) = f(s); loop(s2)(k(b))
      }
      case Bind(Inject(e), k) => Inject[F, Any](e).flatMap(x => _loop(s)(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

    loop(s)(a)
  }

  /**
   * A program over a PART of the state, run over the whole: every `Get`
   * becomes a read of the whole through `look`, every `Update` one update
   * of the whole through `look` and `put`; the rest of the row passes through untouched.
   * Two functions rather than a lens, as in the Scala 3 core, where this
   * is what keeps the core free of optics: okay2-optics gives back the
   * lens spelling (`State.zoom(lens)(prog)`).
   */
  def zoomWith[S, A, X, R <: Row](look: S => A, put: A => S => S)(p: Free[State[A] with R, X])(implicit @unused d: Distinct[State[A] with R]): Free[State[S] with R, X] =
    zoomAt[S, A, X, R](look, put)(p)

  /** `zoomWith` at its own shape, the rest of the row named */
  def zoomAt[S, A, X, F <: Row](look: S => A, put: A => S => S)(p: Free[State[A] with F, X]): Free[State[S] with F, X] = {
    def readPart: Free[State[S] with F, A] = get[S].map(look)
    def updatePart[B](f: A => (B, A)): Free[State[S] with F, B] =
      update[S, B] { s => val (b, a) = f(look(s)); (b, put(a)(s)) }
    val Mine = Split.at[State[A]]

    // a call from inside flatMap cannot be a jump; `again` takes it, so the walk
    // itself stays a checked loop (specs/stack-safety.md)
    def again(x: Free[State[A] with F, X]): Free[State[S] with F, X] = loop(x)
    @tailrec def loop(x: Free[State[A] with F, X]): Free[State[S] with F, X] = Free.resume(x) match {
      case Return(v) => Return(v)
      // a lone operation is a Bind with a pure continuation (package.scala)
      case Inject(e) => loop(Bind(Inject[State[A] + F, X](e), (v: X) => Return[State[A] + F, X](v)))
      case Bind(Inject(Mine(op)), k) => op match {
        case Get() => readPart.flatMap(a => again(k(a)))
        case Update(f) => updatePart(f).flatMap(v => again(k(v)))
      }
      case Bind(Inject(e), k) => Inject[F, Any](e).flatMap(v => again(k(v)))
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
  def get[S, R]: Cont[S, S => R, S => R] = Cont.shift[S, S => R, S => R](k => new GetAt[S, R](k))

  /** write a state of a possibly different type; the old state is the value */
  def set[S, S2, R](s2: S2): Cont[S, S2 => R, S => R] = Cont.shift[S, S2 => R, S => R](k => new SetAt[S, S2, R](k, s2))

  // the two bodies as `Bounce`s: the rest of the program, `k(s)`, answered with its state, never applied here
  private final class GetAt[S, R](k: S => S => R) extends Bounce[S, R] {
    type X = S
    def next(s: S): S => R = k(s)
    def arg(s: S): S = s
  }
  private final class SetAt[S, S2, R](k: S => S2 => R, s2: S2) extends Bounce[S, R] {
    type X = S2
    def next(s: S): S2 => R = k(s)
    def arg(s: S): S2 = s2
  }

  /**
   * A FUNCTION ANSWER APPLIED BY A LOOP (okay2-cont-fun-answer, the Scala 3 core's cont-fun-answer). A
   * state-passing body `s => k(s)(s2)` applies the rest inside its own frame, so applying the answer of n steps
   * nests n host frames, outside any machine. A `Bounce` answers the next function and its argument (`next`,
   * then `arg`) instead of applying them, and its `apply` is the loop: one frame for the whole chain, on every
   * platform. A function that is not a `Bounce` ends the chain.
   */
  abstract class Bounce[-S, +R] extends (S => R) {
    /** the next function's argument type */
    type X
    /** the next function, not applied: called once a step, before `arg` */
    def next(s: S): X => R
    /** its argument */
    def arg(s: S): X
    final def apply(s: S): R = Bounce.run[S, R](this, s)
  }

  object Bounce {
    def run[S, R](b: Bounce[S, R], s: S): R = loop[R](erase[S, R](b), s)

    // THE ONE CLAIM: the pair the loop carries is a function and ITS argument — each step takes both from one
    // `Bounce` (`next`, `arg`, typed `X => R` and `X`), and the first from `run`'s typed pair. It travels erased
    // because scalac 2 keeps no `@tailrec` across changing type arguments (the Scala 3 core's loop is typed),
    // and a typed holder a step is the 24 B an operation that cost the core 1.07x on statePara.
    private def erase[A, R](f: A => R): Any => R = f.asInstanceOf[Any => R]

    @tailrec private def loop[R](f: Any => R, a: Any): R = f match {
      case b: Bounce[Any, R] @unchecked => loop[R](erase[b.X, R](b.next(a)), b.arg(a))
      case g => g(a)
    }
  }

  /** a typestate transition read as a two-parameter carrier in its
   * state, `L[A, B] = Cont[X, B => R, A => R]` — what okay2-optics
   * gives a `Strong` instance, so a lens zooms it (the Scala 3 core's
   * `PState.Zooming`, a type lambda there) */
  type Zooming[X, R] = { type L[A, B] = Cont[X, B => R, A => R] }

  /** run from an initial state to (final state, value) */
  def run[S, S2, A](s: S)(m: Cont[A, S2 => (S2, A), S => (S2, A)]): (S2, A) =
    (m / ((a: A) => (s2: S2) => (s2, a)))(s)
}
