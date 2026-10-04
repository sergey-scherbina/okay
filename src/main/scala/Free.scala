package okay

import scala.annotation.tailrec

/**
 * The freer monad (Kiselyov–Ishii 2015): free over any signature with no Functor requirement, because `Bind`
 * keeps the continuation as a plain function. The signature `G` also carries the answer types `S` and `R` of a
 * `(A => S) => R`, so the same tree is `Cont`'s (Cont.scala): a `Bind` joins a left side answering `T => R` to
 * a continuation answering `S => T`, Danvy–Filinski's answer-type modification written on the node. A
 * signature that ignores them (`Lift`) gives the plain effect tree, `Free[F, A]`, at `Unit`.
 *
 * `A` comes LAST on purpose: a unary constructor inferred from a program value (`Monad[M]` from an `A ! F`)
 * abstracts over the last parameter. A matched `Bind` leaves its middle index existential, and
 * `Free.Bind`'s extractor binds it back to `Unit`: the one cast (specs/freer-base.md).
 */
enum Freer[G[_, _, +_], S, R, +A] {
  // `S` and `R` are INVARIANT. Read as a continuation `(A => S) => R`, `R` would be covariant; read as a
  // state transition `R => (S, A)`, contravariant. Invariance serves both, and a handler that consumes its
  // index (a type-changing state) is typed by the GADT (TestFreerPara; specs/freer-base.md, "McBride's reading").
  /** a finished computation: the inner answer IS the outer one */
  case Return[G[_, _, +_], R, A](a: A) extends Freer[G, R, R, A]

  /** a single operation of the signature: for an effect, `F[A]`; for
   * `Cont`, the shift body `(A => S) => R` itself */
  case Inject[G[_, _, +_], S, R, A](a: G[S, R, A]) extends Freer[G, S, R, A]

  /** sequencing: run a, then feed its value to the plain-function
   * continuation f; the answer types meet at `T` */
  case Bind[G[_, _, +_], S, T, R, A, B](a: Freer[G, T, R, A],
                                        f: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  /** a deferred subprogram, forced by the interpreter's loop and continued as is (`Free.delay`; `Free.defer` is
   * this under a `Bind`). Public so that the interpreters in other files can match it */
  case Delay[G[_, _, +_], S, R, A](thunk: () => Freer[G, S, R, A]) extends Freer[G, S, R, A]

  /** sequencing is a data node: nothing runs until an interpreter walks the tree */
  inline def flatMap[B, S2](f: A => Freer[G, S2, S, B]): Freer[G, S2, R, B] = Bind(this, f)

  /** a Bind whose continuation is a `Freer.Mapped`: the same node and the
   * same run as `flatMap(a => Return(f(a)))`, but a builder that knows it
   * (`!.foldM`) can read the function back out (one-bind-hot-steps) */
  inline def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, Freer.Mapped[G, S, A, B](f))

  /**
   * THE rotation: normalize to a head form — `Return(a)`, `Inject(e)` or `Bind(Inject(e), k)` — in constant stack, for every signature, with no cast. Sound by associativity,
   * linear-time amortized for programs built by `foldLeft`. Stepping one operation at a time measures within
   * ~8% of a bulk run (HandlerBenchmark), so the queue of "reflection without remorse" (van der Ploeg–Kiselyov
   * 2014) is not needed. A member, so every interpreter's `.resume` reaches it with no import.
   *
   * Matches over its result are written `(x.resume: @unchecked) match`: the result is always a head form,
   * which the type cannot say. A view ADT would cost an allocation per step; `@unchecked` costs nothing.
   */
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).resume
    case Bind(Return(a), f) => f(a).resume
    // forced HERE, in the loop: its binds rotate through the cases above, and nothing is composed onto it
    case Delay(t) => t().resume
    case Bind(Delay(t), g) => Bind(t(), g).resume
    case a => a

  /**
   * `resume` that STOPS at a run — a `Delay` whose thunk is `Freer.Suspended` — alone or under its `Bind`,
   * instead of forcing it: a loop that can hand itself to the machine walks with this, so a run nested in it
   * never runs inside it. A method of its own, so that `resume` carries no `Pending` test.
   */
  // ONE test for `Bind`, its head matched once: written flat, as `resume` is, it measured 345 bytes, past
  // HotSpot's FreqInlineSize (325) that `resume`'s 322 sit under — the loops lost its inlining, 1.32x
  @tailrec final def resumeRun: Freer[G, S, R, A] = this match
    case Bind(h, g) => h match
      case Bind(a, f) => Bind(a, f(_).flatMap(g)).resumeRun
      case Return(a) => g(a).resumeRun
      case Delay(t) => if t.isInstanceOf[Freer.Suspended] then this else Bind(t(), g).resumeRun
      case _ => this
    case Delay(t) => if t.isInstanceOf[Freer.Suspended] then this else t().resumeRun
    case a => a
}

object Freer {

  /**
   * A MACHINE PRIMITIVE, no effect's: a `Delay` thunk that is a run of its own, which a running machine may step
   * into in its own loop rather than force (a loop that can hand itself to a machine stops at it, `resumeRun`).
   * A machine's own runs implement it; `Freer` knows nothing of which.
   */
  trait Suspended


  /**
   * A continuation that only maps, left by `map`: it runs as `a => Return(f(a))`, and the tree has the same
   * shape, but a builder can read `f` back — `!.foldM` then builds one bind a step instead of two
   * (specs/map-fusion.md). Only a builder that calls nothing but its own next step may do that: `flatMap`
   * itself must not, since its continuation is anybody's (calling it chained Shift's continuations 20 000 deep).
   */
  final class Mapped[G[_, _, +_], S, X, A](val f: X => A) extends (X => Freer[G, S, S, A]):
    def apply(x: X): Freer[G, S, S, A] = Return(f(x))

  /** a bind whose LEFT side is deferred: forced only when an interpreter's loop reaches it, so functions
   * returning `A ! F` call each other in tail position without a JVM frame per call (`!.tailcall`;
   * `Cont.defer` on the Cont side) */
  def defer[G[_, _, +_], S, T, R, A, B](thunk: () => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
    // not a node of its own: one node more at construction, two cases fewer in every loop that walks the tree
    Bind(Delay(thunk), f)

  /** a deferred call with NOTHING after it, `!.tailcall`'s node. Not `defer(thunk)(pure)`: resumed, that
   * pushes a `.flatMap(pure)` down every bind of the deferred subprogram; `Delay` has nothing to push */
  def delay[G[_, _, +_], S, R, A](thunk: () => Freer[G, S, R, A]): Freer[G, S, R, A] = Delay(thunk)

  // level 1 (specs/shift-effect.md): in the companion, so `p.handle` and `p.run` need no import
  extension [A, G[+_]](p: A ! G)
    /** take the handler's effect off the row: `F`, the rest of the row, is what remains */
    def handle[E[+_], I, O[_], N[_[+_]], F[+_]](h: Handler.Full[E, I, O, N])
                                               (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): O[A] ! F =
      h.run[A, F](row(p))

    /** two handlers, innermost first: `p.handle(State(5), Throws.either)` is `p.handle(State(5)).handle(Throws.either)` */
    def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_]](
        h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2])
        (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
               r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2]): O2[O1[A]] ! F2 =
      h2.run[O1[A], F2](r2(h1.run[A, F1](r1(p))))

    /** three handlers, innermost first */
    def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_], Ef3[+_], I3, O3[_], N3[_[+_]], F3[+_]](
        h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2], h3: Handler.Full[Ef3, I3, O3, N3])
        (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
               r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2],
               r3: (O2[O1[A]] ! F2) =:= (O2[O1[A]] ! Ef3 + F3), ok3: O2[O1[A]] <:< I3, d3: Distinct[Ef3 + F3], n3: N3[F3])
        : O3[O2[O1[A]]] ! F3 =
      h3.run[O2[O1[A]], F3](r3(h2.run[O1[A], F2](r2(h1.run[A, F1](r1(p))))))

  extension [A](p: A ! Pure)
    /** a program with no effect left, run to its value */
    inline def run: A = p.runWith

  /**
   * A program value as its answer INSIDE a `direct` block: `Direct.selfColor` for this monad, found here with
   * no import (a conversion's implicit scope is its source type's, and `Free` dealiases to `Freer`).
   * `DirectCtx` exists only inside a block, so outside one a program stays a program; the body never runs.
   * A program of ANOTHER row colours too: the macro, which knows both rows, checks the membership and coerces
   * or refuses by name (searched here, the row's halves would still be free variables).
   */
  given directColor[R[+_], R2[+_], A](using DirectCtx[[X] =>> Free[R, X]]): Conversion[Free[R2, A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct { ... }")

  /** Free[F, *] is a Monad for every signature F, with no constraint on F */
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Return(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)

  /** a program's loop is `!.loop`: the recursion sits in a `Bind` the
   * interpreter resumes, never on the caller's stack
   * (specs/eager-carrier-depth.md) */
  given [F[+_]]: TailRecM[Free[F, *]] with
    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] = Effects.loop(a)(f)

  /** the tree in `ParaMonad`'s order, value first (`Freer` keeps `A` last for inference) */
  type Para[G[_, _, +_]] = [A, S, R] =>> Freer[G, S, R, A]

  /**
   * Freer is Atkey's parameterised monad for every signature: `Return` on the diagonal, `Bind` composing the
   * indexes end to end. Prefix `Bind`, not `m.flatMap(f)`: extension syntax inside an override resolves to
   * the override itself. `map` builds the `Mapped` node `!.foldM` reads back.
   */
  given [G[_, _, +_]]: ParaMonad[Para[G]] with
    override def pure[A, R](a: A): Freer[G, R, R, A] = Return(a)
    extension [A, S, R](m: Freer[G, S, R, A])
      override def flatMap[B, S2](f: A => Freer[G, S2, S, B]): Freer[G, S2, R, B] = Bind(m, f)
      override def map[B](f: A => B): Freer[G, S, R, B] = Bind(m, Mapped[G, S, A, B](f))
}

/** the effect program: the base at `Unary[F]`, every index `Unit`. `object Free` keeps the four names
 * (`Return`, `Inject`, `Bind`, `Delay`) at the arities the match sites and the `direct` macro use */
type Free[F[+_], +A] = Freer[Unary[F], Unit, Unit, A]

object Free {

  /** a value as a tree */
  inline def pure[F[+_], A](a: A): Free[F, A] = Freer.Return(a)

  /** an operation as a tree */
  inline def inject[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Unary[F], Unit, Unit, A](a)

  /** `Freer.defer` at the effect tree's indexes */
  def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Freer.defer(thunk)(f)

  /** `Freer.delay` at the effect tree's indexes */
  def delay[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)

  /** the eliminator: `p` interprets values, `h` operations with their continuations, over the head form
   * `resume` leaves */
  def fold[F[+_], A, B](m: Free[F, A])(p: A => B)
                                     (h: [X] => F[X] => (X => Free[F, A]) => B): B =
    (m.resume: @unchecked) match
      case Return(a) => p(a)
      case Inject(a) => h(a)(Freer.Return(_))
      case Bind(Inject(a), f) => h(a)(f)

  /**
   * The four names at the effect tree's arity (`Inject[F, A](e)`, `case Bind(Inject(e), k)`). The patterns are
   * product matches — `unapply` answers the node itself — so a match allocates nothing. Plain `def`s, not
   * inline: the `direct` macro recognises a program by the symbols of these `apply`s (DirectRow.scala).
   */
  object Return:
    def apply[F[+_], A](a: A): Free[F, A] = Freer.Return(a)
    def unapply[G[_, _, +_], R, A](r: Freer.Return[G, R, A]): Freer.Return[G, R, A] = r

  object Inject:
    def apply[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Unary[F], Unit, Unit, A](a)
    /** the node at its operation's own type, `F[A]` — `Unary`'s extractor, at the indexes every `Free` node
     * has (`Unit, Unit`); a product match, the node itself, nothing allocated (TestInlineBudget measures
     * `handle`'s loop) */
    def unapply[F[+_], A](i: Freer.Inject[Unary[F], Unit, Unit, A]): Freer.Inject[Unary.Op[F], Unit, Unit, A] =
      Unary.unapply(i)

  object Delay:
    def apply[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)
    def unapply[G[_, _, +_], S, R, A](d: Freer.Delay[G, S, R, A]): Freer.Delay[G, S, R, A] = d

  /**
   * THE ONE CAST of the effect side. A `Bind` matched through a tree typed at `Unit` has a middle index `T`
   * the type forgot. Every door that builds an effect program puts `Unit` there, and `Lift` carries no answer
   * type for another `T` to come from, so `T` IS `Unit` — said once here, for every `Bind(Inject(e), k)` site.
   * The type variables sit in the PARAMETER, so the compiler's type test binds them (in the result alone they
   * would be inferred `Nothing`); the result is the node, so the pattern allocates nothing. Unreachable on a
   * `Cont`, which is opaque outside its companion.
   */
  object Bind:
    def apply[F[+_], A, B](a: Free[F, A], f: A => Free[F, B]): Free[F, B] = Freer.Bind(a, f)
    def unapply[G[_, _, +_], T, R, X, A](b: Freer.Bind[G, Unit, T, R, X, A]): Freer.Bind[G, Unit, Unit, R, X, A] =
      b.asInstanceOf[Freer.Bind[G, Unit, Unit, R, X, A]]
}
