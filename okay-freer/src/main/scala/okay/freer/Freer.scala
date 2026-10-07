package okay.freer

import okay.{Monad, ParaMonad, TailRecM, ==>}

import scala.annotation.tailrec

/**
 * The freer monad (Kiselyov–Ishii 2015): free over any signature with no Functor requirement, because `Bind`
 * keeps the continuation as a plain function. The signature `G` also carries the answer types `S` and `R` of a
 * `(A => S) => R`, so the same tree is `Cps`'s (Cps.scala): a `Bind` joins a left side answering `T => R` to
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
   * `Cps`, the shift body `(A => S) => R` itself */
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
   * `Cps.defer` on the Cps side) */
  def defer[G[_, _, +_], S, T, R, A, B](thunk: () => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
    // not a node of its own: one node more at construction, two cases fewer in every loop that walks the tree
    Bind(Delay(thunk), f)

  /** a deferred call with NOTHING after it, `!.tailcall`'s node. Not `defer(thunk)(pure)`: resumed, that
   * pushes a `.flatMap(pure)` down every bind of the deferred subprogram; `Delay` has nothing to push */
  def delay[G[_, _, +_], S, R, A](thunk: () => Freer[G, S, R, A]): Freer[G, S, R, A] = Delay(thunk)

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

  /** the tree into any monad: `G`'s own `tailRecM`, as cats' `Free.foldMap` is, so the fold is exactly as
   * stack-safe as the carrier's loop (specs/eager-carrier-depth.md); each step resumes the tree once. In `Freer`'s
   * companion — `Free` is an alias, its object no companion — so that a tree finds it and it shadows no other
   * `foldMap` (okay-optics has one on `Optic`) */
  extension [F[+_], A](p: Free[F, A])
    def foldMap[G[_]](nt: F ==> G)(using G: Monad[G], R: TailRecM[G]): G[A] =
      R.tailRecM[A ! F, A](p) { p =>
        (p.resume: @unchecked) match
          case Free.Return(a) => G.pure(Right(a))
          case Free.Inject(e) => G.fmap(nt(e), a => Right(a))
          case Free.Bind(Free.Inject(e), k) => G.fmap(nt(e), x => Left(k(x)))
      }
}
