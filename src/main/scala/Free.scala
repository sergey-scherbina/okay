package okay

import scala.annotation.tailrec

/**
 * THE ONE INDEXED BASE (freer-base-step-extractor, 2026-09-29;
 * specs/freer-base.md, "The dual placement"). The freer monad
 * (Kiselyov–Ishii 2015, "Freer Monads, More Extensible Effects"): free
 * over any signature with no Functor requirement, because `Bind` keeps
 * the continuation as a plain function — and the tree under `Cont`
 * (Cont.scala) at the same time, because the signature `G` carries the
 * ANSWER TYPES `S` and `R` of a `(A => S) => R` beside the value. A
 * `Bind` joins a left side that answers `T => R` to a continuation that
 * answers `S => T`, which is Danvy and Filinski's answer-type
 * modification written on the node; a signature that ignores the two
 * (`Lift`, below) leaves them phantom and gets the plain effect tree,
 * `Free[F, A]`, at `Unit`. The value type `A` comes LAST, and that is
 * load-bearing: a unary constructor inferred from a program value —
 * `Monad[M]` from an `A ! F`, `Stream[S, F]` from a writer program —
 * is the type abstracted over its last parameter, and `[A] =>> Free[F,
 * A]` is what every instance in the library is written for.
 *
 * Why the indexes live HERE now and did not before: specs/freer-base.md
 * stage 1 was refuted because matching an indexed `Bind` makes the
 * intermediate index existential, and the 100-odd match sites over an
 * effect program want `k: X => Free[F, A]`. The extractor that stage
 * tried put the type variable only in `unapply`'s RESULT, which dotty
 * infers as `Nothing`. `Free.Bind` below puts it in the PARAMETER, so
 * the compiler inserts the type test that binds it — one cast, at one
 * place, with a constant claim (a `Lift` tree is built with every index
 * `Unit`), where the facade it replaces trusted two casts inside `Cont`'s
 * runner (`Shift.at`, `pinned`) and spelt the leaf `(X => Nothing) =>
 * Any` to fit an unindexed tree.
 *
 * Left-nested binds are rebalanced by tail-recursive rotations in
 * `resume` — which also answers the "reflection without remorse"
 * concern (van der Ploeg–Kiselyov 2014): stepping a program one
 * operation at a time measures within ~8% of running it in bulk here
 * (HandlerBenchmark), so the type-aligned queue of that paper is not
 * needed.
 */
enum Freer[G[_, _, +_], S, R, +A] {
  /**
   * THE INDEXES ARE INVARIANT, and that is the decision that lets the
   * tree serve TWO readings (freer-consumed-index, 2026-09-30, the
   * operator's: "делаем S и R инвариантными"). Read as a continuation,
   * `(A => S) => R`, `R` is produced and would be covariant; read as a
   * state transition, `R => (S, A)`, `R` is consumed and would be
   * contravariant — the two readings want OPPOSITE variances on both
   * indexes, and invariance is what both can live with. `+A` the
   * effect tree always had and keeps. Until this decision the base was
   * `+R`, for one reader: `Cont.tailShift`/`tailPure`'s `liftCo` of a
   * `Return(v): Cont[A, S, S]` into the `Cont[A, S, R]` a tail-shaped
   * shift body is owed. That is now one cast there, justified by the
   * `S <:< R` the macro summons at the site. What invariance buys: a
   * handler that CONSUMES its index — a type-changing state threaded
   * by `State.handle`'s loop, a held resource typed by the index — is
   * typed by the GADT (`Return` gives `S = R`, an operation gives its
   * state's type to the continuation), where `+R` gave only `S <: R`
   * and refused every arm that reads the state (TestFreerPara pins
   * both directions; specs/freer-base.md "McBride's reading").
   */
  /** a finished computation: the inner answer IS the outer one */
  case Return[G[_, _, +_], R, A](a: A) extends Freer[G, R, R, A]

  /** a single operation of the signature: for an effect, `F[A]`; for
   * `Cont`, the shift body `(A => S) => R` itself */
  case Inject[G[_, _, +_], S, R, A](a: G[S, R, A]) extends Freer[G, S, R, A]

  /**
   * A DIAGONAL OPERATION: one that moves no index, and says so ON THE
   * NODE (freer-diag-leaf, 2026-09-30). `Inject` holds an operation at
   * the indexes the signature gives it; for a unary effect lifted into
   * an indexed row (`[S, R, X] =>> PSt[S, R, X] | State[Int, X]`) the
   * signature gives it ANY indexes, and a handler's loop over a mixed
   * row cannot use that: matching `Bind(Inject(op), k)` makes the
   * middle index an existential `T`, and the program the handler
   * continues with sits at `T` where the loop owes one at `R`. A
   * `Diag` matched under a `Bind` gives `T = R` by the GADT, so the
   * continuation IS at `R`: a unary effect enters an indexed row bare,
   * through `Freer.diag`, with no wrapper allocated per operation and
   * no extractor cast — the two roads TestFreerPara's row probe had
   * before this case existed.
   *
   * NEVER built by the erased effect tree or by `Cont`: `Free`'s doors
   * at `Unit` build `Inject` (the 112 `Bind(Inject(e), k)` sites across
   * the family do not move, and at `Unit` the two nodes mean the same),
   * and `Cont`'s companion builds `Shift0` leaves as `Inject`. Their loops
   * say so with `@unchecked` rather than a dead arm in the hottest
   * loop of the library. The discipline is a door's, as `Free.Bind`'s
   * constant claim is: a unary operation enters an INDEXED row through
   * `diag`, and a handler of one matches `Diag`.
   */
  case Diag[G[_, _, +_], R, A](a: G[R, R, A]) extends Freer[G, R, R, A]

  /** sequencing: run a, then feed its value to the plain-function
   * continuation f; the answer types meet at `T` */
  case Bind[G[_, _, +_], S, T, R, A, B](a: Freer[G, T, R, A],
                                        f: A => Freer[G, S, T, B]) extends Freer[G, S, R, B]

  /** a deferred subprogram: forced by the interpreter's loop and
   * continued AS IS — see `Free.delay`; `Free.defer` is this under a
   * `Bind`. Public like the other cases: the interpreters live across
   * files (Effects.scala's `runFree`, Async.scala's loop, Cont.scala's
   * `step`), and hiding a case whose smart constructor is public would
   * stop nobody from building one — only from matching it, which is
   * the half an interpreter needs. */
  case Delay[G[_, _, +_], S, R, A](thunk: () => Freer[G, S, R, A]) extends Freer[G, S, R, A]

  /** sequencing is a data node: nothing runs until an interpreter walks the tree */
  inline def flatMap[B, S2](f: A => Freer[G, S2, S, B]): Freer[G, S2, R, B] = Bind(this, f)

  /** a Bind whose continuation is a `Freer.Mapped`: the same node and the
   * same run as `flatMap(a => Return(f(a)))`, but a builder that knows it
   * (`!.foldM`) can read the function back out (one-bind-hot-steps) */
  inline def map[B](f: A => B): Freer[G, S, R, B] = Bind(this, Freer.Mapped[G, S, A, B](f))

  /**
   * THE rotation, and the only one on this side of the library:
   * normalize to a head form — `Return(a)`, `Inject(e)` or
   * `Bind(Inject(e), k)`, and on an indexed row `Diag(e)` or
   * `Bind(Diag(e), k)` likewise — in constant stack. Written ONCE, for every
   * signature and every index: nothing here casts.
   *
   * Sound by the monad associativity law, and linear-time amortized
   * for programs built by `foldLeft`. It also answers the "reflection
   * without remorse" concern (van der Ploeg–Kiselyov 2014): stepping a
   * program one operation at a time measures within ~8% of running it
   * in bulk here (HandlerBenchmark), so the type-aligned queue of that
   * paper is not needed.
   *
   * It is a MEMBER, not an extension, because it is a property of the
   * tree rather than of any encoding built on it — and because a
   * member wins resolution, so the interpreters that call `.resume`
   * across the library all reach this one loop with nothing imported.
   *
   * THE INVARIANT IT ESTABLISHES, and why every match over it is
   * written `(x.resume: @unchecked) match`: by construction the result
   * is one of exactly those three shapes, because the cases below
   * normalize the other two away. The TYPE cannot say so — it is still
   * `Freer[G, S, R, A]`, whose cases include the ones that cannot
   * occur — so a correct three-case match reads as inexhaustive and did
   * so at forty-two sites, enough to bury every warning worth reading.
   * A three-case view ADT would let the compiler check it, at one
   * allocation per step on the hottest path in the library; explicit
   * impossible branches would cost one more type test per step.
   * `@unchecked` costs nothing and marks exactly the claim being made,
   * at the place it is made.
   */
  @tailrec final def resume: Freer[G, S, R, A] = this match
    case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).resume
    case Bind(Return(a), f) => f(a).resume
    // the deferred subprogram is forced HERE, in the loop, and its own
    // binds then rotate through the cases above — constant stack; and
    // nothing is composed onto it: the thunk's tree continues under
    // whatever was waiting for it (delay-node)
    case Delay(t) => t().resume
    case Bind(Delay(t), g) => Bind(t(), g).resume
    case a => a
}

object Freer {

  /**
   * The effect signature's leaf, indexes IGNORED: `Free[F, A]` is this
   * base at `Lift[F]` with every index `Unit`. Nothing about `F` says
   * anything about an answer type, so nothing in the tree does either;
   * `Free.Bind` (the one cast) is what turns a `Bind`'s existential
   * middle index back into the `Unit` every door here put there.
   */
  type Lift[F[+_]] = Lifted[F]#L

  /**
   * `Lift[F]` is a PROJECTION on a class, not a bare type lambda, and
   * the reason is inference: `[S, R, X] =>> F[X]` applied to a row
   * `Users + F` beta-reduces, when two such lambdas are compared, to
   * `Users[X] | F[X]` against `F1[X] | G[X]`, and a union has no
   * structure to solve `F1` and `G` from — `!.tracing(p)([X] => (e:
   * Users[X]) => …)` inferred `F1 := Users + F`. A projection compares
   * by its PREFIX, `Lifted[Users + F]` against `Lifted[F1 + G]`, so the
   * row's `+` is matched application to application as it was when
   * `Free` was its own enum. The member reduces to `F[X]` wherever it
   * is applied, so nothing else sees the difference.
   */
  sealed trait Lifted[F[+_]]:
    type L[S, R, +X] = F[X]

  /**
   * A CONTINUATION THAT ONLY MAPS, left by `map` (one-bind-hot-steps,
   * 2026-09-27). It runs exactly as `a => Return(f(a))` did, and the tree
   * is the same shape. What it adds is that a BUILDER can see the
   * function. `!.foldM` meets `op.map(f)` as its step and builds
   * `Bind(op, y => next(f(y)))`, one bind instead of the map's and its
   * own nested left and rotated every step (specs/map-fusion.md: 28.6 µs
   * / 306 KB against 12.6 / 138 per 1000 steps).
   *
   * Only a builder that CALLS NOTHING BUT ITS OWN NEXT STEP may do that.
   * `Freer.flatMap` itself must not: its continuation is anybody's, and
   * calling it directly chained Delim's composed continuations 20 000
   * deep (map-fusion, refuted).
   */
  final class Mapped[G[_, _, +_], S, X, A](val f: X => A) extends (X => Freer[G, S, S, A]):
    def apply(x: X): Freer[G, S, S, A] = Return(f(x))

  /** a bind whose LEFT side is deferred: the thunk is not forced at
   * construction, only when an interpreter's own loop (`fold`,
   * `runFree`, `resume`, `Frames.run`) reaches this node — which is
   * what lets two mutually-recursive functions returning `A ! F` call
   * each other in tail position without nesting a JVM stack frame per
   * call (`!.tailcall` is the sugar; `Cont.defer` is the same door on
   * the Cont side). */
  def defer[G[_, _, +_], S, T, R, A, B](thunk: () => Freer[G, T, R, A])(f: A => Freer[G, S, T, B]): Freer[G, S, R, B] =
    // `Bind(Delay(t), f)`, not a node of its own: a `Defer(t, f)` case
    // used to hold the pair, and the runner handled it exactly as it
    // handles this shape — one node more here at construction, two
    // cases fewer in every loop that walks the tree (defer-eff-removal,
    // with the codec trampoline lane as the price it was measured on)
    Bind(Delay(thunk), f)

  /** a unary operation into an INDEXED row, on the diagonal by its
   * node — see `Diag`. The effect tree at `Unit` does not use this door;
   * `Free.inject` builds `Inject` there, and the two coincide. */
  def diag[G[_, _, +_], R, A](a: G[R, R, A]): Freer[G, R, R, A] = Diag(a)

  /** a deferred call with NOTHING to do afterwards — `!.tailcall`'s
   * node. Not `defer(thunk)(pure)`, and the difference is the whole
   * point (delay-node): that spelling resumes to `Bind(t(), pure)`,
   * and when the thunk answers a `Bind` the rotation pushes a
   * `.flatMap(pure)` tail down EVERY bind of the deferred subprogram —
   * a closure and a `Bind` per bind, then a chain of `Bind(Return(a),
   * g)` of the same length at the end. `Delay` has no continuation to
   * push. */
  def delay[G[_, _, +_], S, R, A](thunk: () => Freer[G, S, R, A]): Freer[G, S, R, A] = Delay(thunk)

  // level 1 (specs/shift-effect.md): in the companion, so `p.handle` and `p.run` need no import
  extension [A, G[+_]](p: A ! G)
    /** take the handler's effect off the row: `F`, the rest of the row, is what remains */
    def handle[E[+_], I, O[_], N[_[+_]], F[+_]](h: Handler.Full[E, I, O, N])
                                               (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): O[A] ! F =
      h.run[A, F](row(p))

  extension [A](p: A ! Pure)
    /** a program with no effect left, run to its value */
    inline def run: A = p.runWith

  /**
   * A program value as its answer, INSIDE a `direct` block: the
   * auto-colouring `Direct.selfColor` provides for any monad, given
   * here for this one so that it needs no import — implicit search for
   * `Conversion[Free[R, A], A]` looks in this companion, the source
   * type's own scope, where `Direct.given` had to be imported by name
   * (direct-no-ceremony, 2026-09-15). Gated exactly as `selfColor` is:
   * `DirectCtx` exists only inside a block, so outside one a program is
   * a program. The body never runs — the macro rewrites every call.
   *
   * A program colours inside a block whose row is R — INCLUDING a
   * program of another row R2 (direct-narrow-colour, 2026-09-16). The
   * membership `In[R2, R]` is NOT asked here: an implicit search for
   * it during conversion resolution leaves the row's halves as free
   * variables and fails even where `summon[In[R2, R]]` succeeds
   * (measured). The macro asks for it instead, with both rows already
   * known, and coerces or refuses by name — which is where every other
   * decision about a mark is made.
   *
   * In THIS companion and not `Free`'s: `Free` is an alias now, and the
   * implicit scope of `Free[R2, A]` is the scope of what it dealiases to.
   */
  given directColor[R[+_], R2[+_], A](using DirectCtx[[X] =>> Free[R, X]]): Conversion[Free[R2, A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct { ... }")

  /** Free[F, *] is a Monad for every signature F, with no constraint on F */
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Return(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)

  /**
   * The tree in `ParaMonad`'s order — value first, then the indexes
   * (freer-paramonad, 2026-09-30). `Freer` keeps `A` LAST for inference
   * (the header says why); `ParaMonad[M[_, _, _]]` reads `M[A, S, R]`,
   * so the instance is on this lambda, and a program written against
   * an abstract `ParaMonad[M]` runs at `Freer.Para[G]` for any `G`.
   */
  type Para[G[_, _, +_]] = [A, S, R] =>> Freer[G, S, R, A]

  /**
   * Freer IS Atkey's parameterised monad, for every signature: `Return`
   * on the diagonal, `Bind` composing the indexes end to end — the
   * instance only says so. `Control[Cont]` (Cont.scala) is the same
   * structure at the `Shift` signature with absorption on top, and
   * `Cont` is opaque, so the two never meet in a search.
   *
   * PREFIX `Bind`, not `m.flatMap(f)`: extension syntax inside an
   * override resolves to the override being defined (the self-recursion
   * `Cont.bind`'s comment records). `map` is overridden so the node is
   * the `Mapped` one `!.foldM` can read back, not the default's
   * `flatMap` into a `pure`.
   */
  given [G[_, _, +_]]: ParaMonad[Para[G]] with
    override def pure[A, R](a: A): Freer[G, R, R, A] = Return(a)
    extension [A, S, R](m: Freer[G, S, R, A])
      override def flatMap[B, S2](f: A => Freer[G, S2, S, B]): Freer[G, S2, R, B] = Bind(m, f)
      override def map[B](f: A => B): Freer[G, S, R, B] = Bind(m, Mapped[G, S, A, B](f))
}

/**
 * The effect program: the base at `Lift[F]`, every index `Unit`. An
 * alias, and `object Free` beside it keeps the four names every match
 * site and the `direct` macro use — `Return`, `Inject`, `Bind`, `Delay`
 * — at the arities they always had.
 */
type Free[F[+_], +A] = Freer[Freer.Lift[F], Unit, Unit, A]

object Free {
  import Freer.Lift

  /** a value as a tree */
  inline def pure[F[+_], A](a: A): Free[F, A] = Freer.Return(a)

  /** an operation as a tree */
  inline def inject[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Lift[F], Unit, Unit, A](a)

  /** `Freer.defer` at the effect tree's indexes */
  def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    Freer.Bind(Freer.Delay(thunk), f)

  /** `Freer.delay` at the effect tree's indexes */
  def delay[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)

  /**
   * the eliminator: p interprets values, h interprets operations
   * together with their continuations — three cases over the head
   * form `resume` leaves, rather than a seventh copy of the rotation.
   */
  def fold[F[+_], A, B](m: Free[F, A])(p: A => B)
                                     (h: [X] => F[X] => (X => Free[F, A]) => B): B =
    (m.resume: @unchecked) match
      case Return(a) => p(a)
      case Inject(a) => h(a)(Freer.Return(_))
      case Bind(Inject(a), f) => h(a)(f)

  /**
   * THE FOUR NAMES, at the effect tree. Each is a constructor and a
   * pattern at the arity the old enum had (`Inject[F, A](e)`,
   * `case Bind(Inject(e), k)`), so no match site moved when the base
   * gained its indexes. The patterns are PRODUCT MATCHES: `unapply`
   * answers the node itself, whose `_1`/`_2` are its fields, so a
   * match costs the type test the case class already paid and no
   * `Option`, no tuple, no allocation. They are plain `def`s, not
   * inline, on purpose: the `direct` macro recognises a program by the
   * SYMBOL of `Free.Return.apply`, `Free.Inject.apply` and
   * `Free.Bind.apply` in the user's tree (DirectRow.scala), which an
   * inlined body would erase.
   */
  object Return:
    def apply[F[+_], A](a: A): Free[F, A] = Freer.Return(a)
    def unapply[G[_, _, +_], R, A](r: Freer.Return[G, R, A]): Freer.Return[G, R, A] = r

  object Inject:
    def apply[F[+_], A](a: F[A]): Free[F, A] = Freer.Inject[Lift[F], Unit, Unit, A](a)
    def unapply[G[_, _, +_], S, R, A](i: Freer.Inject[G, S, R, A]): Freer.Inject[G, S, R, A] = i

  object Delay:
    def apply[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Freer.Delay(thunk)
    def unapply[G[_, _, +_], S, R, A](d: Freer.Delay[G, S, R, A]): Freer.Delay[G, S, R, A] = d

  /**
   * THE ONE CAST of the effect side, and the argument for it.
   *
   * A `Bind` reached through a tree typed at `Unit` indexes has a
   * MIDDLE index the type forgot: `Bind[G, Unit, T, R, X, A]` for a
   * `T` the compiler binds fresh at every match. Every door that builds
   * an effect program — `Free.pure`, `inject`, `defer`, `delay`, the
   * four objects here, `Freer.flatMap` and `map` on a program typed at
   * `Unit` — puts `Unit` there, and nothing else can build one: `Lift`
   * carries no answer type for a `T` to come from. So `T` IS `Unit`,
   * and this `unapply` says so, once, for every site that matches
   * `Bind(Inject(e), k)` and wants `k: X => Free[F, A]`.
   *
   * The type variables sit in the PARAMETER type, which is what makes
   * this work where stage 1's extractor (specs/freer-base.md) did not:
   * a variable only in the result is inferred as `Nothing`; one in the
   * parameter is bound by the type test the compiler inserts for it.
   * The result is the node (a Product), so the pattern allocates
   * nothing. Applied to a scrutinee typed `Free[F, A]` — the standing
   * of `(x.resume: @unchecked)`, no worse and no better: on a `Cont`,
   * which is opaque outside its companion, it cannot be reached.
   */
  object Bind:
    def apply[F[+_], A, B](a: Free[F, A], f: A => Free[F, B]): Free[F, B] = Freer.Bind(a, f)
    def unapply[G[_, _, +_], T, R, X, A](b: Freer.Bind[G, Unit, T, R, X, A]): Freer.Bind[G, Unit, Unit, R, X, A] =
      b.asInstanceOf[Freer.Bind[G, Unit, Unit, R, X, A]]
}
