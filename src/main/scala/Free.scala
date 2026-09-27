package okay

import scala.annotation.tailrec

/**
 * The freer monad (Kiselyov–Ishii 2015, "Freer Monads, More Extensible
 * Effects"): free over any signature F with no Functor requirement,
 * because Bind keeps the continuation as a plain function. It is ALSO
 * the tree under `Cont` (Cont.scala): a `Cont` is this enum at a leaf
 * that is a function of the continuation, with its answer types kept
 * on the facade rather than on the nodes — specs/freer-base.md says
 * why they cannot live here. Left-nested
 * binds are rebalanced by tail-recursive rotations in fold — which
 * also answers the "reflection without remorse" concern (van der
 * Ploeg–Kiselyov 2014): stepping a program one operation at a time
 * measures within ~8% of running it in bulk here (HandlerBenchmark),
 * so the type-aligned queue of that paper is not needed.
 */
object Free {
  /** a value as a tree */
  inline def pure[F[+_], A](a: A): Free[F, A] = Return(a)

  /** an operation as a tree */
  inline def inject[F[+_], A](a: F[A]): Free[F, A] = Inject(a)

  /** a bind whose LEFT side is deferred: the thunk is not forced at
   * construction, only when an interpreter's own loop (`fold`,
   * `runFree`, `resume`, `Cont.step`) reaches this node — which is
   * what lets two mutually-recursive functions returning `A ! F` call
   * each other in tail position without nesting a JVM stack frame per
   * call (`!.tailcall` is the sugar; `Cont.defer` is the same door on
   * the Cont side). */
  def defer[F[+_], A, B](thunk: () => Free[F, A])(f: A => Free[F, B]): Free[F, B] =
    // `Bind(Delay(t), f)`, not a node of its own: a `Defer(t, f)` case
    // used to hold the pair, and the runner handled it exactly as it
    // handles this shape — one node more here at construction, two
    // cases fewer in every loop that walks the tree (defer-eff-removal,
    // with the codec trampoline lane as the price it was measured on)
    Bind(Delay(thunk), f)

  /** a deferred call with NOTHING to do afterwards — `!.tailcall`'s
   * node. Not `defer(thunk)(pure)`, and the difference is the whole
   * point (delay-node): that spelling resumes to `Bind(t(), pure)`,
   * and when the thunk answers a `Bind` the rotation pushes a
   * `.flatMap(pure)` tail down EVERY bind of the deferred subprogram —
   * a closure and a `Bind` per bind, then a chain of `Bind(Return(a),
   * g)` of the same length at the end. `Delay` has no continuation to
   * push. */
  def delay[F[+_], A](thunk: () => Free[F, A]): Free[F, A] = Delay(thunk)

  /**
   * A program value as its answer, INSIDE a `direct` block: the
   * auto-colouring `Direct.selfColor` provides for any monad, given
   * here for this one so that it needs no import — implicit search for
   * `Conversion[Free[R, A], A]` looks in this companion, the source
   * type's own scope, where `Direct.given` had to be imported by name
   * (direct-no-ceremony, 2026-09-15). Gated exactly as `selfColor` is:
   * `DirectCtx` exists only inside a block, so outside one a program is
   * a program. The body never runs — the macro rewrites every call.
   */
  /**
   * A program colours inside a block whose row is R — INCLUDING a
   * program of another row R2 (direct-narrow-colour, 2026-09-16). The
   * membership `In[R2, R]` is NOT asked here: an implicit search for
   * it during conversion resolution leaves the row's halves as free
   * variables and fails even where `summon[In[R2, R]]` succeeds
   * (measured). The macro asks for it instead, with both rows already
   * known, and coerces or refuses by name — which is where every other
   * decision about a mark is made.
   */
  given directColor[R[+_], R2[+_], A](using DirectCtx[[X] =>> Free[R, X]]): Conversion[Free[R2, A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct { ... }")

  /**
   * A CONTINUATION THAT ONLY MAPS (map-flatmap-pair-cost, 2026-09-27).
   * `p.map(f).flatMap(g)`, the commonest step there is, used to build
   * `Bind(Bind(p, x => Return(f(x))), g)`: two binds nested LEFT, which
   * `resume` rotates on every step, plus the `Return` and the
   * `Bind(Return, g)` it resolves through. Measured on the same
   * right-nested 1000-operation program under two handlers: 28.6 µs /
   * 306 KB with a map + flatMap per step, against 12.6 µs / 138 KB with
   * one flatMap (BuildShapeBenchmark rowFoldM / rowOneBind). `map` now
   * leaves a `Mapped` as the continuation, and a bind or a map on top
   * of it composes the functions and keeps ONE Bind over `p`.
   *
   * Nothing runs early: `f` is still applied when the interpreter
   * answers `p`, and a program built once runs it again on every run.
   */
  final class Mapped[F[+_], X, A](val f: X => A, val depth: Int) extends (X => Free[F, A]):
    def apply(x: X): Free[F, A] = Return(f(x))

  /** how many maps one `Mapped` composes: the composed function is a
   * chain of `andThen` applies ON THE JVM STACK, so the depth is bounded
   * here (no unbounded stack recursion, AGENTS.md). Past the bound a new
   * `Bind` is nested and `resume` rotates it, as every map used to. */
  inline val MaxFusedMaps = 32

  /*
   * The two type tests below are the ONE claim these builders make, and
   * it is made here only. The continuation of a `Bind[F, x, A]` has the
   * static type `x => Free[F, A]`, and `Mapped` extends exactly that. An
   * object of class `Mapped` found there is therefore a
   * `Mapped[F, x, A]`: its type arguments are erased, but they are
   * fixed by where it sits.
   */

  /** `m.flatMap(g)`, fused through a `Mapped` continuation */
  def bind[F[+_], A, B](m: Free[F, A], g: A => Free[F, B]): Free[F, B] = m match
    case b: Bind[F, x, A] @unchecked => b.f match
      // NOT `y => g(k.f(y))`: that calls the next continuation DIRECTLY,
      // and continuations composed by other continuations (Delim's
      // `k1(x).flatMap(k2)`, n segments long) then chain n direct calls
      // on the JVM stack. TestStackSafetyCore's Delim test overflowed at
      // n = 20 000 with the first cut. `Bind(Return(_), g)` hands `g` back
      // to the interpreter's loop, as the rotation used to, and still
      // saves the rotation's Bind and closure.
      case k: Mapped[F, x, A] @unchecked => Bind(b.a, (y: x) => Bind(Return(k.f(y)), g))
      case _ => Bind(m, g)
    case _ => Bind(m, g)

  /** `m.map(f)`, fused into a `Mapped` continuation below the bound */
  def mapped[F[+_], A, B](m: Free[F, A], f: A => B): Free[F, B] = m match
    case b: Bind[F, x, A] @unchecked => b.f match
      case k: Mapped[F, x, A] @unchecked if k.depth < MaxFusedMaps =>
        Bind(b.a, Mapped[F, x, B](k.f.andThen(f), k.depth + 1))
      case _ => Bind(m, Mapped[F, A, B](f, 1))
    case _ => Bind(m, Mapped[F, A, B](f, 1))

  /** Free[F, *] is a Monad for every signature F, with no constraint on F */
  given [F[+_]]: Monad[Free[F, *]] with
    override inline def pure[A](a: A): Free[F, A] = Return(a)
    extension [A](a: Free[F, A])
      override inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
}

enum Free[F[+_], +A] {
  /** a finished computation */
  case Return(a: A)

  /** a single operation of the signature F */
  case Inject(a: F[A])

  /** sequencing: run a, then feed its value to the plain-function continuation f */
  case Bind[F[+_], A, B](a: Free[F, A],
                         f: A => Free[F, B]) extends Free[F, B]

  /** a deferred subprogram: forced by the interpreter's loop and
   * continued AS IS — see `Free.delay`; `Free.defer` is this under a
   * `Bind`. Public like the other cases: the interpreters live across
   * files (Effects.scala's `runFree`, Async.scala's loop, Cont.scala's
   * `step`), and hiding a case whose smart constructor is public would
   * stop nobody from building one — only from matching it, which is
   * the half an interpreter needs. */
  case Delay(thunk: () => Free[F, A])

  /** sequencing is a data node: nothing runs until an interpreter walks
   * the tree. A bind over a `map` FUSES into it (`Free.bind`). */
  inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.bind(this, f)

  /** a `Bind` whose continuation only maps (`Free.Mapped`), so that the
   * next `flatMap`/`map` composes into it instead of nesting (`Free.mapped`) */
  inline def map[B](f: A => B): Free[F, B] = Free.mapped(this, f)

  /**
   * THE rotation, and the only one on this side of the library:
   * normalize to a head form — `Return(a)`, `Inject(e)` or
   * `Bind(Inject(e), k)` — in constant stack.
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
   * `Free[F, A]`, whose cases include the ones that cannot occur — so
   * a correct three-case match reads as inexhaustive and did so at
   * forty-two sites, enough to bury every warning worth reading. A
   * three-case view ADT would let the compiler check it, at one
   * allocation per step on the hottest path in the library; explicit
   * impossible branches would cost one more type test per step.
   * `@unchecked` costs nothing and marks exactly the claim being made,
   * at the place it is made.
   */
  @tailrec final def resume: Free[F, A] = this match
    case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).resume
    case Bind(Return(a), f) => f(a).resume
    // the deferred subprogram is forced HERE, in the loop, and its own
    // binds then rotate through the cases above — constant stack; and
    // nothing is composed onto it: the thunk's tree continues under
    // whatever was waiting for it (delay-node)
    case Delay(t) => t().resume
    case Bind(Delay(t), g) => Bind(t(), g).resume
    case a => a

  /**
   * the eliminator: p interprets values, h interprets operations
   * together with their continuations — three cases over the head
   * form `resume` leaves, rather than a seventh copy of the rotation.
   */
  final def fold[B](p: A => B)
                   (h: [X] => F[X] => (X => Free[F, A]) => B): B =
    (this.resume: @unchecked) match
      case Return(a) => p(a)
      case Inject(a) => h(a)(Return(_))
      case Bind(Inject(a), f) => h(a)(f)

}
