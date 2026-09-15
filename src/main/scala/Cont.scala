package okay

/**
 * Final tagless interface of delimited control: the parameterised
 * continuation monad (ParaMonad) with the shift operator of Danvy
 * and Filinski, with answer-type modification. M[A, S, R] means
 * (A => S) => R, which `/` (run) eliminates.
 */
trait Control[M[_, _, _]] extends ParaMonad[M]:
  def shift[A, S, R](f: (A => S) => R): M[A, S, R]
  extension [A, S, R](m: M[A, S, R])
    infix def /(k: A => S): R
  inline def reset[A, R](m: M[A, A, R]): R = m / identity

/**
 * Staging via final tagless (Carette–Kiselyov–Shan, the partial
 * evaluation half): in an `inline def` program, `val C = Control[M]`
 * summons the instance at its precise type, so the instance's inline
 * operations resolve statically and the tagless dispatch evaporates
 * at compile time — at the Func carrier the program partially
 * evaluates to plain nested closures.
 */
transparent inline def Control[M[_, _, _]]: Control[M] =
  compiletime.summonInline[Control[M]]

/**
 * A /> R is Cont[A, R, R] — the ordinary continuation monad, "A
 * delivered into the answer R": the diagonal of the paramonad, and an
 * ordinary Monad via the bridge in Monad.scala. Handlers (F !> S) and
 * put live in this fragment.
 */
infix type />[A, R] = Cont[A, R, R]
/** what reset can delimit: the value and its inner answer coincide */
infix type ^[A, R] = Cont[A, A, R]
/** capture the current continuation (Danvy–Filinski, with answer-type modification) */
inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Freer.Op(f)
/** delimit: run the computation with the identity continuation */
inline def reset[A, R](c: A ^ R): R = c / identity
/** deliver a value as the answer directly, top-level alongside shift/reset (named `answer`, not `pure` or
 * `lift` — both already exist as top-level/wildcard-imported names elsewhere in the package and collide) */
inline def answer[A, R](a: A): A /> R = Freer.Pure(a)

/**
 * mark a call to a mutually-recursive function as a tail call, so `/`
 * trampolines it instead of nesting a JVM stack frame per call: the
 * thunk is not forced at construction, only when the runner's own
 * tailrec loop reaches this node (Freer's `Defer` case).
 * `answer` as the continuation costs nothing extra —
 * `Bind(Defer(t, answer), g)` rotates through the same `Defer` case as
 * any other continuation.
 */
inline def tailcall[A, S, R](thunk: => Cont[A, S, R]): Cont[A, S, R] =
  Cont.defer(() => thunk)(answer)

/**
 * The parameterised continuation monad: `Freer` (Freer.scala) whose
 * leaf is a FUNCTION of the continuation. `Cont[A, S, R]` computes A
 * and, applied by `/` to a continuation A => S, makes an answer R,
 * i.e. it means (A => S) => R. `Bind` is a data node and `resume`
 * rebalances left-nested binds in a tail-recursive loop, so running a
 * flatMap chain is stack-safe.
 */
type Cont[A, S, R] = Freer[Shift, A, S, R]

object Cont:
  /** a finished value (the `Freer.Pure` case, named where the 200-odd
   * call sites in this library already look for it) */
  def Pure[A, R](a: A): Cont[A, R, R] = Freer.Pure(a)

  /** a bind whose left side is deferred into the runner's own loop */
  def defer[A, B, S, T, R](thunk: () => Cont[A, T, R])(f: A => Cont[B, S, T]): Cont[B, S, R] =
    Freer.defer(thunk)(f)

/**
 * The leaf of `Cont`: a computation as a plain function of its
 * continuation — the shift of Danvy and Filinski, and the one leaf
 * that can ABSORB a continuation, because it is a function and can
 * compose with one. (`Free`'s leaf is an operation, which cannot;
 * that asymmetry is the whole difference between the two, see
 * specs/freer-base.md.)
 *
 * Opaque, so that this file owns the representation: outside it a
 * `Cont` is built by `shift`, `Cont.Pure` and the combinators, and
 * `Absorbed` below cannot be forged. The companion is also where the
 * extensions live, since it is in the implicit scope of
 * `Freer[Shift, …]`.
 */
opaque type Shift[A, S, R] = (A => S) => R

object Shift:

  /**
   * A leaf that has ALREADY absorbed one continuation.
   *
   * Absorption is a single bit, so the state is the CASE and there is
   * no depth field. `Shift` itself stays the raw function, so an
   * unabsorbed leaf is still `Op` plus the user's lambda and nothing
   * else — an enum here costs no allocation at all, because a case IS
   * a case class with the same two fields the classes had.
   *
   * WHY AN ENUM AND NOT TWO CLASSES (operator's proposal, measured
   * 2026-09-15, history.tsv `once-*`): with `apply` written ONCE here
   * instead of overridden per class, both cases share a vtable entry,
   * so the runner's `s(k)` has a single target and the JIT inlines it
   * instead of dispatching. That is worth 3.2–4.5% on every Fib lane —
   * the residual that survived five sessions and four refuted
   * structural theories (the `Op` wrapper, the absorption depth, the
   * runner's shape, the rotation's composition). What it is NOT is a
   * cure for megamorphism: splitting the call site to make it
   * bimorphic was measured in the same session and did nothing, 0.996
   * to 1.017 across the board. The cost was the number of call
   * TARGETS, not the number of receiver types.
   *
   * One measured price, on a synthetic lane only: alternating the two
   * cases at one site allocates +14 B/op (`leafMixed`), where a single
   * body seems to lose escape analysis that two bodies kept. No real
   * lane shows it — `statePara` 1.002, every Fib lane faster.
   *
   * WHY EXACTLY ONE absorption, swept rather than argued
   * (specs/freer-base.md Results; history.tsv `fuse0-*`, `fuse1-*`,
   * `freer0b-absorb-sweep`). Absorption itself pays 12–25%: turning it
   * off loses on every Fib lane. One step is the whole of that: a
   * budget of 1 equals the old 128 everywhere, inside the bars, with
   * allocation identical at every depth. And depth COSTS — `statePara`
   * reads 0.861 at depth 1 against 1.19 / 1.15 / 1.17 at 4 / 16 / 128,
   * because each further step nests one more closure call per run.
   * That lane has the sharpest response in the suite; price any change
   * here against it.
   */
  private enum Once[A, S, R] extends ((A => S) => R):
    /** flatMap's absorption: the continuation enters the leaf */
    case Absorbed[A, B, S, T, R](s: (A => T) => R, g: A => Cont[B, S, T]) extends Once[B, S, R]

    /**
     * the same for `map`, and it has to be its own case rather than
     * `Absorbed` over `a => Pure(f(a))`: that spelling allocates a
     * `Pure` per element at RUN time, which measured +24 B/op and
     * 8-19% on every Fib lane (specs/freer-base.md Results — the
     * generator maps once per element, so this is its hot path).
     */
    case Mapped[A, B, S, R](s: (A => S) => R, g: A => B) extends Once[B, S, R]

    /**
     * ONE body for both cases, which is the point of the enum: with
     * `apply` defined here rather than overridden per class, both
     * cases share a vtable entry, so the runner's call is monomorphic
     * and inlinable instead of bimorphic. The allocation is unchanged
     * — an enum case is a case class, two fields either way — and
     * `Shift` stays the raw function, so an unabsorbed leaf still
     * costs `Op` plus the user's lambda and nothing else.
     */
    def apply(k: A => S): R = this match
      case Absorbed(s, g) => s(a => run(g(a))(k))
      case Mapped(s, g) => s(a => k(g(a)))

  /**
   * flatMap, in prefix form. The extension below and the
   * `Control[Cont]` instance BOTH call this, so neither can resolve
   * into the other — the self-recursion that the ParaMonad bridge in
   * Monad.scala documents (extension syntax inside an override
   * resolves to the override being defined).
   */
  def bind[A, B, S, S2, R](c: Cont[A, S, R])(f: A => Cont[B, S2, S]): Cont[B, S2, R] =
    c match
      case Freer.Op(s) => s match
        // already absorbed one — see `Once` for why never twice
        case _: Once[?, ?, ?] => Freer.Bind(c, f)
        case _ => Freer.Op(Once.Absorbed(s, f))
      // Pure receivers build a node too: fusing `pure(a).flatMap(f)` at
      // CONSTRUCTION would run `def forever = pure(()).flatMap(_ =>
      // forever)` at construction and diverge (interpreter-optimization)
      case _ => Freer.Bind(c, f)

  /** map, absorbed in its own right — see `Mapped` */
  def mapped[A, B, S, R](c: Cont[A, S, R])(f: A => B): Cont[B, S, R] =
    c match
      case Freer.Op(s) => s match
        case _: Once[?, ?, ?] => Freer.Bind(c, a => Freer.Pure(f(a)))
        case _ => Freer.Op(Once.Mapped(s, f))
      case _ => Freer.Bind(c, a => Freer.Pure(f(a)))

  /** apply to a continuation, as the function (A => S) => R it means */
  def run[A, S, R](c: Cont[A, S, R])(k: A => S): R = step(c)(k)

  /**
   * The loop: rotation and elimination interleaved, as the old
   * `Cont./` had them. The single non-tail case re-enters through
   * `run`, because a shift's body may invoke its continuation, and
   * that frame is direct style's own cost rather than the runner's.
   *
   * WHY THIS IS NOT `Freer.resume` PLUS A THREE-CASE MATCH. `resume`
   * is leaf-agnostic, so it can only compose a rotated continuation as
   * a raw `Bind` node. For `Free` that IS the optimum — its `flatMap`
   * is `Bind`. For `Cont` it is not: composing through `bind` lets the
   * rotated continuation be ABSORBED by the leaf it lands on, which is
   * what the old runner's `f(_).flatMap(g)` did. So the two rotations
   * are not a duplicate: they COMPOSE DIFFERENTLY, each optimally for
   * its leaf, and `TestFreer`'s law is what keeps them equal where it
   * matters — every bind-tree shape agrees with the `Func` reference
   * carrier on the answer and on the order the effects happened in.
   *
   * MEASURED, and honestly: delegating to `resume` instead was tried
   * twice. The first time it looked 8–19% better, but that run still
   * had `map` going through ParaMonad's default; asked again with that
   * fixed, over four rounds, the two shapes are indistinguishable on
   * `relayForward` (1.093 vs 1.094) and disagree in DIRECTION on the
   * Fib lanes (fib10 0.911 vs 1.246, fib100 1.039 vs 0.891) — a JIT
   * inlining-shape lottery on a loaded box, not an effect. This shape
   * is kept because it is the one the old runner had and because it
   * wins `statePara` (0.845 vs 0.900), the lane with the sharpest
   * response in the suite.
   */
  @annotation.tailrec
  private def step[A, S, R](c: Cont[A, S, R])(k: A => S): R = c match
    case Freer.Pure(a) => k(a)
    case Freer.Op(s) => s(k)
    case Freer.Bind(Freer.Op(s), f) => s(x => run(f(x))(k))
    case Freer.Bind(Freer.Bind(a, f), g) => step(Freer.Bind(a, x => bind(f(x))(g)))(k)
    case Freer.Bind(Freer.Pure(a), f) => step(f(a))(k)
    case Freer.Defer(t, f) => step(Freer.Bind(t(), f))(k)
    case Freer.Bind(Freer.Defer(t, f), g) => step(Freer.defer(t)(x => bind(f(x))(g)))(k)

  extension [A, S, R](c: Cont[A, S, R])
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = bind(c)(f)
    def map[B](f: A => B): Cont[B, S, R] = mapped(c)(f)
    infix def /(k: A => S): R = run(c)(k)
    // NO `apply` extension: `c(k)` cannot mean this inside package okay
    // anyway, because Generate.scala's seed-side `apply` (`a(f)` for a
    // Loop body) is in LEXICAL scope and beats anything reachable
    // through the implicit scope of the receiver. It was a member of
    // the old `Cont` enum, where members simply won; as an extension it
    // would work outside this package and not inside it, which is worse
    // than not having it. `c / k` is the spelling, and it always was
    // the meaning — the old member's body was `this / k`.

/** the stack-safe data instance: the default carrier */
given Control[Cont] with
  override inline def pure[A, R](a: A): A /> R = Freer.Pure(a)
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Freer.Op(f)
  extension [A, S, R](m: Cont[A, S, R])
    // prefix form on purpose: `m / k` here would resolve to this very
    // override (see Shift.bind's comment)
    override inline infix def /(k: A => S): R = Shift.run(m)(k)
    override inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
      Shift.bind(m)(f)
    // `map` MUST be overridden, not left to ParaMonad's default: this
    // instance's extensions are what `c.map(f)` actually resolves to
    // (a top-level given is in lexical scope for the package, which
    // beats `object Shift` in the receiver's implicit scope), so the
    // default's `flatMap(x => pure(f(x)))` is what the generator was
    // paying a Pure per element for. `flatMap` never had the problem
    // because both roads lead to `Shift.bind`.
    override inline def map[B](f: A => B): Cont[B, S, R] = Shift.mapped(m)(f)

/**
 * The function encoding is the reference implementation of Control.
 * It is not stack-safe: flatMap nests closures (Cont is the safe one).
 * The choice mirrors Free vs Eff one level up: data for tools and
 * safety, functions for speed.
 */
type Func[A, S, R] = (A => S) => R

/** the reference function instance: fast, fused, not stack-safe */
given Control[Func] with
  override inline def pure[A, R](a: A): Func[A, R, R] = _(a)
  override inline def shift[A, S, R](f: (A => S) => R): Func[A, S, R] = f
  extension [A, S, R](m: Func[A, S, R])
    override inline infix def /(k: A => S): R = m(k)
    override inline def flatMap[B, S2](f: A => Func[B, S2, S]): Func[B, S2, R] =
      k => m(f(_)(k))
    // the same reason as Cont's: the default would build a `pure`
    // closure per element where composing with k is direct
    override inline def map[B](f: A => B): Func[B, S, R] = k => m(x => k(f(x)))
