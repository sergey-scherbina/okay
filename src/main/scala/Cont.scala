package okay

import okay.Free.{Return, Inject, Bind, Delay}

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
inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
/** delimit: run the computation with the identity continuation */
inline def reset[A, R](c: A ^ R): R = c / identity
/**
 * The parameterised continuation monad, as a FACADE over the freer
 * tree: `Cont[A, S, R]` computes A and, applied by `/` to a
 * continuation A => S, makes an answer R — it means (A => S) => R.
 *
 * `S` and `R` are PHANTOM to the tree. The tree is `Free[Shift, A]`,
 * the same `Return | Inject | Bind | Delay` every effect program is
 * made of, and it carries no answer type at all. Danvy and Filinski's
 * answer-type modification — `PState` changing its state type,
 * `Loop`'s open recursion — lives entirely in the signatures of this
 * companion: `shift`, `bind`, `run` say what the tree may not.
 *
 * WHY THE INDEXES ARE NOT ON THE TREE (specs/freer-base.md, the
 * stage-1 refutation): they were, and it cost the shared base its
 * other half. An indexed `Bind` carries its left side's answer type,
 * and a pattern match makes that an existential — so every one of
 * the library's 89 match sites on a `Free` would have seen a
 * continuation at an index no type could pin back, and a pinning
 * extractor is refuted by the compiler inferring its free parameter
 * as `Nothing`. Indexes belong on facades, which are never matched.
 * That is also what lets stage 2 put a protocol state on an effect
 * program: the same move, one more facade.
 *
 * So `Cont` is `Free` with a function in the leaf and its types on
 * the outside — and, seen the other way, Free is Cont whose shift
 * body the handler chooses rather than the program.
 */
type Cont[A, S, R] = Cont.Rep[A, S, R]

object Cont:

  /**
   * The leaf, as the tree stores it: a shift with its answer types
   * FORGOTTEN. `(X => S) => R <: (X => Nothing) => Any` for every S and
   * R — a function is contravariant in its argument and `Nothing <: S`,
   * covariant in its result and `R <: Any` — so this is the one
   * supertype every typed shift conforms to, and it is written HERE
   * and nowhere else. (Any other spelling fails on that variance: a
   * wildcard is a supertype, and the argument slot needs a subtype of
   * every `X => S` — measured by compiling, specs/freer-base.md.)
   *
   * Why the types cannot stay on the leaf while `Cont` is `Free`:
   * `Free.Bind` joins a left tree and a continuation over ONE `F`, and
   * answer-type modification joins a `(X => S) => R` on the left with
   * a `(X => S2) => S` on the right — so the tree's `F` cannot name S
   * and R, and the facade's signatures are the only place they live.
   * Three spellings the operator asked about, settled by scalac 3.9.9
   * on 2026-09-15 (cont-shift-doors): `(X => ?) => Any` is REFUSED
   * ("Found: (X => S) => R, Required: Shift[X]" — a wildcard is a
   * supertype, and the argument slot needs a subtype); a binary
   * `Shift[+X, -S] = (X => S) => Any` compiles, but the tree can only
   * hold it at `S = Nothing` (the two sides of a `Bind` disagree on S
   * under answer-type modification), so the second parameter would be
   * decoration; and a leaf enum `case Shift[A, S, R](k: (A => S) => R)`
   * reads best and changes nothing — S and R are existential under the
   * tree's wildcard, the cast stays, and a raw shift gains a wrapper
   * object that stage 0 measured and refused (`once-*`).
   *
   * So the type has two named doors, and the ugly spelling is behind
   * them: `Shift.of` forgets (an upcast, free), `at` remembers (THE
   * cast, below).
   */
  private[okay] type Shift[+X] = (X => Nothing) => Any

  // Not `private`: `shift` is inline and reaches this object, and a
  // private member behind an inline body makes the compiler
  // synthesize an accessor with an unstable name (E192 — the
  // `DiagonalMonad` finding, Effects.scala); `private[okay]` still
  // does, measured 2026-09-15. The alias above is package-private so
  // `of`'s signature may name it; `Rep` stays opaque, so nothing
  // outside this companion can put a leaf in a `Cont` anyway.
  object Shift:
    /** the door in: a typed shift, its answer types forgotten — an
     * upcast, no cast at all */
    inline def of[X, S, R](f: (X => S) => R): Shift[X] = f

    extension [X](s: Shift[X])
      /**
       * THE ONE CAST, and what makes it right: the door out.
       *
       * The facade typed this leaf when it was built — `shift(f: (X =>
       * S) => R)` — and every combinator since has threaded those types
       * through its own signature, so a runner that has been handed a
       * `Cont[A, S, R]` and a `k: A => S` knows the leaf it reaches is
       * the function the facade said it was. Nothing else can put a
       * leaf in a `Cont`: the alias is opaque and this companion is the
       * only place that sees through it. The cast is erased on the JVM
       * and costs nothing at run time; what it costs is that this line,
       * and no other, is where the answer-type discipline is trusted
       * rather than checked. Same standing as `Writer`'s phantom
       * equation and `Delim`'s two claims.
       */
      inline def at[S, R](k: X => S): R = s.asInstanceOf[(X => S) => R](k)

  /**
   * The representation, opaque HERE rather than at top level — and
   * that placement is load-bearing, not style. A top-level `opaque
   * type` is transparent to its whole PACKAGE, so declared there a
   * `Cont` would still be plainly `Free[Shift, A]` everywhere in
   * `okay`, and every extension written for a program carrier would
   * apply to it: Generate.scala's for-comprehension picked up
   * `Stream`'s `map`, which takes a function INTO a program, and the
   * absorption below would have been bypassed wholesale. Inside an
   * object the scope is the object, which is what a facade needs.
   */
  opaque type Rep[A, S, R] = Free[Shift, A]

  import Shift.at

  /** a finished value (named where the 200-odd call sites already look for it) */
  def Pure[A, R](a: A): Rep[A, R, R] = Free.Return(a)

  /** a computation as a function of its continuation — the shift of
   * Danvy and Filinski, through `Shift.of` */
  inline def shift[A, S, R](f: (A => S) => R): Rep[A, S, R] = Free.Inject(Shift.of(f))

  /**
   * A bind whose LEFT side is deferred into the runner's own loop: the
   * thunk is not forced at construction, only when `step` reaches the
   * node — which is what lets two mutually recursive functions call
   * each other in tail position without a JVM frame per call
   * (the codecs' trampolines past `NativeThreshold` are its heaviest
   * users; a call with nothing to do afterwards is `delay`). It is
   * `Bind(Delay(t), f)` on the tree, so the continuation is an ordinary
   * bind and rotates like one.
   */
  def defer[A, B, S, T, R](thunk: () => Rep[A, T, R])(f: A => Rep[B, S, T]): Rep[B, S, R] =
    Free.defer(thunk)(f)

  /** `defer` with nothing to do afterwards — `Free.delay` on this
   * side, and the same reason: `defer(t)(Return)` would push a rotated
   * `Return` continuation down the deferred subprogram (delay-node) */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Free.delay(thunk)

  /**
   * A leaf that has ALREADY absorbed one continuation.
   *
   * Absorption is a single bit, so the state is the CASE and there is
   * no depth field. An enum here costs no allocation — a case IS a
   * case class with the same two fields — and `apply` written ONCE
   * gives both cases a shared vtable entry, so the runner's call has a
   * single target the JIT inlines. That was worth 3.2–4.5% on every
   * Fib lane against two classes with two bodies (history.tsv
   * `once-*`), while making the same call site merely bimorphic was
   * worth nothing: the JIT counts call TARGETS, not receiver types.
   *
   * WHY EXACTLY ONE absorption, swept rather than argued (history.tsv
   * `fuse0-*`, `fuse1-*`, `freer0b-absorb-sweep`): absorption itself
   * pays 12–25%, one step is the whole of that, and depth COSTS —
   * `statePara` reads 0.861 at depth 1 against 1.15–1.19 deeper,
   * because each further step nests one more closure call per run.
   * That lane has the sharpest response in the suite; price any
   * change here against it.
   *
   * The type parameters are the facade's, and inside this companion
   * `Cont[B, S, T]` is plainly `Free[Shift, B]` whatever S and T are,
   * so a `bind` may build an `Absorbed` at whatever indexes inference
   * finds — the tree will not remember them and the facade already
   * checked them.
   */
  private enum Leaf[A, S, R] extends ((A => S) => R):
    /** flatMap's absorption: the continuation enters the leaf */
    case Absorbed[A, B, S, T, R](s: (A => T) => R, g: A => Rep[B, S, T]) extends Leaf[B, S, R]

    /**
     * the same for `map`, its own case rather than `Absorbed` over
     * `a => Return(f(a))`: that spelling allocates a `Pure` per element
     * at RUN time, which measured +24 B/op and 8-19% on every Fib lane
     * — the generator maps once per element, so this is its hot path.
     */
    case Mapped[A, B, S, R](s: (A => S) => R, g: A => B) extends Leaf[B, S, R]

    def apply(k: A => S): R = applyAt(k, roomOf(k))

    /** the leaf re-enters the runner with the room the runner has left
     * (specs/stack-safety.md stage 1c) */
    def applyAt(k: A => S, room: Int): R = this match
      case Absorbed(s, g) => s(Reentry(g, k, room - 1))
      case Mapped(s, g) => s(a => callK(k, g(a), room - 1))

  /**
   * flatMap, in prefix form. The extension below and the
   * `Control[Cont]` instance BOTH call this, so neither can resolve
   * into the other — the self-recursion that the ParaMonad bridge in
   * Monad.scala documents (extension syntax inside an override
   * resolves to the override being defined).
   */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] =
    c match
      case Inject(s) => s match
        // already absorbed one — see `Leaf` for why never twice
        case _: Leaf[?, ?, ?] => Bind(c, f)
        case _ => Inject(Leaf.Absorbed(s, f))
      // Pure receivers build a node too: fusing `pure(a).flatMap(f)` at
      // CONSTRUCTION would run `def forever = pure(()).flatMap(_ =>
      // forever)` at construction and diverge (interpreter-optimization)
      case _ => Bind(c, f)

  /** map, absorbed in its own right — see `Mapped` */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] =
    c match
      case Inject(s) => s match
        case _: Leaf[?, ?, ?] => Bind(c, a => Free.Return(f(a)))
        case _ => Inject(Leaf.Mapped(s, f))
      case _ => Bind(c, a => Free.Return(f(a)))

  /** apply to a continuation, as the function (A => S) => R it means */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R = step(c)(k)(StackSwitch.firstRoom)

  /**
   * THE CONTINUATION A SHIFT'S BODY RECEIVES, when calling it re-enters
   * this runner — and the room left on this stack, carried as a FIELD
   * rather than in a ThreadLocal (specs/stack-safety.md stage 1c).
   *
   * Direct style's own cost: a body gets `k`'s VALUE, so `k` runs the
   * rest of the program inside the body's call, and shifts in a row
   * nest one level each. `room` counts the levels this stack still
   * takes; at zero the rest continues on a FRESH stack
   * (`StackSwitch.fresh`) and this one waits for its answer. No
   * exception unwinds anything and nothing runs twice: the body's frame
   * simply stays where it is, on the stack below, until the answer
   * comes back.
   */
  private final class Reentry[X, B, S, T](f: X => Rep[B, S, T], k: B => S, val room: Int) extends (X => T):
    def apply(x: X): T = enter(x, room)

    /** enter from a place with `here` levels of room left: a
     * continuation may be called DEEPER than it was made (the runner
     * hands an answer to an outer continuation from inside an inner
     * segment), so the room is the smaller of the two */
    def enter(x: X, here: Int): T =
      val r = math.min(here, room)
      if r > 0 then step(f(x))(k)(r)
      else StackSwitch.fresh(fresh => step(f(x))(k)(fresh))

  /** call a continuation from inside the runner, with the room HERE */
  private def callK[A, S](k: A => S, a: A, room: Int): S = k match
    case r: Reentry[x, ?, ?, t] => r.enter(a, room)
    case _ => k(a)

  /** the room a continuation carries: a runner's own, or the first
   * room when a user's function calls in */
  private def roomOf(k: Any): Int = k match
    case r: Reentry[?, ?, ?, ?] => r.room
    case _ => StackSwitch.firstRoom

  /**
   * Is this program already an ANSWER — and if so, continue on the
   * answer itself instead of applying a continuation to it.
   *
   * `c / k` and `ifAnswer(a)` agree for an answer by the runner's own
   * `Return` case, so this decides nothing about meaning; what it
   * decides is who builds the continuation. A handler that does NOT
   * capture builds exactly `Return` — every comonadic handler does, and
   * they are the overwhelming majority — and a caller that can go on
   * from the answer with a tail call then needs no trampoline node at
   * all. `Effects.handle` is that caller, and the node it avoids was
   * measured at 59 µs and 730 328 B on a 10 000-operation program
   * (handle-forward-fast): a `Defer` whose continuation is `Return`
   * rotates into a LEFT-nested `Bind`, and left-nesting is the one
   * shape this tree rewrites, so every following operation pays.
   *
   * Inline with inline branches, like `split`, so neither arm costs a
   * closure or an `Option` on the hot path.
   *
   * It matches the representation, which the facade's own rule
   * forbids — "facades are never matched". The rule is about matching
   * a facade from OUTSIDE, where the tree is opaque and the indexes
   * are a claim nobody can check; here the companion looks at its own
   * tree, which is the one place that is allowed to.
   */
  inline def onAnswer[A, S, B](c: Rep[A, S, S])(inline ifAnswer: A => B)
                                               (inline otherwise: => B): B =
    c match
      case Return(a) => ifAnswer(a)
      case _ => otherwise

  /** `Shift.at`'s claim at the other node the tree cannot type: a `Return`
   * reached through a `Cont[A, S, R]` was built by `Cont.Pure[A, R']`,
   * whose signature is `Cont[A, R', R']` — so the facade already fixed
   * S = R' = R, and the tree, which keeps no answer type, cannot say
   * it. Both claims are the one invariant named above. */
  private inline def pinned[S, R](s: S): R = s.asInstanceOf[R]

  /**
   * The loop: rotation and elimination interleaved, as the original
   * `Cont./` had them. The single non-tail case re-enters through
   * `run`, because a shift's body may invoke its continuation, and
   * that frame is direct style's own cost rather than the runner's.
   *
   * Composing the rotated continuation through `bind` rather than a
   * raw `Bind` lets it be absorbed by the leaf it lands on; measured
   * to move nothing on the Fib lanes (their programs are right-nested,
   * so the case barely fires) and kept because it is the shape the
   * original runner had. Delegating to `!.resume` and a three-case
   * match instead is indistinguishable on `relayForward` and loses
   * `statePara` (0.845 vs 0.900) — history.tsv `freer0b-runner-shape`.
   */
  /**
   * `Shift.at`, with the runner's room handed to an absorbed leaf. A
   * leaf re-enters the runner through ITS continuation, not the one it
   * is given, so the room has to reach it here: read off the
   * continuation it would only ever see the program's outermost one,
   * and shifts in a row would never count down (measured: a 20 000
   * shift program overflowed with the room in `k` alone).
   *
   * The cast is `Shift.at`'s own claim at the leaf's class: a leaf is
   * built only by `bind`/`mapped`, at the facade's indexes.
   */
  private inline def leafAt[X, S, R](s: Shift[X], k: X => S, room: Int): R = s match
    case l: Leaf[?, ?, ?] => l.asInstanceOf[Leaf[X, S, R]].applyAt(k, room)
    case _ => s.at[S, R](k)

  @annotation.tailrec
  private def step[A, S, R](c: Rep[A, S, R])(k: A => S)(room: Int): R = c match
    case Return(a) => pinned[S, R](callK(k, a, room))
    case Inject(s) => leafAt[A, S, R](s, k, room)
    // the leaf's inner answer is the Bind's existential — `Any` names
    // "whatever it is". Left to inference it came out `Nothing`, and a
    // lambda whose body is typed `Nothing` carries a checkcast to
    // Nothing$ that throws (ClassCastException: null, four TestFree
    // rotation laws, 2026-09-15). `typed(s)(...)` never hit this only
    // because its two argument lists resolved the variable differently.
    case Bind(Inject(s), f) => s.at[Any, R](Reentry(f, k, room - 1))
    case Bind(Bind(a, f), g) => step(Bind(a, x => bind(f(x))(g)))(k)(room)
    case Bind(Return(a), f) => step(f(a))(k)(room)
    case Delay(t) => step(t())(k)(room)
    case Bind(Delay(t), g) => step(Bind(t(), g))(k)(room)

  extension [A, S, R](c: Cont[A, S, R])
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = bind(c)(f)
    def map[B](f: A => B): Cont[B, S, R] = mapped(c)(f)
    infix def /(k: A => S): R = run(c)(k)
    // NO `apply` extension: `c(k)` cannot mean this inside package okay
    // anyway, because Generate.scala's seed-side `apply` (`a(f)` for a
    // Loop body) is in LEXICAL scope and beats anything reachable
    // through the implicit scope of the receiver. `c / k` is the
    // spelling, and it always was the meaning.

  /**
   * THE SAME WORD, INSIDE A DIRECT BLOCK (cont-in-direct, 2026-09-17).
   *
   * `import Cont.direct.*` in a block whose monad is Cont's
   * DIAGONAL — `[X] =>> Cont[X, R, R]`, the case where the answer type
   * does not move — and a capture takes ONE type argument, the value's:
   * the answer type comes from the block, exactly as `Delim.shift`'s
   * comes from its `Prompted` evidence.
   *
   * ITS OWN SCOPE, not an overload of the package-level `shift`, and
   * that is measured, not taste: a call with NO type arguments —
   * `shift: k => ...`, the shape every handler in this library writes,
   * `runChoice` included — resolves to the one-argument alternative and
   * then fails for want of a `DirectCtx`. An import is the opt-in.
   *
   * A block that MOVES the answer type is not spellable here, and not
   * for want of a name: `Direct.direct` is diagonal — one `F[A]` for
   * the whole block — while answer-type modification gives every step
   * its own F. That shape stays in `for`, with the expected type on
   * the `reset`.
   */
  object direct:

    /**
     * The answer type of a block whose monad is the diagonal. Evidence,
     * not a row member: `Cont` is a type alias, not an effect, so there
     * is nothing to put in a row — and it carries `apply` so the
     * diagonal is re-associated by TYPING it, with no cast.
     */
    trait AnswerOf[F[_]]:
      type R
      def apply[A](c: Cont[A, R, R]): F[A]

    object AnswerOf:
      given [R0]: AnswerOf[[X] =>> Cont[X, R0, R0]] with
        type R = R0
        def apply[A](c: Cont[A, R0, R0]): Cont[A, R0, R0] = c

    /**
     * Capture the rest of the block up to its `reset`. `A` is the only
     * type argument; the answer type is the block's own, and the
     * `DirectCtx` is what makes this an error outside a direct block.
     * The `DummyImplicit` is the generalized-method-syntax tax: a
     * `using` clause between two type clauses is what makes the first
     * one mandatory, so that `shift[Int]` names A and not F.
     */
    inline def shift[A](using d: DummyImplicit)[F[_]]
                       (using inline ctx: DirectCtx[F])
                       (using a: AnswerOf[F])
                       (f: (A => a.R) => a.R): F[A] =
      a(okay.shift[A, a.R, a.R](f))

  /**
   * Monadic reflection (Filinski, "Representing Monads", POPL 1994):
   * with delimited control, ANY monad runs in direct style — `reflect`
   * delivers the A of an F[A] as a plain value, `reify` delimits a
   * block back into F. Answer-type modification types the construction
   * precisely: a reflected F[A] is Cont[A, F[B], F[B]] — "A now, F[B]
   * eventually" — and multi-shot comes for free, because the captured
   * continuation is a pure closure that F's own flatMap may call once
   * (Option), many times (List, Logic), or not at all (None is an
   * abort).
   *
   * The names are Filinski's, and they live in a NESTED object for the
   * same reason `direct` above does: the package-level `reflect`/
   * `reify` (Effects.scala) already name the encoding round-trip — a
   * different construction that happens to deserve the same words —
   * and `Direct.scala` already has its own `.reflect`/`.?` marks over
   * a different receiver. Nesting under `Cont`, not flattening into
   * it, is what keeps all three apart: each needs its own import.
   */
  object Monadic:

    extension [F[_] : Monad, A](m: F[A])
      /** μ: the monadic value as a direct value — one definition, both
       * spellings: `m.reflect` and `reflect(m)` (an extension is a
       * method; the prefix form is its desugared call) */
      inline def reflect[B]: Cont[A, F[B], F[B]] =
        shift(k => m.flatMap(k))
      /** the symbolic μ — the same glyph as Direct's mark and as
       * `Throws.?` (specs/unwrap-glyph.md): the value, the context deals
       * with what was around it */
      inline def ?[B]: Cont[A, F[B], F[B]] = reflect[B]

    /** the delimiter: a direct-style block back into its monad */
    inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
      p / (a => M.pure(a))


/** the stack-safe data instance: the default carrier */
given Control[Cont] with
  override inline def pure[A, R](a: A): A /> R = Cont.Pure(a)
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
  extension [A, S, R](m: Cont[A, S, R])
    // prefix form on purpose: `m / k` here would resolve to this very
    // override (see Cont.bind's comment)
    override inline infix def /(k: A => S): R = Cont.run(m)(k)
    override inline def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] =
      Cont.bind(m)(f)
    // `map` MUST be overridden, not left to ParaMonad's default: this
    // instance's extensions are what `c.map(f)` actually resolves to
    // (a top-level given is in lexical scope for the package, which
    // beats the companion in the receiver's implicit scope), so the
    // default's `flatMap(x => pure(f(x)))` is what the generator was
    // paying a Pure per element for.
    override inline def map[B](f: A => B): Cont[B, S, R] = Cont.mapped(m)(f)

/**
 * The function encoding is the reference implementation of Control.
 * It is not stack-safe: flatMap nests closures (Cont is the safe one).
 * The choice mirrors Free vs an inline handler-passing program one
 * level up: data for tools and safety, functions for speed.
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
