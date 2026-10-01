package okay

import okay.Freer.{Return, Inject, Bind, Delay, Diag}
import scala.annotation.tailrec

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
inline def shift[A, S, R](inline f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
/** delimit: run the computation with the identity continuation */
inline def reset[A, R](c: A ^ R): R = c / identity
/**
 * The parameterised continuation monad, as a FACADE over the freer
 * tree: `Cont[A, S, R]` computes A and, applied by `/` to a
 * continuation A => S, makes an answer R — it means (A => S) => R.
 *
 * `S` and `R` are ON THE TREE (freer-base-step-extractor, 2026-09-29).
 * The tree is `Freer[Shift, S, R, A]`, the same `Return | Inject |
 * Bind | Delay` every effect program is made of, indexed by the answer
 * types: a `Bind` joins a left side answering `T => R` to a
 * continuation answering `S => T`, which is Danvy and Filinski's
 * answer-type modification — `PState` changing its state type,
 * `Loop`'s open recursion — written on the node. The signatures of
 * this companion (`shift`, `bind`, `run`) say the same thing the tree
 * does, and the runner below is typed by the GADT: no cast.
 *
 * WHY THE INDEXES WERE NOT ON THE TREE FOR A YEAR, and what changed
 * (specs/freer-base.md, the stage-1 refutation and "The dual placement,
 * LANDED"): an indexed `Bind` carries its left side's answer type, and
 * a pattern match makes that an existential — so every one of the
 * library's hundred-odd match sites on a `Free` would have seen a
 * continuation at an index no type could pin back, and the pinning
 * extractor stage 1 tried put its type variable only in the RESULT,
 * which dotty infers as `Nothing`. `Free.Bind` (Free.scala) puts it in
 * the PARAMETER, so the type test binds it, and answers the effect
 * tree's constant claim — every index `Unit` — once, for every site.
 * A protocol's state is on the nodes since indexed-effects (stage 2,
 * `okay.sql.TxOp`): the signature says the transition, the tree checks it.
 *
 * So `Cont` is `Free` with a function in the leaf and its answer types
 * carried where `Free` carries `Unit` — and, seen the other way, Free
 * is Cont whose shift body the handler chooses rather than the program.
 */
type Cont[A, S, R] = Cont.Rep[A, S, R]

object Cont:

  /**
   * The representation, opaque HERE rather than at top level — and
   * that placement is load-bearing, not style. A top-level `opaque
   * type` is transparent to its whole PACKAGE, so declared there a
   * `Cont` would still be plainly `Freer[Sig, S, R, A]` everywhere in
   * `okay`, and every extension written for a program carrier would
   * apply to it: Generate.scala's for-comprehension picked up
   * `Stream`'s `map`, which takes a function INTO a program. Inside an
   * object the scope is the object, which is what a facade needs.
   */
  opaque type Rep[A, S, R] = Freer[Sig, S, R, A]

  /** a finished value (named where the 200-odd call sites already look for it) */
  def Pure[A, R](a: A): Rep[A, R, R] = Return(a)

  /** a computation as a function of its continuation — the shift of
   * Danvy and Filinski. For the machine every one is ONE leaf, a
   * `shift0` to the run's root (`leaf`); `ContMacro` is an optimization
   * over it that picks, at compile time, how the body is given its `k`:
   * a tail body becomes the value it passes and captures nothing
   * (`tailShift`/`tailPure`), an answer-using body a program over the
   * lazy `k` (`lazyLeaf`), anything else the strict `k` (`shiftLeaf`). */
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ ContMacro.shift('f) }

  /** the leaf an opaque body becomes (one the macro can neither make a
   * value nor CPS-transform): a `shift0` to the run's root whose clause
   * runs the body as it is, given a STRICT `k` — the rest of the run
   * forced as a nested run (`force`) — its answer the value in the
   * root's place. An ordinary clause: the machine does not know it is
   * Cont's. The cast is the facade's erasure claim (`erased`), on the
   * user's body. */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] =
    val body = f.asInstanceOf[(Any => Any) => Any]
    leaf((k: K) => Return(body(Resumption(k))))

  /** THE LEAF, the one there is: a `shift0` to the run's root whose
   * clause takes the captured stack. `shiftLeaf` and `lazyLeaf` differ
   * only in the clause they hand it. */
  private def leaf[A, S, R](clause: K => P): Rep[A, S, R] =
    typed(Inject(Cont0.Shift0[NoEffect, Any, Any, Any, Any, Any](rootAt, clause, "Cont.shift")))

  /** a tail-shaped body `k => { stats; k(v) }`, as the value it passes:
   * `v` computed when the runner reaches it, in the runner's own loop.
   * `S <:< R` is what the body's own typing gave the macro — `k(v): S`
   * was its `R`, and `ContMacro` summons the evidence at the call site.
   * The value is a `Return(v)`, the leaf `k => k(v)` at answer type `S`,
   * owed as a `Cont[A, S, R]`: with `S <: R` every answer `k(v): S` IS
   * an `R`, so the node runs as the type claims. That is the facade's
   * erasure claim (`typed`) — the evidence is the parameter, so only a
   * body whose own typing gave `S <: R` reaches it (freer-consumed-index,
   * 2026-09-30, on why the invariant base cannot say it for free). */
  def tailShift[A, S, R](v: () => A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    typed(Freer.delay[Sig, Any, Any, Any](() => Return[Sig, Any, Any](v())))

  /** the same when `v` is a literal or a stable name and nothing runs
   * before it: no thunk at all */
  def tailPure[A, S, R](v: A)(using @annotation.unused ev: S <:< R): Rep[A, S, R] =
    typed(Return[Sig, Any, Any](v))

  /**
   * LAYER 1 B (specs/cont-stack.md plan stage E, cont-stack-layer1-b):
   * a body that USES the answer of `k` — `k(1) + k(10)`, `a :: k(x)`,
   * `s"${k(a)}"` — cannot become a value the way a tail body does:
   * something is left to do after each call. `ContMacro` CPS-transforms
   * such a body SELECTIVELY (Rompf, Maier & Odersky, ICFP 2009): every
   * `k(e)` becomes `Cont.call(k, e, rest)` — the PROGRAM over a lazy `k`
   * (cont-frames-strict-k): `k`'s nodes pushed by the frame machine,
   * `rest` the frame under them, and the body's answer `Cont.done`. No
   * body frame, no room counted, no switch, `k` multi-shot as before.
   * Until then the macro built a `Body` tree (`Done`/`Call`) inside an
   * anonymous `Cps` class, and the runner converted it to these nodes on
   * every call — the old runner's walked data, kept past the runner.
   *
   * NOT a function answer (`PState`'s `s => k(s)(s2)`): measured at
   * 2.8x the direct road on statePara on the old runner (specs/
   * cont-stack.md, stage E); such a body stays the opaque leaf.
   *
   * Public because a macro expansion at the user's call site builds it;
   * not an API.
   */
  opaque type Lazy[R] = Freer[Sig, Any, Any, Any]

  /** the body's answer */
  def done[R](r: R): Lazy[R] = Return(r)

  /** `k(a)`, then `rest` of its answer: `k` is the captured stack (a
   * lazy leaf's body is only ever given one), applied as a program the
   * machine pushes, `rest` the bind's continuation */
  def call[A, S, R](k: A => S, a: A, rest: S => Lazy[R]): Lazy[R] =
    Bind(k.asInstanceOf[K](a), rest.asInstanceOf[Any => P])

  /** the leaf an answer-using body becomes */
  def lazyLeaf[A, S, R](body: (A => S) => Lazy[R]): Rep[A, S, R] =
    // the body IS the clause: given the captured stack as its `k`
    leaf(body.asInstanceOf[K => P])
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
    Freer.defer(thunk)(f)

  /** `defer` with nothing to do afterwards — `Free.delay` on this
   * side, and the same reason: `defer(t)(Return)` would push a rotated
   * `Return` continuation down the deferred subprogram (delay-node) */
  def delay[A, S, R](thunk: () => Rep[A, S, R]): Rep[A, S, R] = Freer.delay(thunk)

  /**
   * flatMap, in prefix form. The extension below and the
   * `Control[Cont]` instance BOTH call this, so neither can resolve
   * into the other — the self-recursion that the ParaMonad bridge in
   * Monad.scala documents (extension syntax inside an override
   * resolves to the override being defined).
   */
  def bind[A, B, S, S2, R](c: Rep[A, S, R])(f: A => Rep[B, S2, S]): Rep[B, S2, R] = Bind(c, f)

  /** map: `Freer.map`'s node, the `Mapped` continuation a builder can read */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] = c.map(f)

  /** apply to a continuation, as the function (A => S) => R it means */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R =
    val r = Root(k.asInstanceOf[Any => Any], StackSwitch.firstRoom)
    answerOf(Frames.run[NoEffect, Any, Any, Any](Inject(Cont0.Dollar0[NoEffect, Any, Any, Any, Any](rootAt, r, erased(c))))).asInstanceOf[R]

  // ==================================================================
  // THE RUNNER IS THE FRAME MACHINE (cont-step-on-frames, 2026-09-30;
  // specs/freer-kont.md, stage 3). A Cont program is a `Cont0` program
  // of one ROOT prompt: `run` installs a `Dollar` of it whose `ret` is
  // the user's `k`, and every leaf is a `Shift0` to the nearest — the
  // run it is in, since a `k(x)` re-installs its root with it. Two roads,
  // the ones the macro already separates:
  //   an answer-using body (`lazyLeaf`, the macro's selective CPS transform:
  //     `k(1) + k(10)`) is a program over a LAZY `k` — `call(k, a, rest)`
  //     is `k(a).flatMap(rest)`, pushed by the machine, no JVM frame;
  //   an opaque body (`k` where the macro cannot see it) gets a STRICT
  //     `k`: a `Resumption`, whose `apply` is a nested run to a value —
  //     direct style's own cost — at one level less of room, moved to a
  //     fresh stack at zero (`StackSwitch`).
  // What this replaces: `step`, the `Reentry` chain, the `Pending` stack
  // of Layer 1 B, and the absorbed leaves — one machine for Delim and
  // Cont, where there were two.
  // ==================================================================

  /** a Cont program has no effect but its own */
  private[okay] type NoEffect = [S, R, X] =>> Nothing
  private[okay] type Sig = Cont0.Row[NoEffect]

  private type P = Freer[Sig, Any, Any, Any]
  private type K = Stack[NoEffect, Any, Any, Any, Any]

  /** THE ROOT PROMPT: one for every run — runs nest by the stack, and a
   * leaf's cut stops at the nearest delimiter of it */
  private val root: Prompt[Any] = new Prompt[Any]("Cont.run", "Cont.scala")
  /** the root as the machine's delimiter: at Cont's one index, `Any` —
   * every node of a Cont program is built at it (`erased`) */
  private val rootAt: Cont0.Delimiter[Any, Any] = Cont0.delimiter(root)

  /** the root's `ret`: the user's `k`, and the room this run has on its
   * stack — where a strict `k` reads it (`force`) */
  private final class Root(val k: Any => Any, var room: Int) extends (Any => P):
    def apply(x: Any): P = Return(k(x))

  /** THE STRICT `k` an opaque body is given: a function `A => S` whose
   * `apply` runs the captured stack to its value — the machine's own
   * loop over `k`'s frames, however many (a trampoline), nested in the
   * body's JVM frame, so counted (`force`) */
  private final class Resumption(k: K) extends (Any => Any):
    def apply(x: Any): Any = force(k, x)

  /**
   * THE CLAIM of this runner, and its argument. Cont's answer types are
   * the FACADE's: `Rep` is opaque and every node is built in this
   * companion, so a Cont program is a `Cont0` program at erased
   * indexes. A body answering `R` where its `k` answers `S` (answer-type
   * modification with escape) stands in the root's place, which is the
   * run's answer: the erasure is the whole of it — the machine's join
   * index cannot carry an escape type, and Cont's strict `k` is where it
   * is real (specs/freer-kont.md, Results).
   */
  private def erased[A, S, R](c: Rep[A, S, R]): P = c.asInstanceOf[P]
  private def typed[A, S, R](p: P): Rep[A, S, R] = p.asInstanceOf[Rep[A, S, R]]

  /** a run's head form, which for a Cont program is its value: it has no
   * operation but its own, so the machine answers a `Return` */
  private def answerOf(head: P): Any = head match
    case Return(v) => v
    case _ => throw IllegalStateException("a Cont program answered an operation: it has none")

  /**
   * THE STRICT `k`: the rest of the run, run NOW to its value — nested,
   * so counted. The root of `k` carries the room of the run it was
   * captured from; the nested run gets one level less, and at zero the
   * rest runs on a fresh stack (`StackSwitch.fresh`). The bound is the
   * level count: a fixed room per stack, written in `StackSwitch`.
   */
  private def force(k: K, x: Any): Any =
    val r = rootOf(k)
    // the room is the RUN's, scoped dynamically around the nested run:
    // forces nest strictly (a nested run returns before its caller goes
    // on, on this stack or on a fresh one it waits for), so a saved value
    // restored in `finally` is exact — where rebuilding `k` with the room
    // in its root cost a node walk and two nodes a call
    val here = r.room - 1
    if here > 0 then nested(r, here, k, x)
    else StackSwitch.fresh(fresh => nested(r, fresh, k, x))

  /** `k` run nested with `room` levels, the run's own room restored after */
  private def nested(r: Root, room: Int, k: K, x: Any): Any =
    val saved = r.room
    r.room = room
    try enter(k, x) finally r.room = saved

  /** `k` run now to its value: the resumption `k(x)`, run */
  private def enter(k: K, x: Any): Any =
    answerOf(Frames.run[NoEffect, Any, Any, Any](k(x)))

  /** the root delimiter at the bottom of `k`: the run it was captured
   * from. Always there: a strict `k` is made only by a leaf, and a leaf
   * cuts to its run's root, so `k` ends at it */
  @annotation.tailrec
  private def rootOf(k: Stack[NoEffect, ?, ?, ?, ?]): Root = k match
    case Stack.Dollar(p, r: Root, _) if p eq root => r
    case Stack.Dollar(_, _, below) => rootOf(below)
    case Stack.Run(_, below) => rootOf(below)
    case c: Stack.Cat[NoEffect, ?, ?, ?, ?, ?, ?] @unchecked => rootOf(Frames.uncat(c))
    case _ => throw IllegalStateException("a strict k without its run's root: only a leaf makes one, and a leaf cuts to the root")


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
  // the LEAF, not the macro: this override implements an abstract
  // method, so Scala 3 keeps a non-inline retained body for it, and a
  // macro expanded here — in the file that defines the types
  // ContMacro reads — made a suspension cycle ("stale symbol Cont$",
  // every compile, clean or not). `f` is a plain parameter the macro
  // could not read anyway.
  override inline def shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Cont.shiftLeaf(f)
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

/**
 * THE MACHINE'S CONTINUATION STACK AS A FREER (specs/freer-kont.md;
 * freer-kont-frames-probe and freer-kont-migrate, 2026-09-30; the stack
 * SEGMENTED at its delimiters by cont-step-on-frames the same day).
 *
 * Two type-aligned lists, both continuations, both joined as `Bind`
 * joins its indexes. `Frames` is the frames of ONE segment — the binds
 * pushed since the last delimiter. `Stack` is the whole stack: segments
 * (`Run`) and the delimiters between them (`Dollar`) — "`Frames` of
 * segments". Dybvig, Peyton Jones and Sabry's monadic framework (JFP
 * 2007): a continuation is a list of segments split at prompts, so a
 * capture to the NEAREST prompt takes the current segment as it is and
 * a resumption pushes segments, and neither copies a frame.
 *
 * WHY SEGMENTS, measured: the single list before this cut had to COPY
 * the frames between a capture and its delimiter (an immutable list
 * cannot be cut at a node in the middle), so n captures from a deep
 * stack were O(n²) — Cont's 20 000 shifts in a row under one root
 * delimiter; and a resumption copied its segment onto the stack, so n
 * NESTED resumptions were O(n²) too (TestContMacro's SmallStack test
 * spun ten minutes on the probe). Now a capture copies node SHELLS for
 * the delimiters it crosses and a resumption for the nodes of `k` —
 * usually two — and frames are shared, never copied.
 *
 * The idiom is `Freer.Mapped`'s: a function that knows what it is,
 * sitting in a `Bind` as an ordinary `A => Freer`, callable as one,
 * and taken apart by the one loop that knows the class — `Stack`'s; a
 * segment is only ever a register or a field, never a continuation. `Freer` knows
 * nothing of either; this is a second interpreter, orthogonal to the
 * tree (the operator's ask). THE INDEX IS THE JOIN'S, NOT A STACK OF
 * PROMPTS: Delim.Stacked types a program by the prompts installed
 * around it in its DOORS, with `rebase` as their claim (specs/freer-kont.md,
 * Results).
 */
enum Frames[F[_, _, +_], A, S, T, Z]:
  /** the empty segment: the identity, on the diagonal like `Return` */
  case End[F[_, _, +_], A, S]() extends Frames[F, A, S, S, A]

  /** a frame and the rest of the segment: `f` consumes `T`, produces
   * `S2`; the rest goes from `S2` to `S` — `Bind`'s join */
  case Frame[F[_, _, +_], A, S, S2, T, Y, Z](f: A => Freer[Cont0.Row[F], S2, T, Y],
                                              rest: Frames[F, Y, S, S2, Z]) extends Frames[F, A, S, T, Z]

/**
 * THE STACK: segments and the delimiters between them, the same join —
 * Dybvig, Peyton Jones & Sabry's `Seq = EmptyS | PushSeg | PushP`, one
 * constructor a role (cont-core-design). A captured `k` is one of these
 * — `Run(frames, Dollar(p, ret, Done))` for a capture to the nearest
 * delimiter — and so is the continuation
 * of a head form. Applied as a plain function by ANY interpreter of the
 * tree it answers a `Delay` whose thunk runs the machine on itself with
 * `a` at its top — the continuation carries its own interpreter (the
 * operator's ask) — and the machine, meeting that `Delay`, pushes its
 * nodes instead of forcing it: a clause's `k(x)` is lazy, so 100 000
 * resumptions from 100 000 clauses nest no JVM frame (TestKont).
 */
enum Stack[F[_, _, +_], A, S, T, Z] extends (A => Freer[Cont0.Row[F], S, T, Z]):
  /** the bottom: the identity */
  case Done[F[_, _, +_], A, S]() extends Stack[F, A, S, S, A]

  /** a segment of frames, then the rest */
  case Run[F[_, _, +_], A, S, S2, T, Y, Z](frames: Frames[F, A, S2, T, Y],
                                            below: Stack[F, Y, S, S2, Z]) extends Stack[F, A, S, T, Z]

  /**
   * THE DELIMITER, `ret $ ·` (Materzok & Biernacki, APLAS 2012): `ret`
   * on the stack with the prompt beside it, so a body that returns runs
   * `ret` by the ordinary pop, OUTSIDE the delimiter — the `($v)` rule —
   * and a `shift0` cuts the stack at it, so its `k` carries `ret` — the
   * `($/S0)` rule. A plain `reset` is `pure $ ·`. Not `⟨ret ·⟩`: that
   * runs `ret` inside, where a capture in `ret` to the same prompt would
   * find it. A program asks for one with the OPERATION `Cont0.Dollar0`,
   * never by building this node: an operation passes through a handler
   * loop between the program and the machine, a node on the machine's
   * stack does not.
   *
   * The segment waiting for its answer is the `Run` BELOW it, not a
   * field of it: until cont-core-design a delimiter headed its segment,
   * which saved a node per install and made every rule that touches a
   * delimiter touch a segment too. A separate PLAIN node (DPJS's
   * `PushP`, no `ret` to call) stood beside this one while `control`
   * needed it typed; with `control` gone it is an optimization, and
   * returns only with a number.
   */
  case Dollar[F[_, _, +_], A, S, T, Y, Z](p: Cont0.Delimiter[Y, T], ret: A => Freer[Cont0.Row[F], T, T, Y],
                                          below: Stack[F, Y, S, T, Z]) extends Stack[F, A, S, T, Z]

  /**
   * A RESUMPTION'S STACK, O(1): `k` then `below` — the catenation that
   * van der Ploeg & Kiselyov's type-aligned sequences give a free monad
   * ("Reflection without Remorse", Haskell 2014), on the machine's
   * stack. Resuming `k` over the live registers is this one node, where
   * it was `k`'s nodes reversed and relinked (two objects a node). The
   * machine takes it apart only when it reaches it (`Frames.uncat`), one
   * node of `k` at a time — so a `k` resumed and dropped early costs
   * nothing for the nodes it never reached. A capture walks through it
   * the same way, so a captured `k` never holds one.
   */
  case Cat[F[_, _, +_], A, S, T, Y, S2, Z](k: Stack[F, A, S2, T, Y],
                                           below: Stack[F, Y, S, S2, Z]) extends Stack[F, A, S, T, Z]

  def apply(a: A): Freer[Cont0.Row[F], S, T, Z] = this match
    case Done() => Return(a)
    case _ => Delay(Frames.Resume(a, this))

object Frames:
  import Stack.{Done, Run, Dollar, Cat}

  /**
   * A RESUMPTION `k(a)`, as the thunk of a `Delay`. Two readers, two
   * meanings, and that is why it is a class and not a `Bind(Return(a),
   * k)`: the machine recognises it and PUSHES `k`'s nodes (rule 2), so
   * a resumption inside a run stays in the run; an outer interpreter
   * that is handed `k` (the head form's continuation) forces it, which
   * runs a machine on `k` — where a bare `Bind(Return(a), k)` would be
   * `k(a)` again under `Freer.resume`, forever.
   */
  final class Resume[F[_, _, +_], A, S, T, Z](val a: A, val k: Stack[F, A, S, T, Z]) extends (() => Freer[Cont0.Row[F], S, T, Z]):
    def apply(): Freer[Cont0.Row[F], S, T, Z] =
      Frames.run[F, S, T, Z](Bind[Cont0.Row[F], S, T, T, A, Z](Return[Cont0.Row[F], T, A](a), k))

  /**
   * THE EMPTY SEGMENT AND THE EMPTY STACK, ONE OBJECT EACH. `End()` and
   * `Done()` are cases with a parameter list, so every call would build
   * one — and the loop makes one at every delimiter it pops and every
   * stack it splices (measured: part of step 1's first 2x on
   * DelimBenchmark). They hold no field, so the indexes are phantom and
   * one value serves every one of them, as `Nil` serves every `List`:
   * the cast is that sentence.
   */
  private val theEnd: End[Nothing, Any, Any] = End()
  private val theDone: Done[Nothing, Any, Any] = Done()
  def noFrames[F[_, _, +_], A, S]: Frames[F, A, S, S, A] = theEnd.asInstanceOf[Frames[F, A, S, S, A]]
  def noStack[F[_, _, +_], A, S]: Stack[F, A, S, S, A] = theDone.asInstanceOf[Stack[F, A, S, S, A]]

  /**
   * THE TWO CLASS TESTS of the machine, and the claim they make: a
   * stack sitting as a `Bind`'s continuation, or a `Resume` as a
   * `Delay`'s thunk, is typed by that node
   * — the function type IS its type, so the test on the class is the
   * whole test (`Free.Bind`'s constant claim, in the same spirit).
   * `null` for any other function.
   */
  private[okay] def as[F[_, _, +_], A, S, T, Z](f: A => Freer[Cont0.Row[F], S, T, Z]): Stack[F, A, S, T, Z] = f match
    // ONE test per bind: a segment never stands as a continuation (a
    // captured `k` and a head form's continuation are both `Stack`s), so
    // `Frames` is not a function and there is no second class to tell
    // apart — the `Known` marker and its nested test were step 1e's
    // answer to a question this removes
    case st: Stack[?, ?, ?, ?, ?] => st.asInstanceOf[Stack[F, A, S, T, Z]]
    case _ => null

  private def resume[F[_, _, +_], S, T, Z](t: () => Freer[Cont0.Row[F], S, T, Z]): Resume[F, ?, S, T, Z] = t match
    case r: Resume[?, ?, ?, ?, ?] => r.asInstanceOf[Resume[F, ?, S, T, Z]]
    case _ => null

  /** TWO DELIMITERS THAT ARE ONE OBJECT ARE ONE TYPE — answer and index
   * alike: `Same.byIdentity`'s axiom, the generative prompt's (DPJS's
   * `eqPrompt` is the same `unsafeCoerce`), taken here without its
   * `Option` and without the lazy given behind `Delim.samePrompt` (3.4%
   * of a capture-heavy lane's samples, read off async-profiler): the
   * caller tests `eq`, this is the claim for the pair it tested */
  private final class Ident[Y, I, Y2, I2](val answer: Y =:= Y2, val index: I =:= I2)
  private val theSame = new Ident[Any, Any, Any, Any](<:<.refl, <:<.refl)
  private def identical[Y, I, Y2, I2](@annotation.unused a: Cont0.Delimiter[Y, I], @annotation.unused b: Cont0.Delimiter[Y2, I2]): Ident[Y, I, Y2, I2] =
    theSame.asInstanceOf[Ident[Y, I, Y2, I2]]

  private[okay] def runOf[F[_, _, +_], A, S, S2, T, Y, Z](fs: Frames[F, A, S2, T, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = fs match
    case _: End[F, A, S2] @unchecked => st
    case _ => Run(fs, st)

  /**
   * A stack whose head is a `Cat`, as one whose head is not: `k`'s head
   * node over `k`'s rest catenated with `below` — a shell for that one
   * node, the rest still a `Cat` until it is reached. Left-nested `Cat`s
   * re-associate to the right, one step a turn of this loop.
   */
  @tailrec private[okay] def uncat[F[_, _, +_], A, S, T, Z](st: Stack[F, A, S, T, Z]): Stack[F, A, S, T, Z] = st match
    case c: Cat[F, A, S, T, y, s2, Z] => c.k match
      case _: Done[F, A, T] @unchecked => uncat(c.below)
      case r: Run[F, A, `s2`, ?, T, ?, `y`] => Run(r.frames, Cat(r.below, c.below))
      case d: Dollar[F, A, `s2`, T, y1, `y`] => Dollar(d.p, d.ret, Cat(d.below, c.below))
      case i: Cat[F, A, `s2`, T, ?, ?, `y`] => uncat(Cat(i.k, Cat(i.below, c.below)))
    case _ => st

  /** the prompts installed on a stack, innermost first — `NoPrompt`'s list */
  @tailrec def installed[F[_, _, +_]](st: Stack[F, ?, ?, ?, ?], acc: List[String] = Nil): List[String] = st match
    case c: Cat[F, ?, ?, ?, ?, ?, ?] @unchecked => installed(uncat(c), acc)
    case Run(_, below) => installed(below, acc)
    case Dollar(p, _, below) => installed(below, if p eq Cont0.boundary[Any, Any] then acc else p.label :: acc)
    case _ => acc.reverse

  /**
   * The one loop: run `p` to a head form — `Return(x)`, or `Bind(Inject(e), k)`
   * for the first operation no delimiter on the stack answers, `k` the
   * stack itself (which re-enters this loop when applied). Three
   * registers: the program in focus, the frames of the current segment,
   * and the stack below them. `S0`, `R`, `Z` are the run's; every arm is
   * typed by GADT refinement.
   */
  def run[F[_, _, +_], S0, R, Z](p: Freer[Cont0.Row[F], S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] = machine[F, S0, R, Z](p)

  /** the machine: a program over the empty registers */
  private def machine[F[_, _, +_], S0, R, Z](focus0: Freer[Cont0.Row[F], S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    type G = Cont0.Row[F]

    final class Next[X, T, S1, Y](val focus: Freer[G, T, R, X], val fs: Frames[F, X, S1, T, Y], val st: Stack[F, Y, S0, S1, Z])

    /** cut the stack at the delimiter naming `sh.p`, walking its NODES:
     * `k` is the nodes above it with it, the body takes the delimiter's
     * place with the stack below it */
    @tailrec def cut[X, Y, I, T, T2, C](sh: Cont0.Shift0[F, Y, I, T, R, X], all: Stack[F, X, S0, T, Z], st: Stack[F, C, S0, T2, Z], rev: Rev[F, X, T, T2, C]): Next[?, ?, ?, ?] = st match
      // no delimiter answers, and no boundary: the capture goes OUT as an
      // operation, for a machine outside this one (Delim.runNested)
      case Done() => null
      case c: Cat[F, C, S0, T2, ?, ?, Z] => cut(sh, all, uncat(c), rev)
      case r: Run[F, C, S0, ?, T2, ?, Z] => cut(sh, all, r.below, Rev.SnocRun(rev, r.frames))
      // THE BARRIER (Flatt, Yu, Findler & Felleisen, ICFP 2007's
      // continuation barrier): `Delim.run`'s root delimiter, which no
      // capture may cross. It is a node of the stack, so it travels in
      // every `k` and a run of `k` started by an outer interpreter meets
      // it too — which is why the check is the machine's and not the
      // door's: the door returns before those runs happen
      case d: Dollar[F, C, S0, T2, ?, Z] if d.p eq Cont0.boundary[Any, Any] => throw NoPrompt(sh.at, sh.p.label, installed(all))
      case d: Dollar[F, C, S0, T2, y2, Z] =>
        if sh.p eq d.p then
          // the delimiter's own answer and index ARE the leaf's `Y` and
          // `I`: the shift named this delimiter, which carries both
          val same = identical(sh.p, d.p)
          val y = same.answer.flip
          val i = same.index.flip
          // the nodes WITH the delimiter: `k` carries `ret` (the $/S0 rule)
          val k = y.liftCo[[a] =>> Stack[F, X, I, T, a]](
            i.liftCo[[t] =>> Stack[F, X, t, T, y2]](Rev.link(Rev.SnocDollar(rev, d.p, d.ret), noStack[F, y2, T2])))
          Next[Y, I, I, Y](sh.f(k), noFrames[F, Y, I],
            y.liftCo[[a] =>> Stack[F, a, S0, I, Z]](i.liftCo[[t] =>> Stack[F, y2, S0, t, Z]](d.below)))
        else
          cut(sh, all, d.below, Rev.SnocDollar(rev, d.p, d.ret))

    /** an operation of `F`, not of `Cont0`: what the head form hands out */
    def foreign(a: Freer[G, ?, ?, ?]): Boolean = a match
      case Inject(e) => !e.isInstanceOf[Cont0[?, ?, ?, ?]]
      case _ => false

    /** a capture: the walk to the delimiter it names; `null` when no
     * delimiter on this machine answers it */
    def capture[X, T, S1, Y](sh: Cont0.Shift0[F, ?, ?, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Next[?, ?, ?, ?] =
      val all = runOf(fs, st)
      cut(sh, all, all, Rev.nil[F, X, T])

    /** a resumption's registers: `focus` over the stack `k`'s nodes were
     * catenated onto (`Rev.onto`), its head segment unpacked into the frames
     * register — the one place both resuming arms (a `k` as a bind's
     * continuation, a `Resume` as a delay's thunk) go through */
    def pushed[X, T](focus: Freer[G, T, R, X], k: Stack[F, X, S0, T, Z]): Next[?, ?, ?, ?] = uncat(k) match
      case rn: Run[F, X, S0, s2, T, y, Z] => Next[X, T, s2, y](focus, rn.frames, rn.below)
      case sp => Next[X, T, T, X](focus, noFrames[F, X, T], sp)

    @tailrec def loop[X, T, S1, Y](focus: Freer[G, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Freer[G, S0, R, Z] = focus match
      case b: Bind[G, T, ?, R, ?, X] => Frames.as(b.f) match
        case null => b.a match
          // a value under a Bind: apply, no frame — what `Freer.resume`'s
          // `Bind(Return(a), f) => f(a)` does, and a right-nested chain is
          // nothing else (measured: +24 B and 1.26x a step without this)
          case r: Return[G, R, x0] => loop(b.f(r.a), fs, st)
          // the head form already — an operation nobody here answers,
          // nothing pushed: hand it back as it is, no push (and no tuple
          // to ask: measured, a `(fs, st) match` here built one per bind)
          case a => fs match
            case _: End[F, X, S1] @unchecked => st match
              case _: Done[F, Y, S0] @unchecked if foreign(a) => focus
              case _ => loop(a, Frame(b.f, fs), st)
            case _ => loop(a, Frame(b.f, fs), st)
        // a stack as the continuation — a resumed `k`, or the head form
        // fed back: its nodes pushed, never its frames copied
        case ks =>
          val n = pushed(b.a, Rev.onto(ks, fs, st))
          loop(n.focus, n.fs, n.st)
      case r: Return[G, R, X] => fs match
        case fr: Frame[F, X, S1, s2, T, ?, Y] => loop(fr.f(r.a), fr.rest, st)
        case _: End[F, X, S1] @unchecked => st match
          // the ($v) rule: a `$` is popped like any frame, its `ret` applied
          case d: Dollar[F, Y, S0, S1, y, Z] => loop(d.ret(r.a), noFrames[F, y, S1], d.below)
          // the next segment, unpacked into the frames register
          case rn: Run[F, Y, S0, ?, S1, ?, Z] => loop(focus, rn.frames, rn.below)
          // a resumed `k` over the rest: its next node, reached now
          case c: Cat[F, Y, S0, S1, ?, ?, Z] => loop(focus, fs, uncat(c))
          case _: Done[F, Y, S0] @unchecked => focus
      case d: Delay[G, T, R, X] => Frames.resume[F, T, R, X](d.thunk) match
        // a resumption: the value at the top of its stack, pushed — never forced
        case null => loop(d.thunk(), fs, st)
        case r: Resume[F, a, T, R, X] =>
          val n = pushed(Return[G, R, a](r.a), Rev.onto(r.k, fs, st))
          loop(n.focus, n.fs, n.st)
      // the operations, in the loop: a `Next` per `Dollar0` and a union
      // result's type test cost the first cut of step 1 its install lane.
      // Either node carries one — `Diag` is `Inject` on the diagonal, its
      // `T = R` refined by the match — and the machine treats them alike:
      // the operation is read out by the same two class tests the two
      // arms used to make, then dispatched once
      case _ =>
        val e: G[T, R, X] = (focus: @unchecked) match
          case i: Inject[G, T, R, X] => i.a
          case i: Diag[G, R, X] => i.a
        e match
          // the delimiter, asked for as an operation, becomes a node — entered
          case ds: Cont0.Dollar0[F, X, a, T, R] @unchecked =>
            loop[a, T, T, a](ds.body, noFrames[F, a, T], Dollar(ds.p, ds.ret, runOf(fs, st)))
          case sh: Cont0.Shift0[F, ?, ?, T, R, X] @unchecked => capture(sh, fs, st) match
            case n: Next[x, ?, ?, ?] => loop(n.focus, n.fs, n.st)
            // nobody here answers it: out, as an operation over the stack
            case null => Bind(focus, runOf(fs, st))
          case _ => Bind(focus, runOf(fs, st))

    loop[Z, S0, S0, Z](focus0, noFrames[F, Z, S0], noStack[F, Z, S0])

/**
 * THE TWO OPERATIONS, λ$'s two (Materzok & Biernacki): the delimiter
 * and the capture, both on the join index of the run. `Dollar0` is
 * `ret $ body`: the body at `T`, `ret` at `T` too, the delimiter
 * answering `Y`; `reset` is `pure $ body`. `Shift0`'s `f` takes the
 * stack up to and including the delimiter naming `p` — a `Stack[F, X,
 * I, T, Y]`, from the operation's value `X` through the delimiter to
 * its answer `Y`, at the delimiter's index `I` — and answers a program
 * that stands in the delimiter's place. `shift`, `abort` and `reset`
 * are derived below. `control`/`control0` (a `k` WITHOUT the delimiter)
 * were here until cont-core-design: no module used them, and their
 * one consumer, `Lexical.shallow`, went with them. An enum, for the
 * `+X` a `Freer` signature needs.
 */
enum Cont0[F[_, _, +_], T, R, +X]:
  case Dollar0[F[_, _, +_], Y, A, T, R](p: Cont0.Delimiter[Y, T],
                                        ret: A => Freer[Cont0.Row[F], T, T, Y],
                                        body: Freer[Cont0.Row[F], T, R, A]) extends Cont0[F, T, R, Y]
  case Shift0[F[_, _, +_], Y, I, T, R, X](p: Cont0.Delimiter[Y, I],
                                          f: Stack[F, X, I, T, Y] => Freer[Cont0.Row[F], I, R, Y],
                                          at: String) extends Cont0[F, T, R, X]

object Cont0:
  /** the row: `Cont0` beside any indexed signature `F`; the effect tree's
   * signature is `Freer.Lift[Fx]` here */
  type Row[F[_, _, +_]] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

  /** a fresh delimiter tag, labelled with the line that asked for it */
  def prompt[Y](using at: At): Prompt[Y] = new Prompt[Y]("prompt", at.where)

  /**
   * A PROMPT AS THE MACHINE SEES IT: the delimiter's answer `Y` AND the
   * index `I` it is installed at (cont-core-design step 9). A capture
   * names its delimiter by this, so finding the `Dollar` by identity
   * types both — the `k` it cuts and the stack the body runs on — where
   * the machine used to claim the leaf's index for the delimiter's
   * (`rebase`). The prompt itself, so `eq`, `label` and every
   * `Prompt[Y]` use stay as they were.
   */
  opaque type Delimiter[Y, I] <: Prompt[Y] = Prompt[Y]

  /**
   * THE CLAIM, made where the index is KNOWN: "this prompt's delimiter is
   * installed at `I`". A door knows it by construction — every unstacked
   * Delim program is at `Unit` (`Delim.atUnit`), every Cont program at
   * `Any`, a stacked prompt's delimiter at the stack below it (`Has`) —
   * and says so once, at the door, instead of the machine claiming at
   * every capture that two indexes it cannot relate are one.
   */
  def delimiter[Y, I](p: Prompt[Y]): Delimiter[Y, I] = p

  /** THE BOUNDARY: a root delimiter nobody can name, installed by
   * `Delim.run`. A cut that walks into it has passed every delimiter of
   * the machine and found none: `NoPrompt`, with the ones it passed. A
   * run without it (`Delim.runNested`) lets such a capture out as an
   * operation, for a machine outside to answer. */
  private val theBoundary = new Prompt[Any]("boundary", "Delim.run")
  /** at any answer type and index: it is compared by `eq` and never
   * answers anything */
  def boundary[Y, I]: Delimiter[Y, I] = theBoundary.asInstanceOf[Delimiter[Y, I]]

  /** `ret $ body`: the body under the delimiter — an operation, so it
   * reaches the machine through any handler loop between them */
  def dollar[F[_, _, +_], Y, A, T, R](p: Delimiter[Y, T])(ret: A => Freer[Row[F], T, T, Y])(body: Freer[Row[F], T, R, A]): Freer[Row[F], T, R, Y] =
    Inject[Row[F], T, R, Y](Cont0.Dollar0[F, Y, A, T, R](p, ret, body))

  /** `⟨body⟩`: reset, the PLAIN delimiter — `pure $ body` (λ$'s own
   * definition). The lambda captures nothing, so it is one object per
   * call site, not one per reset */
  def reset[F[_, _, +_], T, R, A](p: Delimiter[A, T])(body: Freer[Row[F], T, R, A]): Freer[Row[F], T, R, A] =
    Inject[Row[F], T, R, A](Cont0.Dollar0[F, A, A, T, R](p, (a: A) => Return[Row[F], T, A](a), body))

  def shift0[F[_, _, +_], Y, I, T, R, X](p: Delimiter[Y, I])(f: Stack[F, X, I, T, Y] => Freer[Row[F], I, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, Y, I, T, R, X](p, f, at.where))

  /** `shift`: `shift0` whose body runs under a fresh plain delimiter of
   * the same prompt, `k` still carrying `ret` — APLAS 2012's
   * `S k.e = S0 k.⟨e⟩`, derived, not a case of the machine */
  def shift[F[_, _, +_], Y, I, T, R, X](p: Delimiter[Y, I])(f: Stack[F, X, I, T, Y] => Freer[Row[F], I, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    shift0[F, Y, I, T, R, X](p)(k => reset[F, I, R, Y](p)(f(k)))

  /** leave the delimiter with a value: a shift0 that drops `k` — a
   * `Return` in the delimiter's place, so at the diagonal */
  def abort[F[_, _, +_], Y, T, X](p: Delimiter[Y, T])(value: Y)(using At): Freer[Row[F], T, T, X] =
    shift0[F, Y, T, T, T, X](p)(_ => Return[Row[F], T, Y](value))

/**
 * The reversed stack of NODES: the same type-aligned discipline,
 * outermost first. A cut walks the nodes down to the delimiter building
 * one of these and links it onto `Done` — O(delimiters and segments
 * crossed), frames shared; a resumption reverses `k`'s nodes and links
 * them onto the live stack — O(nodes of `k`), usually two.
 */
private enum Rev[F[_, _, +_], A, T, S2, Y]:
  case Nil[F[_, _, +_], A, T]() extends Rev[F, A, T, T, A]
  case SnocRun[F[_, _, +_], A, T, S3, S2, Y0, Y](prev: Rev[F, A, T, S3, Y0], frames: Frames[F, Y0, S2, S3, Y]) extends Rev[F, A, T, S2, Y]
  case SnocDollar[F[_, _, +_], A, T, S2, Y0, Y](prev: Rev[F, A, T, S2, Y0], p: Cont0.Delimiter[Y, S2], ret: Y0 => Freer[Cont0.Row[F], S2, S2, Y]) extends Rev[F, A, T, S2, Y]

private object Rev:
  @tailrec def link[F[_, _, +_], A, S, T, S2, Y, Z](rev: Rev[F, A, T, S2, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = rev match
    case Nil() => st
    case SnocRun(prev, frames) => link(prev, Stack.Run(frames, st))
    case SnocDollar(prev, p, ret) => link(prev, Stack.Dollar(p, ret, st))

  /** the empty prefix, one object (as `Frames.noFrames`: no field, phantom indexes) */
  private val theNil: Nil[Nothing, Any, Any] = Nil()
  def nil[F[_, _, +_], A, T]: Rev[F, A, T, T, A] = theNil.asInstanceOf[Rev[F, A, T, T, A]]

  /**
   * A RESUMPTION onto the live registers — the segment and the stack
   * under it: ONE `Cat`, `k` over them, O(1); `k`'s nodes are reached
   * one at a time as the machine pops (`Frames.uncat`), frames shared.
   */
  def onto[F[_, _, +_], A, S, T, S2, Y, S1, W, Z](ks: Stack[F, A, S2, T, Y], fs: Frames[F, Y, S1, S2, W], st: Stack[F, W, S, S1, Z]): Stack[F, A, S, T, Z] = fs match
    // an EMPTY machine first: a `k` re-entering from outside (a handler of
    // another effect resuming the head form it was handed) is pushed onto
    // nothing, so it IS the stack; the two tests refine the indexes
    case _: Frames.End[F, Y, S1] @unchecked => st match
      case _: Stack.Done[F, W, S] @unchecked => ks
      case _ => Stack.Cat(ks, Frames.runOf(fs, st))
    case _ => Stack.Cat(ks, Frames.runOf(fs, st))
