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
   * Danvy and Filinski. A body that only calls `k` in tail position is
   * rewritten at compile time to the value it passes (`ContMacro`,
   * specs/cont-stack.md Layer 1 A); any other body is `shiftLeaf`. */
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ ContMacro.shift('f) }

  /** the leaf an opaque body becomes (one the macro can neither make a
   * value nor CPS-transform): the body runs as it is, given a STRICT `k`
   * — the rest of the run forced as a nested run (`force`) */
  def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] =
    leaf(k => Return(f.asInstanceOf[(Any => Any) => Any](x => force(k, x))))

  /** a tail-shaped body `k => { stats; k(v) }`, as the value it passes:
   * `v` computed when the runner reaches it, in the runner's own loop.
   * `S <:< R` is what the body's own typing gave the macro — `k(v): S`
   * was its `R`, and `ContMacro` summons the evidence at the call site.
   * THE ONE CAST on the Cont side, and its argument (freer-consumed-index,
   * 2026-09-30): the value is a `Return(v): Cont[A, S, S]`, the leaf
   * `k => k(v)` at answer type `S`, owed as a `Cont[A, S, R]`. With
   * `S <: R` every answer `k(v): S` IS an `R`, so the node runs as the
   * type claims; the base used to be covariant in `R` and `liftCo`
   * said the same thing for free, and invariance — which lets the tree
   * carry a CONSUMED index (Free.scala's header) — took that road away.
   * Isolated here and in `tailPure`, the evidence as the parameter. */
  def tailShift[A, S, R](v: () => A)(using ev: S <:< R): Rep[A, S, R] =
    tailAt[A, S, R](Freer.delay[Sig, S, S, A](() => Return[Sig, S, A](v())))

  /** the same when `v` is a literal or a stable name and nothing runs
   * before it: no thunk at all */
  def tailPure[A, S, R](v: A)(using ev: S <:< R): Rep[A, S, R] =
    tailAt[A, S, R](Return[Sig, S, A](v))

  /** the cast, once: a `Cont[A, S, S]` whose every answer is an `S`
   * conforms to `Cont[A, S, R]` when `S <: R` — the evidence is the
   * parameter, so no caller can reach this without it */
  private def tailAt[A, S, R](c: Rep[A, S, S])(using S <:< R): Rep[A, S, R] =
    c.asInstanceOf[Rep[A, S, R]]

  /**
   * LAYER 1 B (specs/cont-stack.md plan stage E, cont-stack-layer1-b):
   * a body that USES the answer of `k` — `k(1) + k(10)`, `a :: k(x)`,
   * `s"${k(a)}"` — cannot become a value the way a tail body does:
   * something is left to do after each call. `ContMacro` CPS-transforms
   * such a body SELECTIVELY (Rompf, Maier & Odersky, ICFP 2009): every
   * `k(e)` becomes a `Call` naming what is left. The body is then DATA,
   * turned into a program over a LAZY `k` (`bodyProgram`): a call of `k`
   * is `k`'s nodes pushed by the frame machine, its rest a frame under
   * them. No body frame, no room counted, no switch, `k` multi-shot as
   * before. (Until cont-on-frames-probe the old runner walked it with a
   * `Pending` stack of its own; the frame machine's stack is that stack.)
   *
   * NOT a function answer (`PState`'s `s => k(s)(s2)`): measured at
   * 2.8x the direct road on statePara (specs/cont-stack.md, stage E),
   * where Layer 3's exact room already runs the same program with no
   * switch; such a body stays the opaque leaf.
   *
   * A `Cps` is a `Shift` on the tree, extending the function type
   * exactly as `Leaf` does so `Inject` takes it; applied as one by code
   * that is not the runner, it runs the same loop from the top. Public
   * because a macro expansion at the user's call site builds it (an
   * anonymous subclass, one allocation per shift); not an API.
   */
  enum Body[R]:
    /** the body's answer */
    case Done[R](r: R) extends Body[R]
    /** `k(a)`, then `rest` of the answer */
    case Call[A, S, R](k: A => S, a: A, rest: S => Body[R]) extends Body[R]

  /** a CPS-transformed body, as the `(A => S) => R` it still means */
  abstract class Cps[A, S, R] extends ((A => S) => R):
    def body(k: A => S): Body[R]
    final def apply(k: A => S): R = runBody[R](body(k))

  /** the leaf an answer-using body becomes */
  def cps[A, S, R](c: Cps[A, S, R]): Rep[A, S, R] = leaf(k => bodyProgram(c.asInstanceOf[Cps[Any, Any, Any]].body(k)))
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
    answerOf(Frames.runUnder[NoEffect, Any, Any](erased(c), root, Root(k.asInstanceOf[Any => Any], StackSwitch.firstRoom))).asInstanceOf[R]

  // ==================================================================
  // THE RUNNER IS THE FRAME MACHINE (cont-step-on-frames, 2026-09-30;
  // specs/freer-kont.md, stage 3). A Cont program is a `Cont0` program
  // of one ROOT prompt: `run` installs a `Reset` of it whose `ret` is
  // the user's `k`, and every leaf is a `Shift0` to the nearest — the
  // run it is in, since a `k(x)` re-installs its root with it. Two roads,
  // the ones the macro already separates:
  //   an answer-using body (`Cps`, the macro's selective CPS transform:
  //     `k(1) + k(10)`) is a program over a LAZY `k` — `Call(k, a, rest)`
  //     is `k(a).flatMap(rest)`, pushed by the machine, no JVM frame;
  //   an opaque body (`k` where the macro cannot see it) gets a STRICT
  //     `k`: `x => force(k, x)`, a nested run to a value — direct style's
  //     own cost — at one level less of room, moved to a fresh stack at
  //     zero (`StackSwitch`, Layer 3).
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
   * leaf's cut stops at the nearest `Reset` of it */
  private val root: Prompt[Any] = new Prompt[Any]("Cont.run", "Cont.scala")

  /** the root's `ret`: the user's `k`, and the room this run has on its
   * stack — where a strict `k` reads it (`force`) */
  private final class Root(val k: Any => Any, var room: Int) extends (Any => P):
    /** this run's stack gauge, attached at its first exhaustion (Layer 3):
     * one per run, so a grant reads the stack it measured last, where a
     * fresh gauge per exhaustion asked the OS for the stack every time */
    private var gauge: Gauge | Null = null
    def gaugeNow: Gauge = gauge match
      case null => val g = Gauge(); gauge = g; g
      case g: Gauge => g
    def apply(x: Any): P = Return(k(x))

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

  /** a leaf: a `Shift0` to the root, its clause given the stack up to it */
  private def leaf[A, S, R](clause: K => P): Rep[A, S, R] =
    typed(Inject(Cont0.Shift0[NoEffect, Any, Any, Any, Any](root, clause, false, false, "Cont.shift")))

  /** road 2: a transformed body as a program over the lazy `k` */
  private def bodyProgram(b: Body[?]): P = b match
    case Body.Done(r) => Return(r)
    case Body.Call(kk, a, rest) =>
      val next: Any => P = s => bodyProgram(rest.asInstanceOf[Any => Body[?]](s))
      Frames.as[NoEffect, Any, Any, Any, Any](kk.asInstanceOf[Any => P]) match
        case null => Delay(() => next(kk.asInstanceOf[Any => Any](a)))
        case ks => Bind(ks(a), next)

  /** a walked body, applied as the function it means: its program run */
  private def runBody[R](b: Body[R]): R = value(bodyProgram(b)).asInstanceOf[R]

  /** a run to its value: a Cont program has no other effect, so the head
   * form is a `Return` */
  private def value(p: P): Any = answerOf(Frames.run[NoEffect, Any, Any, Any](p))

  /** a run's head form, which for a Cont program is its value: it has no
   * operation but its own, so the machine answers a `Return` */
  private def answerOf(head: P): Any = head match
    case Return(v) => v
    case _ => throw IllegalStateException("a Cont program answered an operation: it has none")

  /**
   * THE STRICT `k`: the rest of the run, run NOW to its value — nested,
   * so counted. The root of `k` carries the room of the run it was
   * captured from; the nested run gets one level less, and at zero the
   * stack is asked (Layer 3), and a stack with none left switches.
   */
  private def force(k: K, x: Any): Any =
    val r = rootOf(k)
    if r eq null then enter(k, x)
    else
      // the room is the RUN's, scoped dynamically around the nested run:
      // forces nest strictly (a nested run returns before its caller
      // goes on, on this stack or on a fresh one it waits for), so a
      // saved value restored in `finally` is exact — where rebuilding `k`
      // with the room in its root cost a node walk and two nodes a call
      val here = r.room - 1
      if here > 0 then nested(r, here, k, x)
      else
        val more = StackSwitch.more(r.gaugeNow)
        if more > 0 then nested(r, more, k, x)
        else StackSwitch.fresh(fresh => nested(r, fresh, k, x))

  /** `k` run nested with `room` levels, the run's own room restored after */
  private def nested(r: Root, room: Int, k: K, x: Any): Any =
    val saved = r.room
    r.room = room
    try enter(k, x) finally r.room = saved

  /** `k` run now to its value, entered at the machine's registers */
  private def enter(k: K, x: Any): Any = answerOf(Frames.enterAt[NoEffect, Any, Any, Any, Any](x, k))

  /** the root delimiter at the bottom of `k`: the run it was captured from */
  @annotation.tailrec
  private def rootOf(k: Stack[NoEffect, ?, ?, ?, ?]): Root | Null = k match
    case Stack.Kept(_, p, r: Root, _) if p eq root => r
    case Stack.Reset(p, r: Root, _, _, below) if p eq root => r
    case Stack.Reset(_, _, _, _, below) => rootOf(below)
    case Stack.Run(_, below) => rootOf(below)
    case _ => null

  /**
   * What a run's stack looked like at its last GRANT (specs/cont-stack.md
   * Layer 3): the stack it was on, the stack pointer then, the levels
   * granted, and the most bytes one level has ever taken in this run.
   * `StackSwitch.more` reads the pointer again at the next exhaustion,
   * and the difference over the levels between is a measured
   * bytes-per-level — opaque bodies' frames included — kept as a
   * maximum, since a body deeper down may be fatter than the ones seen.
   * `worst` starts at the cold constant (interpreted frames) and only
   * rises. A different `top` means a different stack (a segment thread,
   * or a virtual thread moved to another carrier): the mark is dropped.
   *
   * One per run, and NOTHING allocated for it until a run's first
   * exhaustion (plan stage C, C1): the gauge is attached THEN, to the
   * run's root delimiter (`Root.gauge`), and found there at every
   * exhaustion after. The first cut wrapped the
   * user's `k` at `run`, two allocations for every run whether or not
   * it ever went deep, and fib100 (a run per element) paid +1 664 B/op
   * and 1.17x for it (history.d cont-stack-ab). A chain called from
   * two threads at once may attach two gauges and keep one: a lost
   * measurement, never a wrong one — `worst` starts cold either way.
   */
  private[okay] final class Gauge:
    var top: Long = 0L
    var mark: Long = 0L
    var granted: Int = 0
    var worst: Long = StackSwitch.coldBytesPerLevel

  /** the root of a run's continuation chain once it has a gauge: the
   * user's `k`, and the run's gauge behind it */
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
 * (`Run`) and the delimiters between them (`Reset`) — "`Frames` of
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
 * THE STACK: segments and the delimiters between them, the same join.
 * A captured `k` is one of these — `Run(frames, Reset(p, ret, …, Done))`
 * for a capture to the nearest delimiter — and so is the continuation
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
   * THE DELIMITER. `$` (Materzok & Biernacki, APLAS 2012) is `ret` on
   * the stack with the prompt beside it, so a body that returns runs
   * `ret` by the ordinary pop — the `($v)` rule needs no case in the
   * loop — and a `shift0` cuts the stack at the first `Reset` naming
   * its prompt, so its `k` carries `ret` — the `($/S0)` rule. `plain`
   * says `ret` is the identity (a `reset`, not a `$`): what a bare cut
   * (`control`, `control0`) needs. `shots` is a `dollarResumed`'s
   * counter, fresh per capture. A program asks for one with the
   * OPERATION `Cont0.Reset0`, never by building this node: an operation
   * passes through a handler loop between the program and the machine,
   * a node on the machine's stack does not.
   *
   * IT CARRIES THE SEGMENT UNDER IT (Dybvig, Peyton Jones & Sabry's
   * layout: each prompt heads the frames that wait for its answer).
   * Installing one is then ONE node over the live segment, and popping
   * it hands that segment straight back to the frames register;
   * measured, the `Run` a separate segment node cost per delimiter was
   * the last half of `delimPushOnly`'s gap. A copy in a captured `k`
   * has an empty segment and nothing below: what waits for its answer
   * is not captured.
   */
  case Reset[F[_, _, +_], A, S, T, Y, S2, W, Z](p: Prompt[Y], ret: A => Freer[Cont0.Row[F], T, T, Y],
                                                 shots: Cont0.Shots | Null,
                                                 frames: Frames[F, Y, S2, T, W],
                                                 below: Stack[F, W, S, S2, Z]) extends Stack[F, A, S, T, Z]

  /**
   * THE USUAL `k`, ONE NODE: a segment and the delimiter it was cut at,
   * nothing below — `Run(frames, Reset(p, ret, shots, End, Done))`, whose
   * indexes those two empties fix (the delimiter's segment is `End`, so
   * its answer is the stack's `Z`; nothing below, so its `S` is the
   * segment's). A capture to the delimiter at the head of the stack
   * (`nearest`: every `shift`, `emit`, a generator's step) builds this
   * and nothing else, where it built the `Run` and the `Reset` copy; a
   * resumption relinks it over the live registers TYPED (`Rev.onto`),
   * where the two-node shape needed `relink`'s claim.
   */
  case Kept[F[_, _, +_], A, S, T, Y, Z](frames: Frames[F, A, S, T, Y], p: Prompt[Z],
                                        ret: Y => Freer[Cont0.Row[F], S, S, Z],
                                        shots: Cont0.Shots | Null) extends Stack[F, A, S, T, Z]

  def apply(a: A): Freer[Cont0.Row[F], S, T, Z] = this match
    case Done() => Return(a)
    case _ => Delay(Frames.Resume(a, this))

object Frames:
  import Stack.{Done, Run, Reset, Kept}

  /** a resumption: the value and the stack it enters, as the thunk of a
   * `Delay` — run by whoever forces it, pushed by the machine */
  final class Resume[F[_, _, +_], A, S, T, Z](val a: A, val k: Stack[F, A, S, T, Z]) extends (() => Freer[Cont0.Row[F], S, T, Z]):
    def apply(): Freer[Cont0.Row[F], S, T, Z] = Frames.enterAt[F, A, S, T, Z](a, k)

  /**
   * THE COUNT IS A FRAME. A `dollarResumed` is told each time the
   * delimiter is ENTERED: at the operation, and at each run of a
   * capture that took it. A cut puts this identity frame on top of the
   * captured stack, holding the capture's fresh counts, and it is
   * popped exactly when the resumed segment is stepped — once per
   * `k(x)`, never on a head form's re-entry, never for a `k` built and
   * dropped.
   */
  final class Enter[F[_, _, +_], X, T](val shots: List[Cont0.Shots]) extends (X => Freer[Cont0.Row[F], T, T, X]):
    def apply(x: X): Freer[Cont0.Row[F], T, T, X] =
      shots.foreach(_.enter())
      Return(x)

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

  /**
   * THE ONE CLAIM OF THE MACHINE, Delim's `rebase` verbatim: a captured
   * stack's nodes were typed at the join index of the delimiter that
   * bounded them and are handed to a clause typed at the leaf's own;
   * the two are the run's one index, and only the construction knows
   * it. Erased, it costs nothing.
   */
  private def rebase[F[_, _, +_], A, S1, T1, S2, T2, Z](st: Stack[F, A, S1, T1, Z]): Stack[F, A, S2, T2, Z] =
    st.asInstanceOf[Stack[F, A, S2, T2, Z]]

  /** the same claim for a segment handed back from under a delimiter */
  /** THE CLAIM `plain` MAKES, in one place: a plain delimiter's `ret` is
   * the identity, so the segment under it answers the prompt's own type —
   * a bare cut's `k` (the frames above the delimiter, WITHOUT it: the
   * control family) answers `Y` where its nodes say the segment's `C`,
   * and at the run's one index, as `rebase` says for the rest. Only a
   * `control` to a plain delimiter reaches it: `cut` refuses a `dollar`
   * by name first. */
  private def plainly[F[_, _, +_], A, S1, T1, C, T2, Y](k: Stack[F, A, S1, T1, C]): Stack[F, A, T2, T2, Y] =
    k.asInstanceOf[Stack[F, A, T2, T2, Y]]

  /** TWO PROMPTS THAT ARE ONE OBJECT ARE ONE TYPE — `Same.byIdentity`'s
   * axiom, taken here without its `Option` and without the lazy given
   * behind `Delim.samePrompt` (3.4% of a capture-heavy lane's samples,
   * read off async-profiler): the caller tests `eq`, this is the claim
   * for the pair it tested, one shared evidence */
  private def identical[A, B](@annotation.unused a: Prompt[A], @annotation.unused b: Prompt[B]): A =:= B =
    <:<.refl[A].asInstanceOf[A =:= B]

  private def rebaseF[F[_, _, +_], A, S, T1, T2, Z](fs: Frames[F, A, S, T1, Z]): Frames[F, A, S, T2, Z] =
    fs.asInstanceOf[Frames[F, A, S, T2, Z]]

  /** a segment on top of a stack; an empty segment is the stack itself */
  /** a `Kept`'s delimiter as the node it stands for — the `Reset` over an
   * empty segment and nothing below — for a `k` that became the live
   * stack (`pushed`, the loop's pop) or that a walk passes (`cut`) */
  private def delimiterOf[F[_, _, +_], A, S, T, Y, Z](kp: Kept[F, A, S, T, Y, Z]): Stack[F, Y, S, S, Z] =
    Reset[F, Y, S, S, Z, S, Z, Z](kp.p, kp.ret, kp.shots, noFrames[F, Z, S], noStack[F, Z, S])

  private[okay] def runOf[F[_, _, +_], A, S, S2, T, Y, Z](fs: Frames[F, A, S2, T, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = fs match
    case _: End[F, A, S2] @unchecked => st
    case _ => Run(fs, st)

  /** the prompts installed on a stack, innermost first — `NoPrompt`'s list */
  @tailrec def installed[F[_, _, +_]](st: Stack[F, ?, ?, ?, ?], acc: List[String] = Nil): List[String] = st match
    case Run(_, below) => installed(below, acc)
    case Reset(p, _, _, _, below) => installed(below, if p eq Cont0.boundary[Any] then acc else p.label :: acc)
    case Kept(_, p, _, _) => (if p eq Cont0.boundary[Any] then acc else p.label :: acc).reverse
    case _ => acc.reverse

  /**
   * The one loop: run `p` to a head form — `Return(x)`, or `Bind(Inject(e), k)`
   * for the first operation no delimiter on the stack answers, `k` the
   * stack itself (which re-enters this loop when applied). Three
   * registers: the program in focus, the frames of the current segment,
   * and the stack below them. `S0`, `R`, `Z` are the run's; every arm is
   * typed by GADT refinement.
   */
  def run[F[_, _, +_], S0, R, Z](p: Freer[Cont0.Row[F], S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] = machine[F, S0, R, Z, Any](p, null, null, noStack[F, Z, S0])

  /** `p` under the delimiter `ret $_p0`, installed as the run's FIRST
   * stack rather than asked for by a `Reset0` operation the loop's first
   * step turns into the same node: a `Cont` run (its root, `ret` the
   * user's `k`) and `Delim.run` (its boundary) start every run with one,
   * and a run per element (a generator over `Cont`) paid an `Inject`, a
   * `Reset0` and a step each time */
  private[okay] def runUnder[F[_, _, +_], S0, Z](p: Freer[Cont0.Row[F], S0, S0, Z], p0: Prompt[Z],
                                                ret: Z => Freer[Cont0.Row[F], S0, S0, Z]): Freer[Cont0.Row[F], S0, S0, Z] =
    machine[F, S0, S0, Z, Any](p, null, null, Reset[F, Z, S0, S0, Z, S0, Z, Z](p0, ret, null, noFrames[F, Z, S0], noStack[F, Z, S0]))

  /** the machine, entered at a program (`run`) or at a resumption forced
   * by an outer interpreter (`Resume.apply`): the second goes to the
   * registers directly — `k`'s nodes and the value at its top — where it
   * built `Bind(Return(a), k)` for the loop's first step to take apart, two
   * nodes and a step per operation of an effect handled outside */
  /** `k` applied to `a` and run now — a forced resumption's entry, with
   * no `Resume` node built for it (a strict `k` of Cont's enters here) */
  private[okay] def enterAt[F[_, _, +_], A, S0, R, Z](a: A, k: Stack[F, A, S0, R, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    machine[F, S0, R, Z, A](null, a, k, noStack[F, Z, S0])

  private def machine[F[_, _, +_], S0, R, Z, A](p: Freer[Cont0.Row[F], S0, R, Z] | Null,
                                                a: A, k: Stack[F, A, S0, R, Z] | Null,
                                                st0: Stack[F, Z, S0, S0, Z]): Freer[Cont0.Row[F], S0, R, Z] =
    type G = Cont0.Row[F]

    final class Next[X, T, S1, Y](val focus: Freer[G, T, R, X], val fs: Frames[F, X, S1, T, Y], val st: Stack[F, Y, S0, S1, Z])

    /** a capture's fresh count for a delimiter it copies (listed for `Enter`
     * by the caller: a pair here was a `Tuple2` per delimiter crossed) */
    def fresh(shots: Cont0.Shots | Null): Cont0.Shots | Null =
      if shots == null then null else Cont0.Shots(shots.resumed)
    def count(shots: Cont0.Shots | Null, counts: List[Cont0.Shots]): List[Cont0.Shots] =
      if shots == null then counts else shots :: counts

    /** the captured stack under its `Enter` frame, when it has counts */
    /** the capture's body in the delimiter's place, over the segment that
     * waited for the delimiter — and, for `shift`/`control` (`under`), under
     * a fresh plain delimiter of the same prompt, installed here as one
     * node: the `⟨e⟩` of `S k.e = S0 k.⟨e⟩` */
    def started[Y, T, S1, W](sh: Cont0.Shift0[F, Y, T, R, ?], body: Freer[G, T, R, Y],
                             frames: Frames[F, Y, S1, T, W], below: Stack[F, W, S0, S1, Z]): Next[?, ?, ?, ?] =
      if sh.under then Next[Y, T, T, Y](body, noFrames[F, Y, T], Reset[F, Y, S0, T, Y, S1, W, Z](sh.p, Cont0.identity[F, T, Y], null, frames, below))
      else Next[Y, T, S1, W](body, frames, below)

    def entered[X, S, T, Y](k: Stack[F, X, S, T, Y], counts: List[Cont0.Shots]): Stack[F, X, S, T, Y] =
      if counts.isEmpty then k else Run(Frame[F, X, T, T, T, X, X](Enter[F, X, T](counts), noFrames[F, X, T]), k)

    /** cut the stack at the `Reset` naming `sh.p`, walking its NODES:
     * `k` is the nodes above it with it (without it when bare), the
     * body takes the delimiter's place with the stack below it */
    @tailrec def cut[X, Y, T, T2, C](sh: Cont0.Shift0[F, Y, T, R, X], all: Stack[F, X, S0, T, Z], st: Stack[F, C, S0, T2, Z], rev: Rev[F, X, T, T2, C], counts: List[Cont0.Shots]): Next[?, ?, ?, ?] = st match
      // no delimiter answers, and no boundary: the capture goes OUT as an
      // operation, for a machine outside this one (Delim.runNested)
      case Done() => null
      case r: Run[F, C, S0, ?, T2, ?, Z] => cut(sh, all, r.below, Rev.SnocRun(rev, r.frames), counts)
      // a `k` pushed onto an empty machine stands as the stack itself
      // (`Rev.onto`): its two nodes, spelled out, are walked as any
      case kp: Kept[F, C, S0, T2, y, Z] @unchecked =>
        cut(sh, all, Run(kp.frames, delimiterOf(kp)), rev, counts)
      case d: Reset[F, C, S0, T2, ?, ?, ?, Z] if d.p eq Cont0.boundary[Any] => throw NoPrompt(sh.at, sh.p.label, installed(all))
      case d: Reset[F, C, S0, T2, y2, s2, w, Z] =>
        if sh.p eq d.p then
          val ey = identical(sh.p, d.p)
          val k: Stack[F, X, T, T, Y] =
            if sh.bare then
              if !Cont0.plain(d.ret) then throw new UnsupportedOperationException(
                s"${sh.at}: a control-capture to ${d.p.label}, which is a `dollar`: its bare continuation answers the body's type, not the prompt's (specs/shift0-dollar.md)")
              plainly(entered(Rev.link(rev, noStack[F, C, T2]), counts))
            else
              // the nodes WITH the delimiter: `k` carries `ret` (the $/S0 rule), a fresh count
              val shots = fresh(d.shots)
              val counted = count(shots, counts)
              rebase(ey.flip.liftCo[[y] =>> Stack[F, X, T2, T, y]](entered(Rev.close(rev, d.p, d.ret, shots), counted)))
          // the body in the delimiter's place, over the segment that waited for it
          started(sh, sh.f(k), rebaseF(ey.flip.liftCo[[y] =>> Frames[F, y, s2, T2, w]](d.frames)), d.below)
        else
          val shots = fresh(d.shots)
          val counted = count(shots, counts)
          cut(sh, all, runOf(d.frames, d.below), Rev.SnocReset(rev, d.p, d.ret, shots), counted)

    /** an operation of `F`, not of `Cont0`: what the head form hands out */
    def foreign(a: Freer[G, ?, ?, ?]): Boolean = a match
      case Inject(e) => !e.isInstanceOf[Cont0[?, ?, ?, ?]]
      case _ => false

    /**
     * THE COMMON CAPTURE, without the walk: the delimiter named is the
     * head of the stack below the current segment, uncounted, not the
     * boundary, and the capture takes it (not bare) — a generator's
     * `emit`, a deep handler's operation. `k` is the segment and a copy
     * of that one node: two objects, where the walk built a `Run` of
     * the segment, a reversed prefix and its relinking. `null` when the
     * shape is anything else, and the walk decides.
     */
    def nearest[X, Y, T, S1, Y1](sh: Cont0.Shift0[F, Y, T, R, X], fs: Frames[F, X, S1, T, Y1], st: Stack[F, Y1, S0, S1, Z]): Next[?, ?, ?, ?] = st match
      case d: Reset[F, Y1, S0, S1, y2, s2, w, Z] if (sh.p eq d.p) && !sh.bare && (d.shots == null) && !(d.p eq Cont0.boundary[Any]) =>
        val ey = identical(sh.p, d.p)
        val seg: Stack[F, X, S1, T, y2] = Kept[F, X, S1, T, Y1, y2](fs, d.p, d.ret, null)
        val k: Stack[F, X, T, T, Y] = rebase(ey.flip.liftCo[[y] =>> Stack[F, X, S1, T, y]](seg))
        started(sh, sh.f(k), rebaseF(ey.flip.liftCo[[y] =>> Frames[F, y, s2, S1, w]](d.frames)), d.below)
      case _ => null

    /** a capture: to the delimiter at the head of the stack without a
     * walk (`nearest`), else by the walk (`cut`); `null` when no delimiter
     * on this machine answers it */
    def capture[X, T, S1, Y](sh: Cont0.Shift0[F, ?, T, R, X], fs: Frames[F, X, S1, T, Y], st: Stack[F, Y, S0, S1, Z]): Next[?, ?, ?, ?] =
      nearest(sh, fs, st) match
        case null =>
          val all = runOf(fs, st)
          cut(sh, all, all, Rev.nil[F, X, T], Nil)
        case n => n

    /** a resumption's registers: `focus` over the stack `k`'s nodes were
     * pushed onto (`Rev.onto`), its head segment unpacked into the frames
     * register — the one place both resuming arms (a `k` as a bind's
     * continuation, a `Resume` as a delay's thunk) go through */
    def pushed[X, T](focus: Freer[G, T, R, X], k: Stack[F, X, S0, T, Z]): Next[?, ?, ?, ?] = k match
      case rn: Run[F, X, S0, s2, T, y, Z] => Next[X, T, s2, y](focus, rn.frames, rn.below)
      // a `k` that became the whole stack: its segment into the frames
      // register, its delimiter the stack (the empty machine's re-entry)
      case kp: Kept[F, X, S0, T, y, Z] @unchecked =>
        Next[X, T, S0, y](focus, kp.frames, delimiterOf(kp))
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
          // the ($v) rule: a delimiter is popped like any frame — and the
          // segment it carries is the frames register again, in the same
          // step; tested FIRST, the common node under an empty segment
          // a plain one too: skipping `ret` when it is the identity
          // (`Cont0.plain`) bought a plain pop ~0.5 ns and cost every `$`
          // ~3 ns on DelimBenchmark (1h, history.d) — one call for all
          case d: Reset[F, Y, S0, S1, y, ?, ?, Z] => loop(d.ret(r.a), d.frames, d.below)
          // the next segment, unpacked into the frames register
          case rn: Run[F, Y, S0, ?, S1, ?, Z] => loop(focus, rn.frames, rn.below)
          // never live (`pushed` unpacks one), spelled out for the match
          case kp: Kept[F, Y, S0, S1, y, Z] @unchecked =>
            loop(focus, kp.frames, delimiterOf(kp))
          case _: Done[F, Y, S0] @unchecked => focus
      case d: Delay[G, T, R, X] => Frames.resume[F, T, R, X](d.thunk) match
        // a resumption: the value at the top of its stack, pushed — never forced
        case null => loop(d.thunk(), fs, st)
        case r: Resume[F, a, T, R, X] =>
          val n = pushed(Return[G, R, a](r.a), Rev.onto(r.k, fs, st))
          loop(n.focus, n.fs, n.st)
      // the operations, in the loop: a `Next` per `Reset0` and a union
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
          case rs: Cont0.Reset0[F, X, a, T, R] @unchecked =>
            if rs.shots != null then rs.shots.enter()
            loop[a, T, T, a](rs.body, noFrames[F, a, T], Reset(rs.p, rs.ret, rs.shots, fs, st))
          case sh: Cont0.Shift0[F, ?, T, R, X] @unchecked => capture(sh, fs, st) match
            case n: Next[x, ?, ?, ?] => loop(n.focus, n.fs, n.st)
            // nobody here answers it: out, as an operation over the stack
            case null => Bind(focus, runOf(fs, st))
          case _ => Bind(focus, runOf(fs, st))

    if k ne null then
      val n = pushed(Return[G, R, A](a), k)
      loop(n.focus, n.fs, n.st)
    else loop(p.nn, noFrames[F, Z, S0], st0)

/**
 * THE TWO OPERATIONS: the delimiter and the capture, both on the join
 * index of the run. `Reset0` is `ret $ body`: the body at `T`, `ret`
 * at `T` too, the delimiter answering `Y`. `Shift0`'s `f` takes the
 * stack up to and including the `Reset` naming `p` — a `Frames[F, X, T,
 * T, Y]`, from the operation's value `X` through the `Reset` to its
 * answer `Y` — and answers a program that stands in the delimiter's
 * place; `bare` leaves the `Reset` out of `k` (the control family). An
 * enum, for the `+X` a `Freer` signature needs (Delim.Op's shape).
 */
enum Cont0[F[_, _, +_], T, R, +X]:
  case Reset0[F[_, _, +_], Y, A, T, R](p: Prompt[Y],
                                       ret: A => Freer[Cont0.Row[F], T, T, Y],
                                       body: Freer[Cont0.Row[F], T, R, A],
                                       shots: Cont0.Shots | Null) extends Cont0[F, T, R, Y]
  case Shift0[F[_, _, +_], Y, T, R, X](p: Prompt[Y],
                                       f: Stack[F, X, T, T, Y] => Freer[Cont0.Row[F], T, R, Y],
                                       bare: Boolean, under: Boolean, at: String) extends Cont0[F, T, R, X]

object Cont0:
  /** the row: `Cont0` beside any indexed signature `F`; the effect tree's
   * signature is `Freer.Lift[Fx]` here */
  type Row[F[_, _, +_]] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

  /** a `dollarResumed`'s count: told each time the machine ENTERS the
   * delimiter — at the operation, and at each run of a capture that
   * took it (a capture gets a fresh count) */
  final class Shots(val resumed: Int => Unit):
    var n: Int = 0
    def enter(): Unit = { n += 1; resumed(n) }

  /** a fresh delimiter tag, labelled with the line that asked for it */
  def prompt[Y](using at: At): Prompt[Y] = new Prompt[Y]("prompt", at.where)

  /**
   * A PLAIN DELIMITER IS ONE WHOSE `ret` IS THIS OBJECT. `reset` is `$`
   * with `ret = pure`, and every door that builds one hands over this
   * value, so "plain" is `ret eq identity` and needs no field: a `Reset`
   * is 32 bytes, not 40 (the boolean pushed it over the line). It is
   * asked only by a bare cut (`control`); the pop calls `ret` either way
   * (see the loop's `Reset` arm for what testing it there cost). `Return(_)` written anywhere
   * else is a `$` like any other — correct, just not recognised as plain.
   * One object at every index, as `noFrames` is: the cast is that sentence.
   */
  private val theIdentity: Any => Freer[Row[Freer.Lift[Pure]], Any, Any, Any] = Return(_)
  def identity[F[_, _, +_], T, A]: A => Freer[Row[F], T, T, A] =
    theIdentity.asInstanceOf[A => Freer[Row[F], T, T, A]]
  /** is this delimiter plain — its `ret` the identity */
  def plain(ret: AnyRef): Boolean = ret eq theIdentity

  /** THE BOUNDARY: a root delimiter nobody can name, installed by
   * `Delim.run`. A cut that walks into it has passed every delimiter of
   * the machine and found none: `NoPrompt`, with the ones it passed. A
   * run without it (`Delim.runNested`) lets such a capture out as an
   * operation, for a machine outside to answer. */
  private val theBoundary = new Prompt[Any]("boundary", "Delim.run")
  /** at any answer type: it is compared by `eq` and never answers anything */
  def boundary[Y]: Prompt[Y] = theBoundary.asInstanceOf[Prompt[Y]]

  /** `ret $ body`: the body under the delimiter — an operation, so it
   * reaches the machine through any handler loop between them */
  def dollar[F[_, _, +_], Y, A, T, R](p: Prompt[Y])(ret: A => Freer[Row[F], T, T, Y])(body: Freer[Row[F], T, R, A]): Freer[Row[F], T, R, Y] =
    Inject[Row[F], T, R, Y](Cont0.Reset0[F, Y, A, T, R](p, ret, body, null))

  /** `$` told each time it is entered (see `Shots`) */
  def dollarResumed[F[_, _, +_], Y, A, T, R](p: Prompt[Y])(ret: A => Freer[Row[F], T, T, Y], resumed: Int => Unit)(body: Freer[Row[F], T, R, A]): Freer[Row[F], T, R, Y] =
    Inject[Row[F], T, R, Y](Cont0.Reset0[F, Y, A, T, R](p, ret, body, Shots(resumed)))

  /** `$` with `ret = pure`: reset, a PLAIN delimiter */
  def reset[F[_, _, +_], T, R, A](p: Prompt[A])(body: Freer[Row[F], T, R, A]): Freer[Row[F], T, R, A] =
    Inject[Row[F], T, R, A](Cont0.Reset0[F, A, A, T, R](p, identity[F, T, A], body, null))

  def shift0[F[_, _, +_], Y, T, R, X](p: Prompt[Y])(f: Stack[F, X, T, T, Y] => Freer[Row[F], T, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, Y, T, R, X](p, f, false, false, at.where))

  def control0[F[_, _, +_], Y, T, R, X](p: Prompt[Y])(f: Stack[F, X, T, T, Y] => Freer[Row[F], T, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, Y, T, R, X](p, f, true, false, at.where))

  /** `shift`: the body under a fresh plain delimiter; `k` still carries
   * `ret`. APLAS 2012's `S k.e = S0 k.⟨e⟩`, with the `⟨⟩` installed by the
   * machine (`under`) where the body starts, not asked for by a `reset`
   * the body is wrapped in — that spelling cost a closure, a `Reset0`, an
   * `Inject` and a loop step per capture */
  def shift[F[_, _, +_], Y, T, R, X](p: Prompt[Y])(f: Stack[F, X, T, T, Y] => Freer[Row[F], T, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, Y, T, R, X](p, f, false, true, at.where))

  /** `control`: `control0` with the body under a fresh plain delimiter, the same way */
  def control[F[_, _, +_], Y, T, R, X](p: Prompt[Y])(f: Stack[F, X, T, T, Y] => Freer[Row[F], T, R, Y])(using at: At): Freer[Row[F], T, R, X] =
    Inject[Row[F], T, R, X](Cont0.Shift0[F, Y, T, R, X](p, f, true, true, at.where))

  /** leave the delimiter with a value: a shift0 that drops `k` — a
   * `Return` in the delimiter's place, so at the diagonal */
  def abort[F[_, _, +_], Y, T, X](p: Prompt[Y])(value: Y)(using At): Freer[Row[F], T, T, X] =
    shift0[F, Y, T, T, X](p)(_ => Return[Row[F], T, Y](value))

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
  case SnocReset[F[_, _, +_], A, T, S2, Y0, Y](prev: Rev[F, A, T, S2, Y0], p: Prompt[Y], ret: Y0 => Freer[Cont0.Row[F], S2, S2, Y], shots: Cont0.Shots | Null) extends Rev[F, A, T, S2, Y]

private object Rev:
  @tailrec def link[F[_, _, +_], A, S, T, S2, Y, Z](rev: Rev[F, A, T, S2, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = rev match
    case Nil() => st
    case SnocRun(prev, frames) => link(prev, Stack.Run(frames, st))
    case SnocReset(prev, p, ret, shots) => link(prev, Stack.Reset(p, ret, shots, Frames.noFrames, st))

  /**
   * A cut's `k`, closed: the reversed prefix linked down to the delimiter
   * the cut stopped at, the bottom as ONE `Kept` (the last segment and that
   * delimiter, nothing below) — what `link(SnocReset(rev, …), Done)` built
   * as a `Run` over an emptied `Reset`, the shape `relink` needed a claim
   * to resume. An empty last segment is `Kept(End, …)`.
   */
  def close[F[_, _, +_], A, T, S2, Y0, Y](rev: Rev[F, A, T, S2, Y0], p: Prompt[Y],
                                         ret: Y0 => Freer[Cont0.Row[F], S2, S2, Y],
                                         shots: Cont0.Shots | Null): Stack[F, A, S2, T, Y] = rev match
    case r: SnocRun[F, A, T, ?, S2, ?, Y0] @unchecked => link(r.prev, Stack.Kept(r.frames, p, ret, shots))
    case _ => link(rev, Stack.Kept(Frames.noFrames[F, Y0, S2], p, ret, shots))

  @tailrec def reverse[F[_, _, +_], A, S2, T0, T, X, Y](ks: Stack[F, X, S2, T, Y], acc: Rev[F, A, T0, T, X]): Rev[F, A, T0, S2, Y] = ks match
    case Stack.Done() => acc
    case Stack.Run(frames, below) => reverse(below, SnocRun(acc, frames))
    case d: Stack.Reset[F, X, S2, T, y, ?, ?, Y] => d.frames match
      case _: Frames.End[F, y, ?] @unchecked => reverse(d.below, SnocReset(acc, d.p, d.ret, d.shots))
      case fr => reverse(d.below, SnocRun(SnocReset(acc, d.p, d.ret, d.shots), fr))
    case kp: Stack.Kept[F, X, S2, T, y, Y] @unchecked => SnocReset(SnocRun(acc, kp.frames), kp.p, kp.ret, kp.shots)

  /** the empty prefix, one object (as `Frames.noFrames`: no field, phantom indexes) */
  private val theNil: Nil[Nothing, Any, Any] = Nil()
  def nil[F[_, _, +_], A, T]: Rev[F, A, T, T, A] = theNil.asInstanceOf[Rev[F, A, T, T, A]]

  /** `ks ++ st`: `k`'s nodes on top of the stack; onto an empty one, `k` itself */
  def splice[F[_, _, +_], A, S, T, S2, Y, Z](ks: Stack[F, A, S2, T, Y], st: Stack[F, Y, S, S2, Z]): Stack[F, A, S, T, Z] = st match
    case _: Stack.Done[F, Y, S] @unchecked => ks.asInstanceOf[Stack[F, A, S, T, Z]] // Y = Z, S2 = S: st is the identity
    case _ => link(reverse(ks, nil[F, A, T]), st)

  /**
   * A RESUMPTION onto the live registers — the segment and the stack
   * under it. The usual `k` is one segment and the delimiter it was cut
   * at, a copy with an empty segment and nothing below: that delimiter
   * now carries the live segment, so the relinked stack is that ONE
   * node (and the head segment the loop unpacks). Anything else is
   * reversed and relinked.
   */
  def onto[F[_, _, +_], A, S, T, S2, Y, S1, W, Z](ks: Stack[F, A, S2, T, Y], fs: Frames[F, Y, S1, S2, W], st: Stack[F, W, S, S1, Z]): Stack[F, A, S, T, Z] = fs match
    // an EMPTY machine first: a `k` re-entering from outside (a handler of
    // another effect resuming the head form it was handed — every
    // operation of `writerTellUnderDelim`) is pushed onto nothing, so it IS
    // the stack; the two tests refine the indexes, no claim needed
    case _: Frames.End[F, Y, S1] @unchecked => st match
      case _: Stack.Done[F, W, S] @unchecked => ks
      case _ => relinkOrSplice(ks, fs, st)
    case _ => relinkOrSplice(ks, fs, st)

  private def relinkOrSplice[F[_, _, +_], A, S, T, S2, Y, S1, W, Z](ks: Stack[F, A, S2, T, Y], fs: Frames[F, Y, S1, S2, W], st: Stack[F, W, S, S1, Z]): Stack[F, A, S, T, Z] = ks match
    // the usual `k`: its delimiter over the live registers, typed — the
    // segment stays shared, the delimiter is the one node built. Every `k`
    // a capture builds ends in a `Kept` (`nearest`, `close`), so this and
    // the counted shape under it are the two a resumption meets; anything
    // else (a head form fed back) is reversed and relinked
    case kp: Stack.Kept[F, A, S2, T, y, Y] @unchecked => Stack.Run(kp.frames, Stack.Reset(kp.p, kp.ret, kp.shots, fs, st))
    // a `dollarResumed` capture: its `Enter` frame over the `Kept`
    case r: Stack.Run[F, A, S2, ?, T, ?, Y] @unchecked => keptUnder(r, fs, st) match
      case null => splice(ks, Frames.runOf(fs, st))
      case k => k
    case _ => splice(ks, Frames.runOf(fs, st))

  private def keptUnder[F[_, _, +_], A, S, T, S2, S3, Y1, Y, S1, W, Z](r: Stack.Run[F, A, S2, S3, T, Y1, Y], fs: Frames[F, Y, S1, S2, W],
                                                                      st: Stack[F, W, S, S1, Z]): Stack[F, A, S, T, Z] | Null = r.below match
    case kp: Stack.Kept[F, Y1, S2, S3, ?, Y] @unchecked => Stack.Run(r.frames, Stack.Run(kp.frames, Stack.Reset(kp.p, kp.ret, kp.shots, fs, st)))
    case _ => null
