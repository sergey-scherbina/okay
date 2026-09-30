package okay

import okay.Freer.{Return, Inject, Bind, Delay}

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
   * The leaf, as the tree stores it: the shift body ITSELF, answer
   * types and all — `(X => S) => R` at the indexes the node carries
   * (freer-base-step-extractor, 2026-09-29). Until then the tree was
   * `Free[Shift, A]` with no answer type on it, so the leaf had to be
   * stored at the one supertype every typed shift conforms to,
   * `(X => Nothing) => Any`, behind `Shift.of` (an upcast) and read
   * back through `Shift.at` (THE cast) — the facade's signatures were
   * the only place S and R lived. Now `Freer`'s `Bind` joins a left
   * side answering `T => R` to a continuation answering `S => T`, which
   * is exactly answer-type modification, so the leaf keeps its own
   * type and the runner below is typed by the GADT end to end. The two
   * casts (`Shift.at`, `pinned`) and the paragraph that justified the
   * spelling went with them.
   */
  private[okay] type Shift = [S, R, X] =>> (X => S) => R

  /**
   * The representation, opaque HERE rather than at top level — and
   * that placement is load-bearing, not style. A top-level `opaque
   * type` is transparent to its whole PACKAGE, so declared there a
   * `Cont` would still be plainly `Freer[Shift, S, R, A]` everywhere in
   * `okay`, and every extension written for a program carrier would
   * apply to it: Generate.scala's for-comprehension picked up
   * `Stream`'s `map`, which takes a function INTO a program, and the
   * absorption below would have been bypassed wholesale. Inside an
   * object the scope is the object, which is what a facade needs.
   */
  opaque type Rep[A, S, R] = Freer[Shift, S, R, A]

  /** a finished value (named where the 200-odd call sites already look for it) */
  def Pure[A, R](a: A): Rep[A, R, R] = Return(a)

  /** a computation as a function of its continuation — the shift of
   * Danvy and Filinski. A body that only calls `k` in tail position is
   * rewritten at compile time to the value it passes (`ContMacro`,
   * specs/cont-stack.md Layer 1 A); any other body is `shiftLeaf`. */
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ ContMacro.shift('f) }

  /** the leaf every non-tail body becomes: the function, as it is */
  inline def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] = Inject(f)

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
    tailAt[A, S, R](Freer.delay[Shift, S, S, A](() => Return[Shift, S, A](v())))

  /** the same when `v` is a literal or a stable name and nothing runs
   * before it: no thunk at all */
  def tailPure[A, S, R](v: A)(using ev: S <:< R): Rep[A, S, R] =
    tailAt[A, S, R](Return[Shift, S, A](v))

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
   * `k(e)` becomes a `Call` naming what is left. The body is then DATA
   * the runner walks in its own loop, with the pending parts on an
   * explicit stack (`Pending`) instead of the JVM's: a call of `k`
   * continues the program in-loop through the `Reentry`'s fields, and
   * the answer, when the program reaches it, is fed to the part on
   * top. No body frame, no `enter`, no room counted, no switch — at a
   * `Call`, its rest and a `Pending` per call of `k`, with `k`
   * multi-shot as before (the rest of the program is a tree, rebuilt
   * per call).
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
    final def apply(k: A => S): R = walk(body(k))

  /** a CPS body found under a `Bind`, at the leaf's own arguments: the
   * leaf is `(X => T) => R` and the continuation the bind built for it
   * is the `X => T` — the class test is `@unchecked` for the reason the
   * `Inject` case gives, and the body's `R` is the step's */
  private def cpsBody[X, T, R](c: Cps[?, ?, ?])(k: X => T): Body[R] =
    c.asInstanceOf[Cps[X, T, R]].body(k)

  /** the leaf an answer-using body becomes */
  def cps[A, S, R](c: Cps[A, S, R]): Rep[A, S, R] = Inject(c)

  /** the runner's loop from a body: what a `Cps` does when applied as
   * the function it means */
  private def walk[R](b: Body[R]): R =
    step[Any, Nothing, R](noProgram[R])(noK)(StackSwitch.firstRoom)(Pending.None)(b)

  /** the program and continuation a walk starts with — never looked
   * at: a walked body answers through its pending parts, and a program
   * it continues carries its own. A `Delay` because it must sit at the
   * walk's OWN answer index on a base invariant in `R`
   * (freer-consumed-index): `Return` is diagonal, and the `Nothing`-indexed
   * value this was until then rode the covariance that is gone */
  private def noProgram[R]: Rep[Any, Nothing, R] =
    Delay(() => throw IllegalStateException("a walked body answers through its pending parts, never through its program"))
  private val noK: Any => Nothing =
    _ => throw IllegalStateException("a walked body answers through its pending parts, never through k")

  /**
   * The explicit stack of pending body parts (Layer 1 B): what is left
   * to do with the answer of a call of `k`, pushed when the call is
   * made, popped and fed when the program's answer arrives. `None` is
   * the empty stack; a run with no CPS body never allocates one.
   */
  private final class Pending[S, R](val rest: S => Body[R], val next: Pending[?, ?]):
    /** the walk's own claim (see `walked`): the answer the runner
     * reached is the `S` this part was pushed for */
    def deliver(s: Any): Body[R] = rest(walked[S](s))
  private object Pending:
    val None: Pending[?, ?] = Pending[Any, Nothing](_ => throw IllegalStateException("empty"), null)

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
   * The type parameters are the tree's own now: an `Absorbed` is a
   * `(B => S) => R`, which is the leaf type at the indexes `bind`'s
   * result carries, so the compiler checks the pair it is built from.
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

    def apply(k: A => S): R = k match
      case r: Reentry[?, ?, ?, ?] => applyAt(k, r.room)
      case _ => applyAt(k, StackSwitch.firstRoom)

    /** the leaf re-enters the runner with the room the runner has left
     * (specs/stack-safety.md stage 1c) */
    def applyAt(k: A => S, room: Int): R = this match
      case Absorbed(s, g) => s(Reentry(g, k, room - 1))
      case Mapped(s, g) => s(mappedK(k, g, room - 1))

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
        // already absorbed one — see `Leaf` for why never twice; a CPS
        // body is walked by the runner, never applied, so never absorbed
        case _: Leaf[?, ?, ?] | _: Cps[?, ?, ?] => Bind(c, f)
        case _ => Inject(Leaf.Absorbed(s, f))
      // Pure receivers build a node too: fusing `pure(a).flatMap(f)` at
      // CONSTRUCTION would run `def forever = pure(()).flatMap(_ =>
      // forever)` at construction and diverge (interpreter-optimization)
      case _ => Bind(c, f)

  /** map, absorbed in its own right — see `Mapped` */
  def mapped[A, B, S, R](c: Rep[A, S, R])(f: A => B): Rep[B, S, R] =
    c match
      case Inject(s) => s match
        case _: Leaf[?, ?, ?] | _: Cps[?, ?, ?] => Bind(c, a => Return(f(a)))
        case _ => Inject(Leaf.Mapped(s, f))
      case _ => Bind(c, a => Return(f(a)))

  /** apply to a continuation, as the function (A => S) => R it means */
  def run[A, S, R](c: Rep[A, S, R])(k: A => S): R = step(c)(k)(StackSwitch.firstRoom)(Pending.None)(null)

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
   * exhaustion (plan stage C, C1): the gauge is attached THEN, at the
   * root of the continuation chain — the outermost `Reentry`'s `k`,
   * the user's own function, wrapped in a `Gauged` — and found by the
   * same walk at every exhaustion after. The first cut wrapped the
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
  private final class Gauged[B, S](val k: B => S, val gauge: Gauge) extends (B => S):
    def apply(b: B): S = k(b)

  /** the gauge behind a continuation: the one at the chain's root, or
   * one attached there now; a fresh, unattached one for a chain that
   * has no `Reentry` at all (a `Mapped` leaf's lambda): a fresh gauge
   * only makes the next grant conservative — `worst` starts cold —
   * never wrong */
  @annotation.tailrec
  private def gaugeOf(k: Any): Gauge = k match
    case r: Reentry[?, ?, ?, ?] => r.k match
      case inner: Reentry[?, ?, ?, ?] => gaugeOf(inner)
      case _ => r.gauge
    case g: Gauged[?, ?] => g.gauge
    case _ => Gauge()

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
  private final class Reentry[X, B, S, T](val f: X => Rep[B, S, T], var k: B => S, val room: Int) extends (X => T):
    def apply(x: X): T = enter(x, room)

    /** this chain root's gauge, attached on the first ask: `k` is the
     * user's function here (see `gaugeOf`), wrapped once */
    def gauge: Gauge = k match
      case g: Gauged[?, ?] => g.gauge
      case _ =>
        val g = Gauge()
        k = Gauged(k, g)
        g

    /** enter from a place with `here` levels of room left: a
     * continuation may be called DEEPER than it was made (the runner
     * hands an answer to an outer continuation from inside an inner
     * segment), so the room is the smaller of the two. At ZERO the
     * stack is asked how much it really has left (Layer 3): a grant
     * continues here, and only a stack with no room switches. */
    def enter(x: X, here: Int): T =
      val r = if here < room then here else room
      if r > 0 then step(f(x))(k)(r)(Pending.None)(null) else exhausted(x)

    /** the rare road, OUT of `enter` so `enter` stays small enough to
     * inline (cont-stack-fastpath round 2, PrintInlining on fib100:
     * `enter` at 106 bytes read "callee is too large" and `callK`
     * through it "callee uses too much stack", and the `Mapped` lambda
     * that the base scalar-replaced then escaped) */
    private def exhausted(x: X): T =
      val more = StackSwitch.more(gaugeOf(k))
      if more > 0 then step(f(x))(k)(more)(Pending.None)(null)
      else StackSwitch.fresh(fresh => step(f(x))(k)(fresh)(Pending.None)(null))

  /**
   * A `Mapped` leaf's continuation: `callK`'s type test taken ONCE, when
   * the leaf is applied, rather than inside the lambda on every call
   * (cont-stack-fastpath round 3). A plain `k` gets the base's own
   * two-capture lambda, `a => k(g(a))`, which the JIT inlines into the
   * shift's body and scalar-replaces; a `Reentry` gets one that enters
   * it directly. Same meaning as `a => callK(k, g(a), room)`: `k` is
   * fixed for the leaf's life.
   */
  private def mappedK[A, B, S](k: B => S, g: A => B, room: Int): A => S = k match
    case r: Reentry[x, ?, ?, t] => a => r.enter(g(a), room)
    case _ => a => k(g(a))

  /** the continuation a `Bind(Inject(s), f)` hands its leaf: CURRIED, so
   * `B` is fixed by `f` before `k` is checked against it — the tree's
   * `+A` makes the bind's continuation domain a subtype of the step's
   * `A`, and `A => S` is a `B => S` by contravariance, which one
   * parameter list would not let inference see */
  private def reenter[X, B, S, T](f: X => Rep[B, S, T])(k: B => S)(room: Int): X => T = Reentry(f, k, room)

  /** call a continuation from inside the runner, with the room HERE */
  private def callK[A, S](k: A => S, a: A, room: Int): S = k match
    case r: Reentry[x, ?, ?, t] => r.enter(a, room)
    case _ => k(a)

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

  /**
   * THE WALK'S ONE CLAIM, and it is about the pending stack, not the
   * tree (freer-base-step-extractor left it as it was): a body being
   * walked answers through the parts pushed for it, and the loop's
   * `c`/`k`/`R` are the STEP's, not the body's, while it walks (`b ne
   * null` — `noProgram`, `noK`). So a `Done(r)` and a delivered answer
   * arrive typed by whoever pushed the part, which the loop's signature
   * does not carry. The tree's two claims of the same shape (`Shift.at`
   * at the leaf, `pinned` at `Return`) are gone: the GADT types them.
   */
  private inline def walked[R](s: Any): R = s.asInstanceOf[R]

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
   *
   * WITH LAYER 1 B'S PENDING STACK (cont-stack-layer1-b): every exit that used
   * to RETURN an answer — the `Return` case's `callK`, an opaque leaf,
   * an opaque body under a `Bind` — now `answer`s it, which is the
   * return when nothing is pending and otherwise feeds the part on
   * top and walks on. The body being walked is the loop's fifth
   * parameter (`null` when none — a wrapper node per step cost an
   * allocation), so the loop stays one `@tailrec` method: `Call`
   * continues the program through the `Reentry`'s fields in-loop, at
   * the room THIS stack has (no frame was spent). A nested runner — `Reentry.enter`, from an opaque body's
   * own call of `k` — starts with nothing pending and returns as
   * before; its answer lands in this loop's `answer`.
   *
   * A leaf is `(A => S) => R` on the tree now; an absorbed `Leaf` and a
   * `Cps` are its subclasses at the same arguments, found by class (see
   * the `Inject` case). The `Any` at the walk's answer is `walked`'s.
   * Until this stage the room reached an absorbed leaf through a
   * `leafAt` helper, for the reason still true of `applyAt`: a leaf
   * re-enters the runner through ITS continuation, not the one it is
   * given, so read off the continuation it would only ever see the
   * program's outermost one, and shifts in a row would never count
   * down (measured: a 20 000 shift program overflowed with the room
   * in `k` alone).
   */
  @annotation.tailrec
  private def step[A, S, R](c: Rep[A, S, R])(k: A => S)(room: Int)(pending: Pending[?, ?])(b: Body[?]): R =
    inline def answer(r: R): R =
      if pending eq Pending.None then r
      else step[A, S, R](c)(k)(room)(pending.next)(pending.deliver(r))
    if b ne null then b match
      case Body.Done(r) => answer(walked[R](r))
      case Body.Call(kk, e, rest) => kk match
        // `walked`'s claim on a PROGRAM: the answer of this nested run
        // goes to the part just pushed, not to this step's `R` — the
        // loop's result type is the step's, and the pending stack's
        // typing is dynamic (see `walked`)
        case re: Reentry[x, b2, s2, ?] => step[b2, s2, R](walked[Rep[b2, s2, R]](re.f(e)))(re.k)(room)(Pending(rest, pending))(null)
        case _ => step[A, S, R](c)(k)(room)(pending)(rest(kk(e)))
    // `@unchecked`: `Diag` is a case of the base a Cont never holds —
    // this companion builds every Cont leaf, as `Inject`, so it can be
    // absorbed — and a dead arm for it here would be bytes in the loop
    // whose inlining the Fib lanes price (freer-diag-leaf)
    else (c: @unchecked) match
      case Return(a) => answer(callK(k, a, room))
      // an absorbed leaf and a CPS body are found by CLASS: the leaf's
      // type on the tree is `(A => S) => R`, the classes extend it at
      // their own arguments, and those are the same three — nothing
      // else builds either (`bind`, `mapped`, `cps`) — which a type
      // test cannot see through a function type, so the arguments are
      // `@unchecked`: the one claim the class boundary keeps
      case Inject(s) => s match
        case l: Leaf[A, S, R] @unchecked => answer(l.applyAt(k, room))
        case cps: Cps[A, S, R] @unchecked => step[A, S, R](c)(k)(room)(pending)(cps.body(k))
        case _ => answer(s(k))
      // the leaf's inner answer is the Bind's middle index, bound by the
      // match: `Reentry(f, k, room - 1)` is exactly the `X => T` the
      // leaf `(X => T) => R` takes, and nothing names `Any` any more
      case Bind(Inject(s), f) => s match
        case cps: Cps[?, ?, ?] => step[A, S, R](c)(k)(room)(pending)(cpsBody(cps)(reenter(f)(k)(room - 1)))
        case _ => answer(s(reenter(f)(k)(room - 1)))
      case Bind(Bind(a, f), g) => step(Bind(a, x => bind(f(x))(g)))(k)(room)(pending)(null)
      case Bind(Return(a), f) => step(f(a))(k)(room)(pending)(null)
      case Delay(t) => step(t())(k)(room)(pending)(null)
      case Bind(Delay(t), g) => step(Bind(t(), g))(k)(room)(pending)(null)

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
