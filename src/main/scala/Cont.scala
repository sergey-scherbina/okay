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
inline def shift[A, S, R](inline f: (A => S) => R): Cont[A, S, R] = Cont.shift(f)
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
   * Danvy and Filinski. A body that only calls `k` in tail position is
   * rewritten at compile time to the value it passes (`ContMacro`,
   * specs/cont-stack.md Layer 1 A); any other body is `shiftLeaf`. */
  inline def shift[A, S, R](inline f: (A => S) => R): Rep[A, S, R] = ${ ContMacro.shift('f) }

  /** the leaf every non-tail body becomes, through `Shift.of` */
  inline def shiftLeaf[A, S, R](f: (A => S) => R): Rep[A, S, R] = Free.Inject(Shift.of(f))

  /** a tail-shaped body `k => { stats; k(v) }`, as the value it passes:
   * `v` computed when the runner reaches it, in the runner's own loop.
   * The indexes are the facade's: `S` flows to `R` exactly as `k`'s
   * answer did, which the runner's `Return` case already trusts. */
  def tailShift[A, S, R](v: () => A): Rep[A, S, R] = Free.delay(() => Free.Return(v()))

  /** the same when `v` is a literal or a stable name and nothing runs
   * before it: no thunk at all */
  def tailPure[A, S, R](v: A): Rep[A, S, R] = Free.Return(v)

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
    /** `Shift.at`'s door out, for the runner: the continuation it
     * built for this leaf is the one the facade typed the leaf with */
    private[Cont] def walkWith[X](k: X => Any): Body[?] = body(k.asInstanceOf[A => S])

  /** the leaf an answer-using body becomes */
  def cps[A, S, R](c: Cps[A, S, R]): Rep[A, S, R] = Free.Inject(Shift.of(c))

  /** the runner's loop from a body: what a `Cps` does when applied as
   * the function it means */
  private def walk[R](b: Body[R]): R =
    step[Any, Any, R](noProgram)(noK)(StackSwitch.firstRoom)(Pending.None)(b)

  /** the program and continuation a walk starts with — never looked
   * at: a walked body answers through its pending parts, and a program
   * it continues carries its own */
  private val noProgram: Rep[Any, Any, Any] = Free.Return(())
  private val noK: Any => Any =
    _ => throw IllegalStateException("a walked body answers through its pending parts, never through k")

  /**
   * The explicit stack of pending body parts (Layer 1 B): what is left
   * to do with the answer of a call of `k`, pushed when the call is
   * made, popped and fed when the program's answer arrives. `None` is
   * the empty stack; a run with no CPS body never allocates one.
   */
  private final class Pending[S, R](val rest: S => Body[R], val next: Pending[?, ?]):
    /** `Shift.at`'s claim once more: the answer the runner reached is
     * the `S` the facade typed this part for */
    def deliver(s: Any): Body[R] = rest(pinned[Any, S](s))
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
        case _: Leaf[?, ?, ?] | _: Cps[?, ?, ?] => Bind(c, a => Free.Return(f(a)))
        case _ => Inject(Leaf.Mapped(s, f))
      case _ => Bind(c, a => Free.Return(f(a)))

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
   * The leaf's class cast is `Shift.at`'s claim at the leaf's class: a
   * leaf is built only by `bind`/`mapped`, at the facade's indexes;
   * a CPS body's is `Cps.walkWith`. The `Any` at the walk's answer is
   * `pinned`'s.
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
      case Body.Done(r) => answer(pinned[Any, R](r))
      case Body.Call(kk, e, rest) => kk match
        case re: Reentry[x, b2, s2, ?] => step[b2, s2, R](re.f(e))(re.k)(room)(Pending(rest, pending))(null)
        case _ => step[A, S, R](c)(k)(room)(pending)(rest(kk(e)))
    else c match
      case Return(a) => answer(pinned[S, R](callK(k, a, room)))
      case Inject(s) => s match
        case l: Leaf[?, ?, ?] => answer(l.asInstanceOf[Leaf[A, S, R]].applyAt(k, room))
        case cps: Cps[?, ?, ?] => step[A, S, R](c)(k)(room)(pending)(cps.walkWith(k))
        case _ => answer(s.at[S, R](k))
      // the leaf's inner answer is the Bind's existential — `Any` names
      // "whatever it is". Left to inference it came out `Nothing`, and a
      // lambda whose body is typed `Nothing` carries a checkcast to
      // Nothing$ that throws (ClassCastException: null, four TestFree
      // rotation laws, 2026-09-15). `typed(s)(...)` never hit this only
      // because its two argument lists resolved the variable differently.
      case Bind(Inject(s), f) => s match
        case cps: Cps[?, ?, ?] => step[A, S, R](c)(k)(room)(pending)(cps.walkWith(Reentry(f, k, room - 1)))
        case _ => answer(s.at[Any, R](Reentry(f, k, room - 1)))
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
