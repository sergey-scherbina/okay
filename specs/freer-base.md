# Freer base — one indexed freer monad under Cont and Free

## Overview

`Cont` (Cont.scala) and `Free` (Free.scala) are the same data type
with one case renamed. Both are `Pure | leaf | Bind | Defer`, both
rebalance left-nested binds by the same tail-recursive rotation, both
force `Defer` in that loop. The leaf differs: `Cont` has
`Shift(f: (A => S) => R)`, a function of the continuation that
carries the answer types; `Free` has `Inject(e: F[A])`, an operation
whose answer is chosen later by the handler. Everything else is
copied — the rotation exists FIVE times today (`Cont./`, `Free.fold`,
`runFree`, `!.resume`, and Async.scala's own loop at lines 210–231),
and every edit to `Defer` went to all five.

This spec extracts the shared part as one enum, `Freer`, indexed by
Atkey's parameterised-monad indexes, and makes `Cont` and `Free`
two instantiations of it. The indexes read two ways from the same
pair of parameters: for `Cont` they are the answer type
(Danvy–Filinski answer-type modification, which `PState` and `Loop`
already use); for `Free` they are a protocol state (typestate), which
nothing uses yet and stage 2 opens. One sentence carries the design:
**Free is Cont whose shift body is chosen by the handler, not by the
program.**

Three measurement lanes on 2026-09-15 (fuse-bench f4da381e,
fuse-depth e12b36fd, fuse-consumers 1c236e0a; rows `fuse0-*` and
`fuse1-*` in src/jmh/history.tsv, paragraphs in
specs/interpreter-optimization.md Results) settled the one thing that
resisted a shared `flatMap`, Cont's closure fusion:

- fusion pays: `fuse=0` is 1.12–1.23x slower on every Fib lane;
- ONE step is the whole win: `fuse=1` equals `fuse=128` on fib10/50/
  100/1000 and on both `Monadic.reflect` lanes, inside ±1–3% bars;
- the 128 budget COSTS on the one shape that reaches it: `statePara`
  (PState, 1 000 left-nested ops) is 12% faster at `fuse=1`.

Why, in one line: `Op(s).flatMap(g).flatMap(h)` without fusion is
`Bind(Bind(Op(s), g), h)` and the runner pays a rotation (a Bind and a
closure) per element; absorbing `g` into the leaf makes the second
bind `Bind(Op(s'), h)`, already the head-normal form, and the rotation
never happens. Deeper absorption only nests closure calls on the
run-time stack. Only a function leaf can absorb a continuation; a data
leaf `F[A]` cannot — which is also why `Free` has no fusion and the
only alternative for it, the type-aligned queue, was measured
unneeded (HandlerBenchmark: stepping within 8% of bulk).

## Interface

```scala
/** (A => S) => R: a computation of A that, given a continuation into
 *  S, answers R. Read as a transition from index R to index S. */
enum Freer[G[_, _, _], A, S, R]:
  case Pure[G[_,_,_], A, R](a: A)                                                            extends Freer[G, A, R, R]
  case Op[G[_,_,_], A, S, R](g: G[A, S, R])                                                   extends Freer[G, A, S, R]
  /** public, as Free's is today: the tree is for tools, and an
   *  interpreter anywhere matches `Bind(Op(e), k)` after `resume` */
  case Bind[G[_,_,_], A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T])          extends Freer[G, B, S, R]
  /** the runner's alone: `resume` forces it before anyone sees the tree */
  case Defer[G[_,_,_], A, B, S, T, R](thunk: () => Freer[G, A, T, R], f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]

  /** ONE rotation, leaf-agnostic: normalizes to Pure | Op | Bind(Op, k).
   *  Composes continuations as Bind directly — the runner must never
   *  fuse (see Decisions), so this needs no flatMap of any kind. */
  @tailrec final def resume: Freer[G, A, S, R] = this match
    case Bind(Bind(a, f), g)  => Bind(a, x => Bind(f(x), g)).resume
    case Bind(Pure(a), f)     => f(a).resume
    case Defer(t, f)          => Bind(t(), f).resume
    case Bind(Defer(t, f), g) => Defer(t, x => Bind(f(x), g)).resume
    case a                    => a
```

`Freer` has NO `flatMap`/`map` members. Each instantiation owns its
own, as extensions in the leaf type's companion — that companion is
in the implicit scope of `Freer[Leaf, A, S, R]`, so `c.flatMap(f)`
resolves by the receiver's leaf with nothing imported, and there is
no member to shadow it:

```scala
/** the Cont leaf: a function of the continuation, answer types in its type */
opaque type Shift[A, S, R] = (A => S) => R
object Shift:
  /** a shift that has absorbed exactly one continuation — the fusion
   *  bit is the class, there is no depth field */
  private final class Absorbed[A, B, S, T, R](s: (A => T) => R, g: A => Cont[B, S, T])
      extends ((B => S) => R):
    def apply(k: B => S): R = s(g(_) / k)

  extension [A, S, R](c: Cont[A, S, R])
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = c match
      case Op(s: Absorbed[?, ?, ?, ?, ?]) => Bind(c, f)          // already absorbed one: a node
      case Op(s)                         => Op(Absorbed(s, f))   // absorb the first
      case _                             => Bind(c, f)
    def map[B](f: A => B): Cont[B, S, R] = flatMap(a => Pure(f(a)))
    @tailrec infix def /(k: A => S): R = c.resume match
      case Pure(a)           => k(a)
      case Op(s)             => s(k)
      case Bind(Op(s), f)    => s(f(_) / k)

type Cont[A, S, R] = Freer[Shift, A, S, R]
object Cont:                       // the factories the 211 `Cont.Pure` sites and `Cont.defer` need
  inline def Pure[A, R](a: A): Cont[A, R, R] = Freer.Pure(a)
  inline def Shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Freer.Op(f)
  def defer[A, B, S, T, R](t: () => Cont[A, T, R])(f: A => Cont[B, S, T]): Cont[B, S, R] = Freer.Defer(t, f)

/** the Free leaf: an operation of a UNARY signature; the index rides on the node */
opaque type Lift[F[+_]] = [A, S, R] =>> F[A]
object Lift:
  extension [F[+_], A, S, R](p: Freer[Lift[F], A, S, R])
    def flatMap[B, S2](f: A => Freer[Lift[F], B, S2, S]): Freer[Lift[F], B, S2, R] = Bind(p, f)
    def map[B](f: A => B): Freer[Lift[F], B, S, R] = Bind(p, a => Pure(f(a)))

/** stage 1: a closed diagonal program, the phantom index pinned */
infix type ![A, F[+_]] = Freer[Lift[F], A, Unit, Unit]
object !:
  export Freer.{Pure, Bind}
  val Effect = Freer.Op                                   // the extractor, `case Bind(Effect(e), k)` unchanged
  def defer[F[+_], A, B](t: () => A ! F)(f: A => B ! F): B ! F = Freer.Defer(t, f)
  inline def effect[F[+_], A](e: F[A]): A ! F = Freer.Op(e)   // the factory, index pinned at Unit
  // resume, next, ?, tailcall, widen, translate, relay, interpret, tracing: as today, over Freer.resume

/** stage 2 (2026-09-23): an INDEXED program — the same Free tree
 *  behind an opaque facade with two phantom indexes, `S` before and
 *  `R` after. Ordinary programs enter DIAGONALLY (`diag`, at any
 *  index, moving nothing); only a smart constructor may claim a move
 *  (`Prog.transition`), and the module that writes one keeps it
 *  private. Never matched, so the existential leak that refuted the
 *  indexed enum (stage 1) cannot reach it. */
opaque type Prog[F[+_], A, S, R] = Free[F, A]
object Prog:
  inline def diag[S, F[+_], A](p: A ! F): Prog[F, A, S, S]          // any program, index unmoved
  inline def pure[F[+_], A, S](a: A): Prog[F, A, S, S]
  inline def effect[F[+_], A, S](e: F[A]): Prog[F, A, S, S]
  inline def transition[S, R, F[+_], A](p: A ! F): Prog[F, A, S, R]  // THE claim; a module's private tool
  extension [F[+_], A, S, R](m: Prog[F, A, S, R])
    inline def flatMap[B, T](f: A => Prog[F, B, R, T]): Prog[F, B, S, T]
    inline def map[B](f: A => B): Prog[F, B, S, R]
  extension [F[+_], A, S](m: Prog[F, A, S, S])
    inline def free: A ! F                                            // unlift — DIAGONAL only

/** the Delim half: the prompt stack in the index, as a lexical given
 *  (the probe's shape, 4af08745, over the real machine) */
object Delim.Stacked:   // spelled `Delim.Stacked` in Delim.scala
  final class Stack[S0 <: Tuple] { type S = S0 }
  final class In[R, S <: Tuple](val p: Prompt[R]) { given stack: Stack[p.type *: S] }
  sealed trait Has[S <: Tuple, P]        // "p is on the stack" — the compile error's home
  type Under[F[+_], A, S <: Tuple] = Prog[Delim + F, A, S, S]
  def delimited[R, F[+_]](body: (s: In[R, EmptyTuple]) => Under[F, R, s.p.type *: EmptyTuple])
                         (using OneMachine[F], At): R ! F              // the root: installs AND runs
  def reset[R, F[+_]](using st: Stack[?])(body: (s: In[R, st.S]) => Under[F, R, s.p.type *: st.S])
                     (using At): Under[F, R, st.S]                     // nested: installs only
  def shift[R, A, F[+_]](p: Prompt[R])(using st: Stack[?], ev: Has[st.S, p.type])
                        (f: (A => Under[F, R, st.S]) => Under[F, R, st.S])(using At): Under[F, A, st.S]
  // shift0 / control / control0 / abort: the same signatures over their Delim twins
```

Unchanged by this spec: `Control[M]` and its `Cont`/`Func` instances,
`/>`, `^`, `Loop`, `answer`, `tailcall`; `Effects[M]` with its three
instances — `Eff` is a function into `Cont` and `Eager` is a union
`A | (A ! F)` over the alias, neither touches the tree; `Answers`,
`TypeableK`, `<|>`, `split`, the row algebra `+`/`%`; every handler
signature in the library (they take `A ! F`, which is still one alias).

## Behavior

Stage 0 — the enum, and `Cont` on it:
- [ ] `Freer` compiles on JVM, JS and Native with exactly the four
      cases above and no `flatMap` member; `resume` is the only
      rotation in the file.
- [ ] `Cont` is the alias; Cont.scala keeps `Shift`, `Absorbed`, `/`,
      the `Cont` factories and `Control[Cont]` — nothing else. Every
      `Cont.Pure`/`Cont.Shift`/`Cont.defer` call site compiles
      unchanged (211 / 2 / n today).
- [ ] `TestCont`'s fusion-budget spill stress is REWRITTEN for one-step
      absorption: a left-nested chain of 1 000 000 binds runs on the
      default stack, a right-nested one too, and a chain of 1 000
      shifts each followed by two binds runs — the second bind of each
      must land in a `Bind` node, asserted by an `Absorbed`-count probe
      or by the B/op of the lane, not by reading a depth.
- [ ] `PState`, `Loop`, `Monadic.reflect/reify`, `Delim` (its handlers
      are `F !> S`, Cont-valued) and the `Eff` instance are green
      without source changes beyond imports.
- [ ] MEASURED, same session, alternating, per-lane minimum of 3:
      fib10/50/100/1000 and `statePara` at or below the `fuse1-*` rows
      (175.9 / 937.0 / 1888.1 / 27017.1 ns, 27.8 µs); `cont24` and
      `stateEffect` unchanged; `okayDirect`/`okayDirectRec` unchanged.
      Prediction: shift-heavy lanes gain up to 17% in B/op (40 vs 48 B
      per shift: `Op` 16 + `Absorbed` 24 against `Shift` 24 + lambda
      24) and time inside bars; `statePara` keeps its 12%.

Stage 1 — `Free` on it:
- [ ] `A ! F` is the alias at `Unit`; Free.scala keeps `Lift`, `fold`,
      `run` (both), and `Free.defer`. `object !` exports the cases and
      binds `Effect` to `Op`; the 89 `(x.resume: @unchecked) match`
      sites and the 20 files outside Cont/Free/Effects that match
      `Bind`/`Defer`/`Effect` compile UNCHANGED — the count of touched
      call sites is a result to record, and more than the imports is a
      finding against the design.
- [ ] `runFree`, `!.resume`, `!.next`, `!.?`, `relay`, `translate`, the
      Delim machine, `Pipe`, `Stm`, `Sim`, `Chunks`, `State.handle`,
      `Writer.fold` run over `Freer.resume` or match the cases; Async's
      own loop (the fifth rotation, the only place outside the runners
      that CONSTRUCTS `Bind` and matches `Defer`) is deleted in favour
      of `resume`. Nothing outside Freer.scala names `Defer` afterwards
      — grep is the check. `Free.fold` and `runFree` may keep their own
      inlined rotation ONLY if the law below says they must, and then
      they are the two places that see `Defer`.
- [ ] LAW (new test, all three platforms): for every encoding and every
      bind-tree shape TestLowering already enumerates, eliminating
      after `resume` equals eliminating with the eliminator's own
      inlined rotation, on the answer AND the effect trace. This is
      what lets an eliminator inline the four lines for speed without
      a second source of truth.
- [ ] MEASURED: FusionBenchmark `fusedSWr` at or below 13.7 µs /
      122 641 B/op; CompareBenchmark `okayFree`/`okayCont`;
      HandlerBenchmark `relayForward`/`handleForward`/`stepBulk`/
      `stepOneByOne`; WidenBenchmark and PerElementStepBenchmark
      unchanged within bars. Any lane over its bar names the eliminator
      that must inline the rotation (see Decisions).
- [ ] The `Monad[Free[F, *]]`, `Effects[Free]` and `Effects[Eff]`
      instances, `fromFree`, `toEff`, `convert`, `reify`, `reflect`
      and TestReflect's round trip are green.
- [ ] `TestFree`-level laziness contract holds unchanged: `def forever
      = pure(()).flatMap(_ => forever)` does not diverge at
      construction (no Pure-fusion in `Lift.flatMap`, ever).

Stage 2 — the index as typestate, Delim first (lane freer-base-stage2, 2026-09-23):
- [x] `Prog[F, A, S, R]` exists as an opaque facade over `Free[F, A]`
      with `diag`/`pure`/`effect` at the diagonal, `flatMap` composing
      indexes end to end, `map` keeping them, `free` unlifting a
      DIAGONAL program only; `A ! F` is untouched (see Decisions: the
      diagonal is a conversion, not a definition).
- [x] Delim carries its prompt stack in the index: `Delim.Stacked.
      reset` installs `s.p.type` on the stack for its body, `shift(p)`
      requires `Has[stack, p.type]`, and the three `NoPrompt` shapes
      the probe named — a shift with NO reset, a shift to a FOREIGN
      prompt of the same answer type, and a prompt that ESCAPES its
      reset into a `var` and is shifted to afterwards — are
      `compileErrors` in TestProg, with the message naming the stack.
- [x] The five positive shapes of the probe run through the REAL
      machine and answer the shift/reset laws' values: bare, a
      for-comprehension, an ordinary effect in head position, nesting
      with a shift to the OUTER prompt, a reset as a step of a larger
      program. TestDelim is untouched (the facade is additive).
- [x] The spec's own first caveat is a test: a `Throws` abort inside a
      block promising a transition drops the continuation and the
      transition does NOT happen — asserted, so nobody reads the type
      as a run-time guarantee.
- [x] One module protocol typed: okay-sql's transaction as a `Prog`
      over any `Sql` (`Tx.begin: Idle -> Open`, `commit`/`rollback`:
      `Open -> Idle`, `query`/`update`/`batch` at any index, `Tx.run`
      accepting only `Idle -> Idle`) — the nested `begin` that
      `PgSql.begin` refuses with an `IllegalStateException`, a `commit`
      with no `begin`, and a program that ends inside a transaction
      are `compileErrors`; a well-bracketed program runs against a
      recording fake `Sql` in the order the type promised.
- [x] Zero bytes and zero time: the facade is an opaque alias and
      every constructor is `inline`, so a `Prog` program IS the `Free`
      tree it wraps — asserted structurally (`free` is identity; the
      same nodes, `eq`) rather than benchmarked, since no node changes.

## Out of scope

- **A generic `flatMap` over `G`** — no code is polymorphic in the
  leaf; `Effects[M]` abstracts over the encoding, not the leaf.
- **`Delim.Segs` as a `Freer`** — the machine's continuation stack is
  the Bind spine turned inside out (a zipper: `K`/`Mark`/`Done`); it
  could be a third instantiation, but the machine reifies segments back
  into programs anyway, and nothing measured asks for it.
- **Three-ary effect signatures** (`Tx[A, S, R]` as a GADT the compiler
  checks) — that needs a three-ary row algebra beside the unary one.
  Stage 2's transitions are sealed by smart constructors, the Delim
  discipline; the GADT tier is a later spec if the soft one leaks.
- **`Eff` and `Eager`** — untouched by construction (see Interface).
- **Variance on the indexes** — refused, see Decisions.
- **Lowering `Cont.Fuse`** as its own lane — BACKLOG `cont-fuse-one-
  step` is absorbed by stage 0: with `Absorbed` there is no budget.

## Decisions

- **Stage 2 is an ADDITIVE facade, and `A ! F` is not redefined**
  (2026-09-23). The interface once read "`A ! F` is `Prog[F, A, Unit,
  Unit]`"; with `Prog` an opaque alias that cannot be — one type
  cannot be both the alias and its own facade — and it need not be:
  `Prog.diag` is an inline identity, so any program enters at any
  index for nothing, and `free` leaves at the diagonal. The
  PState/Delim policy (operator, 2026-09-01: additive where apt,
  primary only where necessary) decides the rest: `Delim.reset`,
  `shift` and their callers in okay-ui/okay-agent/okay-llm keep their
  spelling; `Delim.Stacked` is the typed door beside them.
- **The stack is a lexical GIVEN with a type member, exactly the
  probe's shape** (4af08745) — not an inferred parameter (a
  for-comprehension head has no expected type), not a curried
  dependent context function (refused by the compiler), not a stack
  carried through a non-curried one (crashes dotty). The escape case
  falls out of the same design with no region system: after a
  `reset` returns, the stack in force is the OUTER given, which has
  no `p.type` in it, so a shift to the leaked prompt has no `Has`.
  specs/delim-safety.md's region tag (`[S] => Prompted[R, S] ?=>`)
  was the other road; it needs `S` in the program's type for the
  tag to bind, and therefore on every `Delim` signature and the four
  inline doors — the given stack costs one `import s.given` per
  reset instead.
- **`transition` is public and named as the claim it is.** A module
  typing its protocol must make its own transitions, so the tool
  cannot be `private[okay]`; the discipline is the module's — it
  calls `transition` inside its private smart constructors and
  exposes only those, which is how `Tx` in okay-sql is written. A
  `transition` at a call site is the `asInstanceOf` of this design and
  reviewed as one.
- **`free` unlifts the diagonal only.** A `Prog[F, A, S, R]` with `S
  != R` is a program that promises a move; letting it out as a plain
  `A ! F` would run the move without the bracket that closes it
  (`begin` without `commit`). A module's runner takes the closed shape
  (`Tx.run: Idle -> Idle`) and unlifts inside.
- **Enum with a closed `Op(g: G[A, S, R])`, not a sealed trait with
  an open leaf.** Chosen for exhaustiveness: every match in the
  library is checked. The wrapper's cost was the objection — `Op(g)`
  is one object more than a leaf that IS the node — and it is answered
  per instantiation: for `Free`, `Op(e)` is byte for byte today's
  `Inject(e)`; for `Cont`, `Op(f)` holds the raw function, and the
  fusion state is the function's CLASS (`Absorbed`), so a shift is 40 B
  against today's 48. Rejected: `type Shift = ((A => S) => R, Int)`
  (the operator's first proposal) — a `Tuple2` beside `Op` is 64 B per
  shift and a boxed `Integer` at the budget; and a depth field on the
  generic `Op` — 8 B on every Free operation for a number that is 0
  or 1.
- **One-step absorption, and the runner never fuses.** fuse-depth and
  fuse-consumers: one step is the whole win, the budget costs on the
  shape it was built for. `resume` composes continuations as `Bind`
  because a rotation that fused would re-nest closure calls, which is
  exactly what the 128 budget was found doing. Predicted, to be
  refuted by stage 0's numbers: rotation losing the chance to absorb
  inside `f(_).flatMap(g)` is worth nothing measurable — `Bind(Op(s'),
  g)` and `Op(Absorbed(s', g))` are the same bytes and both head-
  normal.
- **`flatMap` lives in the leaf's companion, not in a `Sig[G]`
  typeclass.** Same effect, no plumbing: the implicit scope of
  `Freer[Shift, …]` includes `Shift`'s companion, and there is no
  member to shadow the extension. The typeclass is the fallback if
  extension resolution through an opaque leaf misbehaves — the one
  known trap is the repository's own: same-name extensions in
  DIFFERENT files of one package are not overloads, so `Shift.flatMap`
  and `Lift.flatMap` MUST sit in companions, never at package level.
- **Eager right-nesting at construction — REFUTED by construction, do
  not retry.** `Bind(x, g).flatMap(h) = Bind(x, a => g(a).flatMap(h))`
  would spare the runner every rotation, for `Free` too. It nests the
  EARLIER continuation inside the later one, so invoking the composed
  closure calls `g`, which calls its own inner `g₀`, n deep on a
  foldLeft chain — the eff-stack-safety overflow in a new coat. Lazy
  rotation composes the other way round (`f(_).flatMap(g)`: `f` runs,
  returns a node, the runner calls the next) and stays flat. A thunk
  would fix it at +32 B per bind, which is the price already refused
  for Eff's fast path.
- **No Pure-fusion in `Lift.flatMap`.** Unchanged from
  interpreter-optimization.md: `pure(()).flatMap(_ => forever)` must
  not diverge at construction; cats-effect and ZIO give the same
  contract. `Shift.flatMap` on a `Pure` receiver builds `Bind` for the
  same reason.
- **`Free` at a pinned `Unit` index in stage 1; polymorphic factories
  only in stage 2.** A diagonal program at a fixed index is not
  state-agnostic (the enum is invariant), so "indexes everywhere" means
  two phantom type parameters on every handler — ~539 mentions of the
  program type in the 16k-line core, 17 files in the handler layer.
  Stage 1 therefore pins `Unit` at every Free factory and changes no
  signature — REFUTED 2026-09-15, see Results: pinning the factories
  does not pin a pattern match, because `Bind` carries the left side's
  index and a match makes it existential; stage 2 adds `Prog` and the diagonal `effect[F, A, R]`
  beside it, one effect at a time. Type-inference risk is confined to
  stage 2 and has a precedent: `PState.get[S, R]`/`set[S, S2, R]` are
  exactly this shape and infer through for-comprehensions with no
  annotations.
- **Variance is out.** `(A => S) => R` would be `[+A, -S, +R]`, and a
  state-agnostic program could then upcast into any index. But `Cont`'s
  runner types `k(a): R` from the GADT equality `S = R` that matching
  `Pure` provides; with variance that equality is gone and the line
  becomes a cast. No casts without necessity (operator, 2026-09-02).
  SUPERSEDED IN HALF (freer-base-step-extractor, 2026-09-29): the base
  is `Freer[G, S, +R, +A]`. `+A` the effect tree always had; `+R` is
  what lets a tail-shaped shift body — `k => k(v)`, typed `(A => S) =>
  R` with `S <: R` at its site — become the `Return(v): Cont[A, S, S]`
  the macro emits, and it costs the runner nothing: matching `Return`
  now says `S <: R`, and `k(a): S` is an `R` by subtyping, no cast.
  `S` stays invariant, for this decision's reason: contravariance
  would let a continuation of the wrong answer type into a bind by
  upcast. The opaque facade `Rep[A, S, R]` stays invariant in all
  three (backlog: cont-variance).
  SUPERSEDED AGAIN, THE OTHER WAY (freer-consumed-index, 2026-09-30,
  the operator's decision: "делаем S и R инвариантными"): the base is
  `Freer[G[_, _, +_], S, R, +A]`. `+R` had exactly one reader,
  `tailShift`/`tailPure`'s `liftCo`, and it cost the other reading of
  the indexes — a state the handler CONSUMES — every arm that reads the
  state (the probe below, "McBride's reading is refused by the variance
  ALONE"). Invariance serves both readings; the tail-shift macro pays
  one cast in `Cont.tailAt`, justified by the `S <:< R` it already
  summons at the site. `cont-variance` keeps `+A` on the facade as its
  question and loses `+R`.
- **`resume` is the single rotation; an eliminator may inline the
  four lines only under the law.** Free's `runFree` was measured
  within 8% of stepping through `resume` (HandlerBenchmark
  stepOneByOne vs stepBulk); 8% is over the bars on some lanes. The
  law in stage 1 makes an inlined copy a verified optimisation rather
  than a second definition — the JIT will likely inline the small
  final `resume` anyway, and the numbers decide.
- **Index as typestate is a protocol of the TEXT.** An aborting
  handler (`Throws`) drops the continuation and a promised transition
  never runs; a multi-shot handler (`Choice`) runs it twice. Haskell's
  indexed monads do not carry this caveat because they have no
  handlers that capture the continuation; this library does. So the
  index complements the finalizer discipline of `Resource.run`, never
  replaces it — and stage 2 asserts it in a test.
- **Every case public; only `Shift.Absorbed`/`Shift.Mapped` private.**
  Settled with the operator on 2026-09-15 across three questions: must
  `Bind` and `Defer` be public, does a public `Bind` cost anything,
  and — the one that finished it — what does hiding `Defer` buy when
  `Freer.defer` is a public factory that builds exactly one? Nothing:
  hiding a case whose constructor is public does not stop anyone
  MAKING the node, only MATCHING it, and matching is the half a
  stepper, a rewriter or an outside interpreter needs. So the rule
  came out simpler than it went in — privacy guards invariants, and
  the only invariant a case could guard here is "a leaf absorbs at
  most once", which lives in the leaf's own classes inside `Shift`'s
  companion, not in the tree. `Freer.defer` stays the constructor
  because the thunk shape reads better at a call site than
  `Defer(() => …, f)`, not because it is the only way in.
  `Bind` guards nothing either: a hand-built `Bind(Bind(a, f), g)` is a
  left-nested tree the rotation normalizes by the associativity law,
  and a hand-built `Bind(Op(s), f)` is the unabsorbed form — one
  rotation slower, correct. Public is also what the library says of
  itself ("the tree is for tools": step it, inspect it, relay it —
  Kiselyov's freer exposes `Impure(op, k)` for the same reason), what
  `Free` already is in v0.1.1, and what an interpreter outside package
  `okay` needs, which `okay-cats`/`okay-kyo` would be if they were
  third-party. `Cont`'s old reason for a private `Bind` — "the
  representation stays the runner's" — is answered by `resume` being
  the public normal form. Recorded and refused: `private[okay]`
  (hides the node from library users, keeps the 89 in-repo match sites
  in 15 packages compiling — but buys only the freedom to add a case,
  which breaks the `@unchecked` matchers in-repo just the same, and
  needs a `val` alias in `object !` because an export would go public);
  and a fully private `Bind` behind a public trait `Next` matched by
  type-test patterns with bound type variables (Delim's `case n:
  Next[F, a, R]` idiom, allocation-free, compiler-checked, but the 89
  sites become `n.op`/`n.k`). A case-class view returned by `resume`
  was refused earlier for one allocation per step (Effects.scala's
  `resume` comment). The normal form after `resume` — `Pure | Op |
  Bind(Op, k)` — is documented ONCE, on `resume`.
- **Names.** `Freer` after Kiselyov–Ishii; `Op` for the leaf (not
  `Inject`, which named the effect side only); `Lift` for the unary
  signature's leaf; `Shift.Absorbed` for the fused function, nested
  because `okay.Fused` (handler-fusion loops) and `okay.Fuse` (optics)
  are taken; the `!` alias and `Effect` extractor keep their names so
  the 89 match sites do not move. AS LANDED (2026-09-29): the leaf
  kept the name `Inject` after all — `Freer.Inject` holds an `F[A]` or
  a shift body alike, and `object Free` keeps `Return`/`Inject`/`Bind`/
  `Delay` at the old arities as constructors and patterns, so no match
  site and no `direct`-macro symbol lookup moved; `Lift` is as named;
  the fused function stayed `Cont.Leaf.Absorbed`, where it was.

## Results

### Stage 0 — implemented, green, and at parity or better

**The state to judge it by** (branch tip, four alternating rounds on a
quiet box, every core JMH lane, `-prof gc`, history.tsv `once-*`):

| lane | time | B/op vs master |
|---|---|---|
| `statePara` | **0.860** | −40 132 |
| `fib10` | **0.884** | −96 |
| `handleForward` | 0.974 | −158 400 |
| `stepBulk` | 0.974 | −80 016 |
| `stepOneByOne` | 0.984 | −80 016 |
| `relayPrebuilt` | 0.986 | identical |
| `effCont24` | 0.989 | identical |
| `effInline24`, `func24`, `fib1000`, `relayForward`, `stateEffect` | 0.99–1.00 | identical or lower |
| `buildOnly`, `fib100`, `cont24` | 1.00 | identical or lower |
| `effFunc24` | 1.015 | identical |
| `fib50` | 1.024 | −416 |

Nothing is more than 2.4% slower, eight lanes are faster, and
allocation is at or below master on every lane. Three things got it
there, in this order, and each is written up below: `map` reaching the
absorbing path, `relay`'s loop getting back under the JIT's inlining
threshold (landed separately on master, 25102517), and `Once` becoming
an enum.

### Stage 0 — how it read before those three

The code exists on `feature/freer-base-stage0`: `Freer.scala` (the enum
and `resume`), `Cont.scala` rewritten as the alias plus the `Shift`
leaf, `TestFreer.scala` (the law), `TestCont` rewritten for absorption.
**The full matrix is GREEN on the branch**: `scripts/gate.sh` reports
4 419 test results across JVM, JS and Native, 182 module compiles, no
failures and no warnings. Two call sites outside the base changed,
both named below. It is NOT merged, because it does not pass the
PERFORMANCE gate this spec set for it: three Fib lanes and
`relayForward` are 3.6–8.2% slower.

**The final A/B**, master against stage 0, FOUR alternating rounds in
one session on a quiet box, per-lane minimum, `-prof gc`, every core
JMH lane (history.tsv `freer0-*`):

| lane | time | B/op vs master |
|---|---|---|
| `statePara` | **0.851** | −40 096 |
| `fib10` | **0.903** | −96 |
| `stepOneByOne` | 0.954 | −80 016 |
| `effCont24` | 0.979 | identical |
| `handleForward` | 0.985 | −158 400 |
| `stateEffect`, `effInline24`, `func24` | 0.99 | identical |
| `effFunc24`, `cont24` | 1.00–1.01 | identical |
| `stepBulk` | 1.022 | −80 016 |
| `fib100` | 1.029 | −816 |
| `fib1000` | 1.040 | −8 015 |
| `fib50` | 1.053 | −416 |
| `relayForward` | **1.103** | **identical** |

Allocation is at or below master on every lane. Eight lanes are
faster, five are slower, and the slower ones are led by
`relayForward`, whose bytes match master to the digit.

### The second round of speed work, and what it closed

The operator asked for the residual to be chased. Three more
candidates were named and each was measured; all three are REFUTED,
and the exercise is recorded because the refutations are the durable
part.

- **"The `Op` wrapper costs a pointer chase per leaf touch."** NO — it
  is the opposite. A per-construct benchmark (`LeafBenchmark`, one
  lane per node kind) reads `leafOnly` **0.972**, `leafAbsorbed`
  0.970, `leafSpilled` 0.971, each allocating 8–16 B LESS per leaf.
  The leaf is the part that got faster.
- **"One-step absorption is too shallow; the old budget was buying
  something."** NO, and the sweep is worth keeping: depth 1 / 4 / 16 /
  128 against master, three rounds. The Fib lanes do not care (1.047
  at depth 1, 1.043 at depth 128, allocation identical at every
  depth); `statePara` cares enormously and in ONE direction — 0.861 at
  depth 1 against 1.188 / 1.146 / 1.170 at 4 / 16 / 128. Depth 1 is
  settled by measurement, the constant is a literal, and the switch
  that swept it is gone.
- **"The runner should delegate to `Freer.resume` after all."**
  Indistinguishable. Asked again with the `map` defect fixed, four
  rounds: `relayForward` 1.093 against 1.094, and the Fib lanes
  disagree in DIRECTION (fib10 0.911 vs 1.246, fib100 1.039 vs 0.891)
  on a box at load 4–5 — a JIT inlining-shape lottery, not an effect.
  The interleaved loop is kept because it is the shape the old runner
  had and because it wins `statePara` (0.845 vs 0.900).

**`statePara` earned a second job out of this**: a thousand left-nested
operations, and the only lane in the suite with a sharp, monotone
response to absorption depth. Anything that accidentally deepens
absorption shows up there immediately. Price future leaf changes
against it.

**What is left is not explained.** The residual is invariant to
absorption depth, to runner shape and to allocation, and
`relayForward` makes that unmistakable: 100 of its 10 000 operations
touch `Cont` at all (the other 9 900 forward through `Free`, which
stage 0 does not change), its handler returns a `Pure`, its bytes
match master to the digit — and it is 10% slower. No structural
hypothesis available on this machine survives those four facts
together. The next instrument is a disassembling profiler, and
`-prof perfasm` needs Linux `perf`; on macOS that means `dtraceasm`
under root, which is the operator's call and not a thing to take.

### `Once` as an enum — the operator's proposal, and what closed the gap

After four structural theories had been refuted, the residual on the
Fib lanes was still 3–5% and unexplained. The operator proposed making
`Once` an enum with cases `Absorbed` and `Mapped`, keeping `Shift`
itself the raw opaque function. Measured on the branch, three rounds:
**fib10 0.968, fib100 0.968, fib1000 0.955** — and that is the whole
residual.

The mechanism, and it is worth stating precisely because the obvious
reading is wrong. `apply` written ONCE in the enum body instead of
overridden in each of two classes gives both cases the same vtable
entry, so the runner's `s(k)` has a single call TARGET and the JIT
inlines it. It is not a cure for megamorphism: in the same session,
splitting that call site so it tests `Once` first — a bimorphic site
for the absorbed forms, a megamorphic one for user lambdas — did
NOTHING, 0.996 to 1.017 across every lane. The number of receiver
types was never the problem; the number of call targets was.

Allocation is unchanged, by construction: an enum case is a case class
with the same two fields, and `Shift` stays the raw function so an
unabsorbed leaf is still `Op` plus the user's lambda. The one measured
price is synthetic: `leafMixed`, which alternates the two cases at one
site, allocates +14 B/op (225 760 against 211 792) where a single body
appears to lose escape analysis that two bodies kept. No real lane
shows it — `statePara` 1.002, every Fib lane faster.

### What was refuted on the way, with numbers

Four experiments, each its own A/B in its own session; none is worth
retrying blind.

1. **"The absorption rule costs something."** No. A three-arm run —
   master at a budget of 128, master at a budget of 1, stage 0 —
   measured master@1 allocating **byte for byte** what master@128
   allocates on all four Fib lanes (2 080 / 11 616 / 20 752 / 298 970,
   delta zero) and within 0.4% on time. The whole regression was
   structural, and this is what made that certain.
2. **"The cost is splitting rotation from elimination."** Refuted:
   copying `resume`'s four lines into the runner made it WORSE —
   fib10 1.08→1.18, fib50 1.15→1.38, fib100 1.09→1.18 against the same
   baseline in the same session.
3. **"The rotation must compose through the leaf's own `flatMap`, so
   the rotated continuation can be absorbed."** True in principle
   (`Free`'s `flatMap` IS `Bind`, so `resume`'s raw composition is
   already optimal for it, while `Cont`'s can absorb) — and worth
   nothing measurable here: B/op did not move by a single byte on any
   Fib lane. The generator's programs are right-nested, so that
   rotation case almost never fires. The interleaved loop was kept
   anyway, since it is the shape the old runner had.
4. **The actual defect, found by decomposing allocation per construct**
   (`getCurrentThreadAllocatedBytes`, the technique
   exact-bytes-beat-jmh-alloc-norm records; the JMH bars were wider
   than the effect): `s.map(f)` cost **96 B** against master's 48,
   while calling the absorbing path directly cost **40**. So `map` was
   not reaching it. Cause: `Cont` has no members any more, and a
   top-level `given` is in the package's LEXICAL scope, which beats
   `object Shift` in the receiver's implicit scope — so `c.map(f)`
   resolved to `ParaMonad.map`, whose default is `flatMap(x =>
   pure(f(x)))`, a `Pure` per element. `flatMap` never had the problem
   because both roads lead to `Shift.bind`. `ParaMonad.map` was
   `inline`, hence final, hence unoverridable: the fix is one word
   removed there plus an override in each `Control` instance, and it
   moved fib10 from 1.07 to 0.908 and `shift+map+map` from 192 B to
   136.

### Findings that outlived their experiment

- **No `apply` extension on `Cont` is possible inside package `okay`.**
  `c(k)` was a member of the old enum, where members win; as an
  extension it loses to Generate.scala's seed-side `apply` (`a(f)` for
  a `Loop` body) in lexical scope. Two call sites in `relay`
  (Effects.scala) became `g(e) / k`, which was always the meaning.
- **The eager-shift shape got BETTER.** A chain whose shift bodies
  re-enter their continuation immediately overflows the stack in both
  trees — it is direct style's own cost — but master dies at 1 000
  elements where stage 0 survives them, because the old budget nested
  128 fused closure calls where one-step absorption nests one.
- **`Cont`'s cases are matched nowhere outside Cont.scala.** Every
  `case Pure(a)` in the repository (80-odd sites, 15 packages) is
  `Free`'s. The rewrite touched two call sites in total.

### The decision this leaves

Landing is the operator's call, and the trade is: one enum and one
rotation concept instead of five copies, `statePara` 14% faster,
allocation down everywhere — against 3.6–8.2% on four lanes whose
cause is dispatch shape rather than anything this spec can name. The
next experiment, if it is wanted, is `-prof perfasm` on `relayForward`
(identical bytes, 8.2%, the cleanest signal) before any further
redesign.

### Stage 1 — REFUTED AS SPECIFIED (2026-09-15, branch `feature/freer-base-stage1`)

`Free` cannot be `Freer[Lift[F], A, Unit, Unit]` while the library's
match sites stay as they are. The obstacle is exact, and the compiler
said it rather than an argument:

`Bind[G, A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T])`
carries the LEFT side's answer index `T`. Matching a `Free[F, A]`
gives back a continuation at `Freer[Lift[F], A, Unit, T]` for an
EXISTENTIAL `T`, while all 89 `(x.resume: @unchecked) match` sites
want `A ! F`, which is `T = Unit`. They are the same value at run time
— `Lift` ignores both indexes and every factory pins them — but no
type says so.

Three ways out were tried or costed:

1. **An existential outer index**, `type Free[F, A] = Freer[Lift[F],
   A, Unit, ?]`. Fixes elimination, breaks CONSTRUCTION symmetrically:
   `Bind(a, f)` can no longer unify the inner index with `f`'s result.
2. **A pinning extractor** in `object !` — `def unapply[F, X, A](p:
   Free[F, A]): Option[(Free[F, X], X => Free[F, A])]` with the claim
   made once. REFUTED BY THE COMPILER: `X` is unconstrained by the
   scrutinee, and Scala 3 infers it as `Nothing` rather than
   skolemizing, so `case Bind(Effect(e), k)` yields `k: Nothing =>
   Free[F, A]` and the link between the operation's answer type and
   the continuation's argument is gone. This is the load-bearing
   negative result: the trick that makes GADT extractors work
   elsewhere does not apply to a type parameter that appears only in
   the RESULT of the `unapply`.
3. **A uniform-index bind case in the base** — `case Seq[G, A, B,
   R](a: Freer[G, A, R, R], f: A => Freer[G, B, R, R]) extends
   Freer[G, B, R, R]`. This WOULD work: matching a `Free[F, A]` gives
   `f: x => Free[F, A]` with the link intact, because there is no
   inner index to leak. NOT TAKEN without a decision, because of what
   it costs: `Cont` needs the non-uniform `Bind` (`PState` changes the
   answer type — that is the point of the paramonad), so the base
   would carry BOTH, and `resume` would rotate both. A tree is built
   entirely by one instantiation, so the two never mix and the cases
   cannot combine — but "one rotation instead of five" becomes "one
   method holding two rotations", which is most of what stage 1 was
   for.

**What stage 0 still delivers, and it is not nothing:** `Cont` on the
shared base, at parity or better, with the fusion budget gone. What
stage 1 was to add — `Free` on it, so the rotation exists once rather
than four times — needs either option 3 above or a different base
shape, and that is a decision, not a task.

The branch holds the attempt, WIP and not mergeable, so the next
person does not re-derive the leak.

### The turn after the refutation: indexes on facades, `Freer` deleted (landed)

The refutation said where types may NOT live: on the nodes, because a
match makes a node's index existential. The operator drew the
conclusion the other way round — keep `Free` as the base, delete
`Freer`, make `Cont` a facade — and that is what landed
(`feature/cont-on-free`, rows `cof-*`).

```scala
enum Free[F[+_], A]              // the base, unchanged: Pure | Inject | Bind | Defer
type Cont[A, S, R] = Cont.Rep[A, S, R]
object Cont:
  opaque type Rep[A, S, R] = Free[Shift, A]         // S, R phantom to the tree
private type Shift[+X] = (X => Nothing) => Any      // every (X => S) => R conforms: an upcast
```

Danvy–Filinski's answer-type modification lives ONLY in the
companion's signatures. The tree is the same `Pure | Inject | Bind |
Defer` every effect program is made of, and `Free` needs no alias, no
`Lift`, no phantom `Unit`: the 89 match sites are untouched because
nothing about `Free` changed. **Measured against master: every core
lane within ±1%, allocation identical to the byte on all of them** —
the nodes are the same objects, the cast is erased.

Two `asInstanceOf` lines, one invariant ("the facade typed it when
it built it, and nothing else can build one"): `typed`, applying a
leaf to a continuation, and `pinned`, the `Pure` branch of the runner
— the GADT equality `S = R` that `Pure[A, R] extends …[A, R, R]` used
to carry does not exist on an unindexed tree. That is the price of
ATM on a shared homogeneous tree, and it is the operator's rule
satisfied to the letter: one function each, one comment, sound by a
sealed-module claim.

The principle this settles, and it carries into stage 2: **the tree
is syntax; an index is a claim about syntax; claims live on facades;
facades are never pattern-matched.** Typestate for an effect program
is one more facade over `Free[F, A]`, and the leak that killed stage 1
cannot happen to it.

Three spellings the operator proposed were each tried by compiling,
because the session's rule is that an argument is not evidence:

- **`Free[[X] =>> (X => S) => R, A]`, the precise leaf.** Types only
  the diagonal. One `Bind` relates THREE leaf types — left
  `(·=>S)=>R`, continuation results `(·=>S2)=>S`, whole `(·=>S2)=>R` —
  and `Free.Bind[F, A, B]` has one `F`: the node's intermediate answer
  type has no home. `PState.set` changes the answer type, so the
  diagonal is not enough.
- **`(X => ?) => R`.** Fails twice: at `shift`, because a wildcard is a
  SUPERtype and a function's argument slot needs a SUBtype of every
  `A => S`; and at `bind`, for the same reason as the precise leaf.
- **`(X => ?) => ?`.** `bind` now passes — the two wildcards do unify
  the three positions — and only `shift` fails, on the variance point
  above. The one legal spelling of "unknown in the argument slot" is
  `Nothing`, of "unknown in the result slot" `Any`; so this is `Shift`
  before the variance check, and it needs the same cast at run.

**A trap worth its own line: a top-level `opaque type` is transparent
to its whole PACKAGE, not its file.** Declared at top level, `Cont`
was plainly `Free[Shift, A]` in Generate.scala, `Stream`'s
program-carrier `map` captured the for-comprehension, and — as a plain
alias — `Free`'s MEMBERS beat `Cont`'s extensions and switched
absorption off. The representation had to move inside `object Cont`.

- Stage 1: REFUTED as specified; superseded by the facade above, which
  gets the whole of its goal (one base, `Free` untouched) by the
  opposite move.
- Stage 2: BUILT 2026-09-23 — see "Stage 2 — BUILT" below, after the
  probe that decided its shape.

### Stage 2 — BUILT (2026-09-23, lane freer-base-stage2)

`Prog[F, A, S, R]` (src/main/scala/Prog.scala), `Delim.Stacked`
(Delim.scala, the end of the companion) and okay-sql's `Tx` (Tx.scala)
are in; TestProg and TestTx are the boxes above. What the build found
that the probe could not, each a round that failed:

- **The opaque type goes INSIDE the companion**, exactly as `Cont`'s
  `Rep` does (cont-facade-over-free): a top-level `opaque type` is
  transparent to its whole PACKAGE, and the first cut's `k(5).map(_ *
  2)` in a package-okay test typed the lambda's argument as the whole
  program — the package's program-carrier `map` had captured it. As
  `Prog.Rep` behind the alias `Prog`, five of the seven errors went.
- **The other two were `Comonad[Id]`**, the package-level given whose
  extension puts `.map` on EVERY type: in lexical scope for all of
  package `okay`, and for any user file with `import okay.given`, it is
  closer than the facade's companion and wins
  (lexical-extension-beats-companion — `Static` became a class for
  this; an opaque alias cannot). The answer is one import beside the
  given, `import okay.Prog.{flatMap, map}`, stated in `Prog`'s doc, in
  docs/guide.md, and exercised on purpose in TestTx. `inline` was not
  the cause (tried, refuted).
  REVISED 2026-09-23 (comonad-id-map-capture): with `Comonad[Id]`
  moved into its companion the capture is gone, and the import is
  STILL needed — dropped, TestProg's stacked shapes fail with "value +
  is not a member of A" and TestTx with `Required: Prog.Rep[Async, B,
  R, T]`. The resolved method is `Prog`'s own in both; what fails is
  inference of the continuation's type when the extension is reached
  through the companion's implicit scope rather than lexically. So
  the capture masked a second cause, and the import answers that one.
- **`push`/`run` need their type arguments spelled** inside `Stacked`:
  from an argument typed `R ! ([A] =>> Delim[A] | F[A])` the compiler
  does not recover `Delim + F`. Two call sites, explicit `[R, F]`.
- **Running an `Async` program takes `CanBlock`**, which JS does not
  have, so TestTx is a `scala-jvm` test; `Tx` itself is cross.
- **Zero cost, structurally**: `diag(p).free eq p`, and `flatMap`
  builds a `Bind` whose head `eq p` — the same nodes, asserted, no
  benchmark, since no node changed.
- **`shift0`/`control0` are not stacked** (their body's stack is the
  part BELOW the prompt, a match type the probe never exercised); the
  unstacked doors remain. `control` and `abort` are. UPDATED
  2026-09-25 (stacked-shift0, specs/shift0-dollar.md stage 2): `shift0`
  and `dollar` are stacked now. `Below` is a type member of `Has`
  rather than a match type, because prompt singletons are not provably
  disjoint. That lane also found that `shift`/`control` typed their
  body under the WHOLE stack, which was unsound. `control0` stays
  unstacked, for the reason given there.
- **specs/delim-safety.md's stage-2 road was not taken.** The region
  tag needs `S` in the program's type; the given stack closes the
  escape case with one `import s.given` instead, and leaves the four
  inline doors untouched because it is a separate door beside them.

### Stage 2, the Delim half — the identity question, answered by compiling

Stage 2's first behavior item is that `NoPrompt` — the exception
`Delim.scala:223` throws when a shift names a prompt that is not
installed — becomes a compile error. Everything in that item rests on
one question nobody had asked the compiler: a `Prompt[R]` is made at
RUN time by `reset`, so can its IDENTITY, not merely its answer type,
reach the type level?

**It can.** `scripts/stage2-prompt-identity-probe.scala` is the whole
answer in one runnable file: five positives compile and three
negatives are refused, including the case that matters most — a prompt
that ESCAPES its `reset` and is shifted to afterwards, which is
precisely today's throw.

This was asked FIRST, before any lane was claimed, because stage 1
died on exactly this class of question after the implementation was
written. The two questions are cousins and their answers are
opposite: stage 1 needed the compiler to SKOLEMIZE a free parameter in
an `unapply` and it inferred `Nothing` instead; stage 2 needs a
singleton `p.type` of a term parameter in a dependent signature, which
the language supports outright.

The shape that works, and each piece of it is scar tissue:

```scala
final class Stack[S0 <: Tuple]:      type S = S0   // the stack in force
final class In[R, S <: Tuple](val p: Prompt[R]):
  given stack: Stack[p.type *: S] = new Stack      // published, imported
def reset[R](using st: Stack[?])(
  body: (s: In[R, st.S]) => Prog[R, s.p.type *: st.S, s.p.type *: st.S]
): Prog[R, st.S, st.S]
def shift[R, A](p: Prompt[R])(using st: Stack[?], ev: Has[st.S, p.type])(
  f: (A => Prog[R, st.S, st.S]) => Prog[R, st.S, st.S]
): Prog[A, st.S, st.S]
```

**Four compiler facts were paid for to arrive at it**, each by a round
that failed, and they are why the obvious spellings are absent:

1. **An expected type is not enough.** It fixes the indexes for
   `val x: Top[Int] = reset { … }` and for nesting, but the HEAD of a
   for-comprehension has no expected type — `x.flatMap(…)` types `x`
   first — so the stack index fell back to its bound `Tuple` and the
   `Has` search failed. Since TestDelim, okay-ui and okay-agent all
   write for-comprehensions, this alone decides that the stack must be
   a GIVEN rather than an inferred parameter.
2. **A curried dependent context function is refused outright**:
   `(p: Prompt[R]) => Stack[p.type *: S] ?=> Prog[…]` — the obvious way
   to hand the body both the prompt and the stack — answers
   "Implementation restriction … not yet supported".
3. **A non-curried one compiles**, and nested witnesses of the same
   shape resolve to the INNER one with no ambiguity, which the nested
   case needs. But carrying the stack through it CRASHES the compiler:
   `java.lang.AssertionError: wildApprox failed to remove
   uninstantiated R`, in implicit scope computation. That road is
   closed by dotty, not by the design.
4. **Clause ORDER decides inference.** A `using` clause after the
   continuation loses: the lambda is typed first and pins the stack to
   `Tuple`. It goes before — and the stack is a type MEMBER, so no
   call site ever spells it and no method carries a stack type
   parameter that inference can pin too early.

**What it costs at the call site**, which is the number that decides
whether the lane is worth taking: `reset { p => … }` becomes
`reset { s => import s.given; … }`, one line per reset, and the prompt
is `s.p`. `reset` in a generator position needs its answer type
(`reset[Int] { … }`). The type arguments on `shift` are NOT a new
cost: TestDelim writes `shift[Int, Int, okay.Pure](p)` at every call
today.

**What is still unpriced**, and what the lane must not assume:

- Delim's four capture variants (`shift`, `shift0`, `control`,
  `control0`) differ in whether the body CONSUMES the delimiter.
  `shift0` and `control0` pop it, so their index is
  `Prog[A, p.type *: S, S]` rather than the balanced shape — the
  probe only exercised the balanced one.
- `Delim.abort`, and the spec's own first caveat: an abort inside a
  block promising a transition drops the continuation, so the
  transition does not happen. The type says it did. That caveat is
  already a required test in Behavior and it stays required.
- The four files outside the core that name Delim — `Scope` and
  `Screen` in okay-ui, `Stepper` in okay-agent, `Cut` in okay-llm —
  are where the one-line-per-reset cost is actually paid, and none of
  them has been read for this.
- Nothing here is measured. The indexes are phantom and the facade
  erases, so the expectation is allocation identical to the byte, the
  way `cont-on-free` measured — but an expectation is not a number.

### The dual placement, PROBED (2026-09-29, freer-base-step-extractor)

Stage 1 said where the index may not live — on the nodes — because a
match makes it existential. The question the operator put back
(2026-09-29): the old separate `Cont` was an indexed enum and worked
without a cast; can one base be indexed like it and still serve
`Free`? The answer is yes, with ONE cast, and the probe that compiles
it is `src/test/scala/ProbeFreerStep.scala` (kept compiling, like
ProbeRowCrash). What stage 1's extractor lacked is exact:

- `unapply[F, X, A, T](b: Bind[Lift[F], X, A, Unit, T, Unit]): Bind[Lift[F], X, A, Unit, Unit, Unit]`
  — the pattern-bound type variables in the PARAMETER type, so the
  compiler inserts the type test that binds them (stage 1 put `X`
  only in the result, and dotty infers a result-only variable as
  `Nothing`); the result is the node itself, a Product, so the match
  allocates nothing (bytecode `aload_1; areturn`).
- `Lift[F] = [X, S, R] =>> F[X]`, a type lambda in the signature slot:
  `Op(g: G[A, S, R])` reduces to `F[A]` at a match, the existential
  is gone by beta-reduction, and `F` is inferred through it at an
  abstract `F` (`runFree[F[+_], A]`) — the shape row-membership-crash
  made suspect, and it did not crash.
- `resume` once, index-polymorphic, no cast: `Bind(Bind(a, f), g)`
  types through the two intermediates as the old Cont's runner did.
- Cont's runner typed by the GADT: `Return` gives `S = R`, the leaf is
  `(A => S) => R` — `Shift.at` and `pinned` both go.

Casts: two on the facade (trusted at two nodes) against one on the
erased side (a constant claim: every Lift tree is built at Unit).
Refused by the compiler: answer types that do not meet in a bind, a
continuation of the wrong answer type, `Step` on a concrete Cont
(E030), `Step` on an abstract-G tree (E092 — red under "no
warnings"). Let through: `Step` on `Cont[A, R, R]` with R a method
type parameter, which the GADT may bind to Unit — so `Step` is
`object !`'s and is applied to `Free[F, A]` scrutinees, the standing
of `(x.resume: @unchecked)` today. Not measured, and not expected to
move: the nodes are the same objects. The lane is
backlog.d/okay-core/freer-base-step-extractor.md.

### The dual placement, LANDED (2026-09-29, freer-base-step-extractor)

The probe above is the base now. `src/main/scala/Free.scala` holds
`enum Freer[G[_, +_, +_], S, +R, +A]` — `Return | Inject | Bind |
Delay`, one `resume`, index-polymorphic, no cast — and
`type Free[F[+_], +A] = Freer[Lift[F], Unit, Unit, A]` beside an
`object Free` whose `Return`, `Inject`, `Bind` and `Delay` are the four
names at their old arities, constructors and patterns both. `Cont`'s
`Rep[A, S, R]` is `Freer[Shift, S, R, A]` with `Shift = [S, R, X] =>>
(X => S) => R`; `Shift.of`, `Shift.at` and `pinned` are gone from
`Cont.scala`, and `Cps.walkWith` with them.

What the probe did not predict, each found by the compiler:

- **`A` last.** The probe's `Freer[G, A, S, R]` broke every place a
  unary constructor is inferred from a program value (`Monad[M]` from
  an `A ! F`, `Stream[S, F]`, `Applicative`): dotty abstracts an
  applied type over its LAST parameter, and after dealiasing `Free`
  that was `R`. `Freer[G, S, R, A]` puts `A` last and every instance
  infers as before.
- **`Lift` is a class projection, `Lifted[F]#L`.** As a bare lambda,
  `Lift[Users + F]` against `Lift[F1 + G]` beta-reduced to `Users[X] |
  F[X]` against `F1[X] | G[X]`, and dotty solved `F1 := Users + F`:
  `!.tracing(p)([X] => (e: Users[X]) => …)` stopped typing. A
  projection compares by its prefix, `Lifted[Users + F]` against
  `Lifted[F1 + G]`, and the row's `+` matches application to
  application as it did on the old enum. Side effect, recorded in
  `ProbeRowInference`: an argument typed as the EXPANDED union `[A]
  =>> Delim[A] | F[A]` now satisfies `R ! Delim + F` without explicit
  type arguments, which the old enum refused (shape 3 there pinned the
  refusal; it pins the acceptance now).
- **`+R`.** `Cont.tailShift[A, S, R]` emits `Return(v): Cont[A, S, S]`
  for a body whose own typing said `S <: R`; on an invariant base that
  is not a `Cont[A, S, R]`. The base is covariant in `R` (the Variance
  decision, above), and the macro summons `S <:< R` at the call site
  (`Expr.summon`, where the types are concrete) and hands it to
  `tailShift`/`tailPure`, which `liftCo` through the tree; a body
  where the evidence is not found stays a leaf.
- **Two class tests stay `@unchecked`.** An absorbed `Leaf[A, S, R]`
  and a `Cps[A, S, R]` are subclasses of the leaf type `(A => S) => R`
  at the same arguments; a type test with bound variables (`case l:
  Leaf[a, s, r]`) does not derive them through a function type, so
  they are `Leaf[A, S, R] @unchecked` — the one claim a class boundary
  keeps, and a smaller one than `Shift.at`'s, which trusted the
  arguments of EVERY leaf.
- **Seven `split(e) { case Say(w) => … }` sites** in Writer and
  Chronicle needed `(w0: @unchecked) match`, as their `Bind` twins
  already had: `Free.Inject`'s pattern types `e` at the program's own
  answer type rather than a GADT skolem, and the exhaustivity checker
  then cannot see that `Say` is the only constructor.
- **The Layer 1 B walk keeps one cast**, renamed `walked`: the pending
  stack's typing is dynamic (a body being walked answers through the
  parts pushed for it, while the loop's `c`/`k`/`R` are the step's),
  which is about `Pending`, not the tree, exactly as the item said.
- **`Prog` is untouched.** It stays an opaque facade over `Free[F, A]`
  with identity doors (`diag`, `transition`, `free`); the probe's
  third signature `Typed[F] = [S, R, X] =>> (F[X], S => R)` would put
  a transition FUNCTION on every leaf — a different, allocating design
  — and the item's step (4) is closed as "not this lane".

Casts on the tree: two to one. Gate: the core on JVM, Scala.js and
Scala Native (612 / 10 / 14), then `affected origin/master staged`
over the family. Not measured here: `scripts/jmh-lane.sh` needs the
Mac; the lanes to re-read are the item's, and any movement is a
defect, since every node is the same object.


### Freer as the ParaMonad, and the two readings of an indexed signature (2026-09-30, freer-paramonad)

The operator asked for `Freer` to be the `ParaMonad` instance, and
behind it the question this base had not been asked: an effect whose
SIGNATURE carries the indexes — typestate as a `Get`/`Put` enum a
handler can look at, not `PState`'s shift bodies — how is it handled
on this tree, and does "erase to `Unit` in `Free`, reintroduce in
`PState`" have to be the road?

**The instance** is one `given` in `object Freer`: `ParaMonad[Freer.
Para[G]]` for every `G`, with `Para[G] = [A, S, R] =>> Freer[G, S, R,
A]` because the trait reads value-first and the tree keeps `A` last
for inference. `pure` is `Return`, `flatMap` is a prefix `Bind` (the
extension-in-override self-recursion `Cont.bind` documents), `map` is
the `Mapped` bind so builders keep reading it. `Control[Cont]` is the
same structure at `Shift` with absorption, and `Cont` is opaque, so
no search meets both; the effect row's `Monad[Free[F, *]]` still
resolves beside the diagonal bridge — TestFreerPara pins both.

**Where PState already stands.** Since the dual placement landed
(2026-09-29) `PState` is NOT the erased road: `Cont.Rep[A, S, R]` is
`Freer[Shift, S, R, A]`, so `PState.get: Cont[S, S => R, S => R]` is a
leaf of the indexed tree with the state's type ON the node. What is
erased to `Unit` is only the unary effect row, `A ! F`, because a
unary `F[X]` has no answer type to put there.

**Two readings of `Freer[G, S, R, A]` for a three-ary signature,
compiled** (src/test/scala/TestFreerPara.scala):

1. *The index is an ANSWER TYPE* — Cont's, PState's. `enum PSt[S, +R,
   +X]` with `Get[S, Z]() extends PSt[S => Z, S => Z, S]` and `Put[S,
   T, Z](t) extends PSt[T => Z, S => Z, S]` is `PState` as data, the
   same signatures its `get`/`set` carry. A handler is an INDEXED
   NATURAL TRANSFORMATION `[s, r, x] => G[s, r, x] => (x => s) => r`
   — it chooses the shift body (`getAt`/`setAt`), which is the one
   sentence this spec has carried since its overview — and the runner
   is Cont's at any `G`, typed by the GADT end to end (`Return` gives
   `S <: R`, `k(a): S` is the `R`). A program moving `Int -> String ->
   List[String]` runs; a `Put` whose index does not meet the
   continuation's is E007. This reading is what the base's variance
   was designed for, and it is how an indexed effect is made here:
   write the signature with the answer types, hand its handler to
   Cont's runner (or `translate` it into `Cont` and absorb).

2. *The index is a CONSUMED state* — McBride's `IxFree`, `R` the state
   before, `S` after, `enum St[S, +R, +X]` with `Get[S]() extends
   St[S, S, S]`, `Put[S, T](t) extends St[T, S, Unit]`, the handler a
   loop `runSt(p)(r: R): (S, A)`. REFUSED, and by more than predicted:
   the `Return` arm holds an `R` and owes an `S` with only `S <: R`
   (the covariance `+R` that `tailShift` needed), and the `Get` arm
   hands `r: R` to a continuation whose argument the match bound as a
   SUPERtype of the case's own state, not as `R` — the signature is
   covariant in `R` and `X` because the base's bound `G[_, +_, +_]`
   says so, so the GADT yields bounds where an invariant enum gave
   equalities. Only the `Put` arm, which PRODUCES the next state,
   types. Pinned by `compileErrors` (two errors, both `Found: r: R`),
   so the next change to the base's variance re-asks it.

The principle, stated once: **on this base an index is something a
handler PRODUCES, never something it consumes.** Threading a state
through the answer type (`S => Z`) is not PState's trick around a
missing feature; it is the only reading `Freer[G, S, +R, +A]` admits,
and the reason is the same function-type arithmetic that put `+R`
there. A consumed-state base would need `-R`/`+S` — the opposite
variances — which is to say a different tree, and `Cont` would not
fit it. Two consequences for the open items:

- `Prog`'s phantom index and `Delim.Stacked` stay as they are. Their
  index is a protocol CLAIM (a prompt stack, `Idle -> Open`) that the
  machine reads at run time from prompt VALUES; nothing on the tree
  could check it, and moving it onto the nodes would put a consumed
  index under `+R`. The three-ary row algebra this spec lists as out
  of scope is still out of scope: reading 1 needs no row — a
  three-ary signature is handled by ONE natural transformation into
  `Shift`, and combining it with a unary row is `PState`'s existing
  road (a `Cont` program handling `A ! F` operations by `reflect`).
- backlog `cont-variance` (`Rep[+A, S, +R]` on the facade) is
  consistent with reading 1 and gains nothing for reading 2.

Not measured: nothing on a hot path changed. The instance builds the
nodes the tree's own `flatMap`/`map` build, and the runner in the test
is a probe, not a production loop (its re-entry is direct style's
frame, as `ProbeFreerStep`'s).

### The indexes INSIDE the effect system — a row and a handler (2026-09-30, freer-paramonad-row)

The operator's follow-up: not one three-ary signature, but WHERE in
the effect system — rows, handlers, forwarding — the indexes are used.
Compiled, in TestFreerPara, and green at the first typing:

- **A mixed row.** `Row = [S, R, X] =>> PSt[S, R, X] | At[State[Int,
  *], S, R, X]`: the indexed effect beside an ordinary `State % Int`.
  The union's `+` is the unary one written at three parameters;
  nothing else changes — `split`-by-class is index-blind.
- **A unary effect enters ON THE DIAGONAL, and that is the one new
  thing.** `Lift[F]` puts a unary operation at any index, and a
  handler's loop over a mixed row cannot use that: matching
  `Bind(Inject(op), k)` makes the middle index `T` existential, and
  the answer the handler builds from `k`'s program is `T`-indexed
  where it owes an `R`-indexed one. `enum At[F, S, +R, +X]` with
  `Op[F, R, X](e: F[X]) extends At[F, R, R, X]` says the operation
  moves nothing; the GADT gives `T <: R` and `+R` makes the
  continuation's program an `R` one. Price: one wrapper per unary
  operation. The allocation-free twin is `Free.Bind`'s trade at
  `Unit` — an extractor that claims "a unary operation is diagonal"
  by one cast, applied after the class test says the op is unary.
  Which to take is a measurement, not taken here.
- **State's handler, unchanged in shape, over the indexed row.**
  `counted(s)(p: Freer[Row, S, R, A]): Freer[PSt, S, R, (Int, A)]`:
  its own operations answered from the threaded `Int` and continued
  at `T <: R`; a `PSt` operation FORWARDED with the index it came with
  — `Inject(o).flatMap(x => counted(s)(k(x)))`, `(T, R)` then `(S,
  T)`, closing at `(S, R)` — exactly the forwarding arm every handler
  in the library has, at indexes that are no longer `Unit`. A program
  ticking the counter around a `PSt` move `Int -> List[String]` runs
  through `counted` then the indexed natural transformation and
  answers `(List("x", "x"), (3, 3))`.

So the answer to "where": in every handler's forwarding arm, which
already has the right shape, and in the doors — an indexed effect's
smart constructors carry their answer types, a unary effect's carry
the diagonal. What production would add, all of it mechanical and none
of it taken here: `+` at three parameters, `split`/`TypeableK` over
three-ary constructors, `!`'s doors at a non-`Unit` index, and the
diagonal claim for unary operations chosen between `At` and the
extractor. Handlers stay Cont-valued (`F !> S` is `X /> S`); what
moves is only that a handler of an INDEXED effect answers `Cont[X, S,
R]` off the diagonal, which is `PState`'s `getAt`/`setAt` given a
data operation to read.

### McBride's reading is refused by the variance ALONE — probed on the invariant base (2026-09-30, freer-mcbride-probe)

The operator asked what the problem with the consumed-state index is,
what is lost without it, and what it would give. `src/test/scala/
ProbeMcBride.scala` writes the SAME `St` signature and the SAME loop
TestFreerPara pins as refused, against `ProbeFreerStep`'s invariant
copy of the enum, and it types — kept compiling, exercised by
TestFreerPara:

- **No continuation object at all.** `run[A, S, R](p: Freer[St, A, S,
  R])(r: R): (S, A)` is `State.handle`'s loop with the type moving:
  `Return` gives `S = R` so `(r, a)` is the pair owed; `Get` gives
  `X = T = R` so the continuation takes the state held; `Put` hands
  `t: T` on. No `k`, no `Reentry`, no room, no switch — the whole of
  Cont's stack machinery exists because a shift body CALLS `k`, and
  this handler calls nothing.
- **`@tailrec` with the type arguments changing per call.** Every
  recursive call is at a different index; Scala 2 refused that
  ("called recursively with different type arguments") and Scala 3
  accepts it, so the loop is the fast shape, not an erased inner loop.
- **The type still refuses** a `Put` from the wrong state and a run
  from a state of the wrong type (`typeCheckErrors`, in the test).

So the obstacle is `+R` and the bound `G[_, +_, +_]`, and nothing
deeper. What `+R` is FOR, by grep: `Cont.tailShift`/`tailPure`'s
`liftCo` — the macro's tail-shaped body emitted as a `Return(v):
Cont[A, S, S]` where a `Cont[A, S, R]` is owed, with `S <:< R`
summoned at the site. Nothing else reads the base's covariance in `R`
(`Delim`'s `liftCo[Prog]` is on the value; the facade `Rep` is
invariant, backlog `cont-variance`). The price of McBride's reading on
the library's base is therefore exact: `S` and `R` invariant on
`Freer`, `G[_, _, +_]`, and `tailShift` placing its `Return` at
`(S, R)` by ONE cast justified by the evidence it already holds (or a
`Return` case carrying the evidence, +8 B on every `pure`). Both
readings then live on one invariant tree, each as its own signature.

**What is lost without it.** A type-changing state — and any effect
whose handler CONSUMES its index: a held resource typed `Handle[R]`,
a session's channel at its protocol state — can be handled only
through the answer type, which is CPS: `PState` costs a frame per
operation, a `Reentry` per bind and the room/switch bookkeeping, and
measures 1.29x `State.handle` on the same workload (State.scala's
header, 21.23 vs 27.42 µs). McBride's loop is `State.handle`'s own
shape, tail-recursive over `resume`, nodes only. The typed protocol
would then cost what the untyped one costs.

**What McBride's index gives beyond Atkey's, and what this tree cannot
express.** In "Kleisli arrows of outrageous fortune" the value is a
FAMILY over the index, `a : I -> Set`, so the state after an operation
may depend on the VALUE it answers — `tryOpen` answering `Opened` at
`Open` or `Failed` at `Closed`, the continuation typed for both. That
needs `Bind`'s continuation polymorphic in the index, `[j] => A[j] =>
M[B, j]`; this tree's `f: A => Freer[G, S, T, B]` fixes `T`. Atkey's
encoding of the same is a sum-typed STATE, `Either[Open, Closed]`,
which `Stage.phased` already runs (`S1 -> Either[S1, S2]`), the next
operation matching on it. So: the value-dependent post-state stays
encoded, the consumed index is one variance decision away.

Not measured — a probe, not a lane. The lane, if the trade is wanted,
is backlog `freer-consumed-index`; it is in tension with
`cont-variance`, which asks for MORE covariance on the facade, and one
of the two has to be chosen.

### The indexes INVARIANT — both readings on the library's base (2026-09-30, freer-consumed-index, the operator's decision)

"Делаем S и R инвариантными." `enum Freer[G[_, _, +_], S, R, +A]`:
`+A` stays, `+R` and the covariant bound on the signature's second
parameter go. What the compiler then said, each a round:

- **`+R` had TWO readers, not one.** `tailShift`/`tailPure`'s `liftCo`
  (known) and `Cont.noProgram`, the placeholder a CPS walk starts from,
  a `Return` at index `Nothing` that rode the covariance into every
  walk's `R`. The first is one cast in `Cont.tailAt`, the evidence as
  its parameter; the second is a `Delay` that throws, at the walk's own
  index, no cast — it is never matched (a walked body answers through
  its pending parts).
- **A signature with an INVARIANT value parameter silently switches
  the GADT off.** `enum St[S, R, X]` against the bound `G[_, _, +_]`
  typed its doors and then derived NOTHING in the handler's match —
  not `S = R` at `Return`, not even `A0 <: A` — eight errors that read
  like the variance decision had not happened. Bound conformance is
  checked after typing, so the kind mismatch never surfaced as itself.
  `St[S, R, +X]` and every arm typed. A signature's value parameter is
  `+X`, and a handler that derives nothing from a match should check
  the signature's kind before anything else.
- **Doors on an invariant signature spell their type arguments.**
  `Inject(St.Get())` no longer infers `G` from the expected `Freer[St,
  S, S, S]`; `Inject[St, S, S, S](St.Get())` does. The library's own
  doors (`Free.inject`, `Cont.shiftLeaf`) already spell them.
- **The row probe's `At` and `PSt` are invariant in `R` too**: with
  `+R` on the signature the GADT gave `T <: R` where the loop owes an
  `R`-indexed program, and on an invariant base that is no longer
  enough. A signature's variance is now exactly the base's.

The reading-2 pin flipped: `runSt[S, R, A](p: Freer[St, S, R, A])(r:
R): (S, A)`, `@tailrec`, no continuation object, runs `Int -> String ->
List[String]` on the library's `Freer` and still refuses a `Put` from
the wrong state and a run from the wrong state. TestCont, TestContMacro,
TestContStack, TestState and TestProg are green unchanged, so the
Cont side lost nothing to the cast it now carries. Not measured: the
nodes are the same objects; `tailAt` is erased.

### The diagonal leaf as a case of the node (2026-09-30, freer-diag-leaf, the operator's "Да")

The first item of the from-scratch list in freer-consumed-index's
answer, built: `Freer.Diag[G, R, A](a: G[R, R, A]) extends Freer[G, R,
R, A]`, the fifth case, and `Freer.diag` as its door. It says on the
NODE what `Lift`'s phantom index cannot: the operation moves nothing.
Matching `Bind(Diag(e), k)` gives `T = R` by the GADT on the invariant
base, so a handler's loop over a mixed row continues at `R` — which is
what the row probe needed and had from a wrapper (`At.Op`, one
allocation per unary operation) or would have had from an extractor
claiming the diagonal by one cast, `Free.Bind`'s trade. Neither now:
the row is `[S, R, X] =>> PSt[S, R, X] | State[Int, X]`, the unary
member BARE, `tick[R] = Freer.diag[Row, R, Int](State.Modify(_ + 1))`,
and `counted` answers `State` under `Diag` and forwards `PSt` under
`Inject` with its index. A lone `Diag` and one under a `Bind` both run.

What the compiler asked for, each a round:

- **One exhaustive match over the erased tree exists**, `!.peek`
  (Effects.scala); every other site is `(x.resume: @unchecked)`. It
  gained a real arm — at `Unit` a `Diag` holds the same `F[A]` an
  `Inject` does — through `Freer.Diag`, since `object Free` keeps only
  the four old names on purpose (the `direct` macro looks them up by
  symbol) and `Diag` is not one of them.
- **Cont's `step` is `(c: @unchecked) match` now**, not a dead arm: a
  Cont never holds a `Diag` — the companion builds every leaf, as
  `Inject`, so it can be absorbed — and bytes in that loop are what
  the Fib lanes price (cont-stack-fastpath, "callee is too large").
- **`Free`'s doors at `Unit` keep building `Inject`.** The 112
  `Bind(Inject(e), k)` sites across the family do not move, and at
  `Unit` the two nodes mean the same thing. `Diag` is the door of an
  INDEXED row only; the discipline is a door's, as `Free.Bind`'s
  constant claim is, and a handler of a unary effect in an indexed
  row matches `Diag`. What the type does not refuse: a unary
  operation put under `Inject` at a moving index by hand. The row
  probe's `moving` arm throws on it by name; closing it at the type
  is the three-ary row algebra's job (a `+` whose unary member is a
  match type reducing only on the diagonal is the road, untried).

Not measured, and the reason is honest: the CI runner was gating the
box beside this lane all morning. The one thing that could move is
the JIT's view of `Freer`'s sealed hierarchy (five cases now), and no
loop tests for `Diag`; if any core lane moves at the next reading,
this is the change to bisect to.

### Re-measured: the invariant indexes and the diagonal leaf cost nothing (2026-09-30, freer-base-remeasure)

Two base changes landed unmeasured on 2026-09-30 because the CI runner
gated the box beside them. Read afterwards on a quiet box: `mine` =
78f8dfec2 (freer-consumed-index + freer-diag-leaf) against `ref` =
7ae9a6aa9 (master just before them; the arms differ in Free.scala,
Cont.scala, ContMacro.scala and Effects.scala only), three alternating
rounds, MIN per lane, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`, JDK 26,
load 2.6-4.9, all 24 lanes with the script's "box stayed quiet"
verdict (history.d `2026-09-30T095512Z-freer-base-remeasure.tsv`):

| lane | mine | ref | ratio | B/op |
|---|---|---|---|---|
| `fib100` | 2277 ns | 2268 ns | 1.004 | 21 552 both |
| `statePara` | 30.88 µs | 30.57 µs | 1.010 | 301 408 both |
| `relayForward` | 168.0 µs | 167.9 µs | 1.001 | 1 995 617 both |
| `stepBulk` | 192.5 µs | 193.4 µs | 0.996 | 2 319 825 both |

UNCHANGED, as predicted: the nodes are the same objects, `tailAt`'s
cast is erased, and the fifth enum case did not change the JIT's view
of the `Inject`/`Bind` type tests on the Free side (`relayForward`,
`stepBulk`) or of the leaf loop (`fib100`, `statePara`). Round 1 read
`fib100` at 1.019 and rounds 2-3 at 1.000 and 0.978 — the inlining
lottery this spec has met before, and why a single round is a
hypothesis. The lanes the freer-consumed-index and freer-diag-leaf
entries named as "the change to bisect to" need no bisecting.

### PState as data through the threading loop — the payoff, measured (2026-09-30, pstate-threaded)

Item 2 of the from-scratch list, and the number the invariance
decision was for. `PState.Op[S, R, +X]` (`Get[S]` at `(S, S)`,
`Put[S, T](t)` from `S` to `T`, answering the old state as `set`
does), `PState.Threaded[A, S, R] = Freer[Op, S, R, A]`, doors
`Threaded.get`/`put`, and `Threaded.run` — `State.handle`'s loop with
the type moving, `@tailrec`, no continuation object. TestState pins
the protocol (`Int -> String -> Boolean`) and the refusal of a `Put`
from the wrong state. HandlerBenchmark `stateThreaded` is the same
M = 1000 workload as `statePara`. Three lanes on one tree, order
rotated per round, MIN of 3, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`,
every lane quiet (history.d `2026-09-30T104006Z-pstate-threaded.tsv`):

| lane | µs/op | B/op | vs `stateEffect` |
|---|---|---|---|
| `stateEffect` (untyped `State`) | 16.96 | 244 904 | 1.00 |
| `stateThreaded` (typed, data road) | 18.07 | 276 904 | **1.07** |
| `statePara` (typed, shift road) | 30.39 | 301 407 | 1.79 |

The typed protocol costs 7% over the untyped State on the data road
and 79% on the shift road, so the data road is 0.59x of what `PState`
paid. What the invariance bought is exactly this loop: a handler that
CONSUMES its index and continues at the type the operation gives it.
The 32 000 B the data road still carries over State are one
`Inject(Get())` allocated per read, where `State.get` shares one node
through a cast (`SharedOps.getNode`); the same cast here would close
the bytes and is the next rung, priced on its own. The shift road's
own ratio moved from the 1.29x State.scala's header quoted on
2026-09-17 to 1.79x today — the re-entry road's cost is the JIT's
inlining decision, not a constant — and the header says so now.
Which road: the shift road for what only it can do (a body that uses
`k`, the profunctor `Zooming`), the data road for a protocol.
