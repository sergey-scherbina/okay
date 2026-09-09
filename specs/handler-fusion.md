# Handler fusion: one composite handler for a row, staged at compile time

## Overview

A program over a row `F1 + F2 + … + Fk` is run today in one of two
ways, and they are not the same cost. If every effect has a comonadic
`Handler` (an operation answers with a plain value), `runWith` already
runs ONE pass: `Handler.union` assembles a composite handler and
`runFree` is a single tail-recursive loop. But the effects that need
the continuation — `Writer`, `State`, `Throws`, `Choice`, `Reader`'s
`local` — are run ONE AT A TIME: `Throws.run(State.handle(s)(Writer.run(p)))`
is three walks over the program, and at every walk each operation the
walker does not own is REBUILT (`Effect(g).flatMap(k)` — a fresh Bind
node and a closure) for the next walk to find. For a row with k such
effects an operation is rebuilt up to k−1 times and `resume`'s
rotation is redone k times. That is Kiselyov's freer cost, and it is
the cost this spec removes.

The operator's proposal (2026-09-09): instead of running handlers one
after another, COMPOSE the handlers into one handler first, then run
the program once with the composite — and use staging to build and
optimize that composite. This spec is the assessment of that idea
against what the repository already has, and the design that follows.

The assessment, in one paragraph: the idea is right, it is already the
library's design for the comonadic class, it is licensed by theory the
repository already claims, and the literature has done it three times
under three names — fold fusion (Wu & Schrijvers, *Fusion for Free*,
MPC 2015: handlers are folds over the free monad, and by initiality
`fold h2 ∘ fold h1` fuses into one fold), evidence passing (Xie,
Brachthäuser, Hillerström, Schuster & Leijen, *Effect Handlers,
Evidently*, ICFP 2020, and Xie & Leijen, *Generalized Evidence
Passing*, ICFP 2021: the whole handler stack travels as a vector and
an operation indexes its handler directly, so tail-resumptive
operations become direct calls with no forwarding chain), and
capability passing with staging (Schuster, Brachthäuser & Ostermann,
ICFP 2020, and the Effekt line, already cited in theory/08). Okay's
`Free` is initial and `Eff` is the Church encoding — `Eff` IS the
fused form, and `fromFree`/`reify` are the initiality that makes
fusion a theorem here rather than an optimization one hopes is sound.
What is missing is not the principle but the ARTIFACT: a composite
`!>` interpreter for a row of continuation-aware handlers, and a
dispatch that does not pay a type test per effect per operation.

Two things the proposal gets right that must survive into the design,
and two hazards that must not:

- RIGHT: build the composite once, run once. For `Eff` the program is
  literally `[S] => F !> S => A /> S`; give it one composite `!>` and
  the single pass is already there. For `Free`, `runFree` is the
  precedent — one match over the tree with the handler inlined.
- RIGHT: staging. The row is a TYPE, so the composite's shape is known
  at the call site; the composite should be assembled by `inline` at
  compile time, so that the dispatch becomes one flat `match` and each
  handler's step is inlined into the loop.
- HAZARD 1: order is semantics. `State.run(s)(Choice.run(p))` and
  `Choice.run(State.run(s)(p))` are DIFFERENT programs (global state
  vs. state that backtracks); `Throws` over `Writer` decides whether a
  log survives an error. Composition is therefore an ORDERED stack and
  must never be presented as commutative; the composite must reproduce
  the nested meaning for the order the user wrote. Fusion for Free
  handles this by composing the layers' CARRIERS in order — the answer
  type of the fused handler is the transformer stack of the layers'
  answer types.
- HAZARD 2: the refuted road. `specs/staged-effects.md` measured
  run-time closure composition (`foldIn[Func]`) as "staging" and found
  it 1.07x SLOWER than the tail-recursive Cont walk; the staged
  artifact that won (1.9x) was an inline handler-passing program
  evaluated at compile time. A generic "compose any two `!>` handlers"
  combinator that builds the composite from closures at run time would
  be that refuted road under a new name. Run-time quoted staging
  (`okay-staging`, `scala.quoted.staging`) is also the wrong tool: it
  is JVM-only, single-threaded by the compiler's own rule, and pays a
  generation cost per composite — for a shape that is fully static at
  the call site. Compile-time `inline` is the only staging this spec
  admits.

## Interface

Nothing existing changes signature or meaning. Added:

```scala
// 1. flat dispatch for comonadic rows: the same Handler[F + G] as
//    Handler.union, assembled inline so the nested <|> tests unroll
//    into ONE match over the row's operation classes
inline def Handler.flat[F[+_], G[+_]](using TypeableK[F], Handler[F], Handler[G]): Handler[F + G]

// 2. an effect's contribution to a fused loop: what it does to ONE
//    operation given the accumulated state it owns. Tail-resumptive
//    by construction (it answers X and a new Acc); abort and
//    multi-shot are NOT Steps — see Design
trait Step[F[+_], Acc]:
  def step[X](e: F[X], acc: Acc): (Acc, X)

// 3. the composite: a row's steps fused into one loop over a PRODUCT
//    state, immutable, threaded through the loop — never a cell
inline def Fused.run[F[+_], G[+_], AccF, AccG, A]
    (accF: AccF, accG: AccG)(p: A ! (F + G))
    (using TypeableK[F], Step[F, AccF], Step[G, AccG]): ((AccF, AccG), A)
// and the k-ary shape by nesting rows: Fused.run over F + (G + H)
// composes Step[G + H, (AccG, AccH)] inline, so the product state
// nests the way the row does — its layout is the row's layout

// 4. instances for the effects that are steps today, written ONCE
//    each, replacing nothing: Step[State % S, S], Step[Writer % W, Vector[W]]
//    (Fold-generic: Step[Writer % W, Acc] given Fold[W, Acc]),
//    Step[Reader % R, R] (acc is the environment; local is a Step too,
//    it answers a NEW environment for the continuation — see Design)
```

Abort (`Throws`) and multi-shot (`Choice`) keep `Effects.handle`; a
fused loop over a row that contains them handles the resumptive members
as steps and falls back to a shift only at an abort/choose node — the
`Design` says how, and the `Behavior` items pin that the fallback
agrees with the nested meaning.

## Behavior

**Read the Results first.** Stage 0's gate was NOT cleared (1.13–1.29x
against a 1.3x bar) and stages 1–2 as designed DO NOT START, so the
boxes marked GATED OFF below describe machinery that is deliberately
absent — they are not work waiting to be picked up. What IS built and
proven: the hand-written `Fused` loops of stage 0, and stages A and B
of the reordered arc (see "After stage 0").

- [ ] (GATED OFF, stage 2 — `Handler.flat` is not built; see Results)
      `Handler.flat` agrees with `Handler.union` on every operation of
      a four-effect row (the agent's `Model + (Tool + (Context + Async))`
      shape), for all four positions.
- [x] `Fused.run(s, Vector())(p)` over `State % S + Writer % W` agrees
      with `State.handle(s)(Writer.run(p))` AND with
      `Writer.run(State.handle(s)(p))` (both orders reachable, each
      by naming the row in that order), on programs generated to
      interleave get/set/tell arbitrarily (scalacheck, ≥ 1000 cases).
- [x] A fused loop is stack-safe on any bind shape: 1M operations,
      left- and right-nested, no StackOverflowError (the `runFree`
      bar).
- [ ] (GATED OFF, stages 1–2 — the generic fused run is not built; the
      hand-written `Fused` covers State+Writer and Throws+State+Writer only)
      Multi-shot survives fusion: a row `Choice + State % S + Writer % W`
      run fused equals the nested run for BOTH orders of State and
      Choice — global state and backtracking state — on generated
      programs; the fused product state is immutable (the residual
      program after a forwarded operation can be run twice with the
      same answer; the law State.handle already keeps).
- [x] Abort survives fusion: `Throws % E` in a fused row aborts with
      the same value and the same log/state visibility as the nested
      run, for both nestings of Throws relative to Writer.
- [ ] (GATED OFF, stage 1 — the generic `Step`/`Fused.run` is not built)
      The fused product state has the row's layout: `((accF, accG), a)`
      for `F + G`, nested for a nested row — asserted, so that the
      layout is a documented contract and not an artifact.
- [x] Every existing suite stays green; no existing runner changes
      behavior (this spec ADDS a road, it does not move the old one) —
      held through stage 0, stage A (split-without-either, which DID
      touch every hot runner) and stage B, each landed on a green full
      matrix; checked here 2026-09-09 (spec-truth) because it is a
      claim about what was built, not about the gated stages.
- [x] MEASURED before any of the above is built (stage 0, the gate):
      a hand-written fused loop for `State + Writer` on the
      RowLift-style program (N = 1000) is ≥ 1.3x faster than
      `State.handle(Writer.run(p))` in µs/op, and the B/op difference
      names the saved Bind+closure per forwarded operation. If the
      hand-written ceiling does not clear 1.3x, this spec's Results
      record the refutation and stages 1–2 do not start.
- [ ] (GATED OFF, stages 1–2 — nothing to measure until they start)
      MEASURED after: the `inline`-composed `Fused.run` is within 10%
      of the hand-written loop (staging did not leave the win on the
      table), and the three-effect row (`+ Throws`) gains more than the
      two-effect one (the win grows with k, as the cost model says).
- [ ] (GATED OFF, stage 2 — `Handler.flat` is not built)
      MEASURED: `Handler.flat` on the four-effect agent row is not
      slower than `Handler.union` at any position, and faster at the
      last (the position that pays four tests today).

## Out of scope

- Run-time staging (`scala.quoted.staging`, okay-staging): the
  composite's shape is static at every call site; a JVM-only,
  single-threaded, generation-per-composite tool buys nothing here.
- Run-time closure composition as a "generic `!>` union": the road
  `specs/staged-effects.md` refuted. If a generic combinator is ever
  wanted for ergonomics, it must be measured against the inline form
  and shipped only as a convenience with the cost stated.
- Commutative composition, or any reordering of handlers by the
  library. The row names the order; the user chose it.
- Changing `State.handle`, `Writer.fold`, `Throws.run`, `Choice.run`,
  `Reader.run` or `relay`. They stay as the one-effect-at-a-time
  road, which remains right for a row with ONE continuation-aware
  effect (already one pass) and for stepping/inspecting a program
  between layers.
- Scoped operations carrying computations (`Fork(prog)`, `OrElse(a, b)`)
  — theory/05 rules the scoped hazard out by kind; fusion does not
  change that and this spec does not touch those nodes.
- Cross-platform claims beyond "compiles and agrees": the fused loop
  is plain Scala over `Free`, so JS and Native get it for free; only
  the JVM is measured.

## Design

**Where the cost is, precisely.** Take `Throws.run(State.handle(s)(Writer.run(p)))`
over a program with n operations. `Writer.run` walks p once, consumes
the tells, and for each `State`/`Throws` operation emits
`Effect(e).flatMap(x => _loop(acc)(k(x)))` — one Bind node, one closure
capturing the accumulator. `State.handle` walks THAT program: consumes
get/set, re-emits every `Throws` operation the same way. `Throws.run`
walks the result. So a Throws operation was allocated three times and
rotated by `resume` three times; a State operation twice. Per pass,
every operation not owned costs one Bind (16–32 B) and one closure, and
every pass re-runs the left-nested rotation. The fused loop allocates
nothing per operation that it owns as a step: it is `runFree` with a
richer handler and a product accumulator.

**The composite is a loop, not a value.** The composite's form is the
one `runFree`, `State.handle`, `Writer.fold` and `Stm`'s runner all
already have — a `@tailrec` match over `Pure` / `Effect` / `Bind(Effect, k)`
with the operation's meaning inlined. What changes is that the match
arm is the row's flat dispatch and the accumulator is a product. It is
written as `inline def Fused.run`, and `Step` instances are `inline`
too where their step is a few instructions (State's get/set, Writer's
append), so the composed loop is ONE static expression per call site —
the shape staged-effects.md measured at 1.9x, now applied to a row of
handlers rather than a chain of binds.

**Tail-resumptive members are steps; the others are shifts.** A `Step`
answers `(Acc, X)` — it resumes exactly once, in place, which keeps
the loop tail-recursive and captures no continuation. That is
evidence passing's tail-resumptive fast path (Xie & Leijen 2021), and
it covers `State`, `Writer`, `Reader` (including `local`: the step
answers a new environment that the loop threads to the continuation
and restores after — the product state carries a stack of
environments for nested locals, exactly what `Reader.local` does with
its relay today). `Throws.abort` and `Choose` are NOT steps: an abort
discards the continuation, a choose runs it more than once. In a row
with them the fused loop handles every step-member in the tail loop
and, at an abort/choose node, does what `Effects.handle` does — a
`shift` over the answer type, with the product state captured
immutably in the closure. The nested run's meaning for that node is
"the outer handlers see the inner handlers' results so far", and the
product state IS those results so far, so the fallback agrees by
construction; the Behavior items make the claim testable rather than
trusted.

**Immutability is not a style choice.** A staged loop's natural
optimization is a mutable cell per accumulator — and it is wrong here,
for the reason `State.handle` already states: a forwarded or captured
continuation must be re-runnable (multi-shot, `Choice`), so the state
it closed over must be the state at capture time. The product state
is a value threaded through the loop; the loop allocates a new tuple
only when a component changes. This is also what makes the "run the
residual twice" law hold, and that law is in Behavior.

**Order is the row's order.** `Fused.run` over `F + G` handles F's
operations "inside" G's, i.e. as `G.run(F.run(p))` would — the row
`State % S + Writer % W` fuses to `Writer.run(State.handle(s)(p))`'s
meaning, `Writer % W + State % S` to the other. The product state's
layout follows the row, which is why the layout is asserted rather
than left to whoever reads the tuple. A user who wants the other
meaning names the other row; nothing reorders.

**Flat dispatch for comonadic rows** is the small win and the first
thing to build, because it stands alone: `Handler.union` is a left-
nested chain of `<|>` tests, so the k-th effect's operations pay k
`TypeableK` checks. Assembled `inline`, the chain unrolls into one
`match` whose cases are the row's operation classes in order; the JIT
sees one type switch. The agent row is the measuring case because it
is the row that motivated `Handler.union` in the first place.

**What `Eff` gets.** `Eff`'s program is its `foldCont`; a composite
`!>` built the same inline way (flat dispatch + product state carried
in the answer type, i.e. `S = Acc => (Acc, A)` for the step members)
runs it in one pass with no tree. That is stage 3, after the `Free`
loop has the numbers, because `Eff` was not stack-safe on left-nested
binds (it is since specs/eff-stack-safety.md) and the fused loop's first job is to keep `runFree`'s bar.

## Decisions

- **Assess against `specs/staged-effects.md` first** — chosen because
  that spec already tried "staging" as run-time closure composition
  and refuted it 3/3; this spec must not re-run that experiment under
  a new name. Rejected: a generic `!>` union built from closures
  (measured slower than the tail loop it would replace).
- **Compile-time `inline`, not `scala.quoted.staging`** — chosen
  because the row and the handler identities are static at every call
  site (givens resolve there), so partial evaluation by `inline` is
  the whole of the staging needed; run-time staging is JVM-only,
  single-threaded and pays generation per composite for nothing.
  Rejected: okay-staging's road (right for codecs over RUN-TIME schema
  values, wrong for a compile-time row).
- **`Step` is tail-resumptive by type** — chosen because a step that
  answers `(Acc, X)` cannot abort or resume twice, so the fused loop's
  tail recursion is guaranteed by the interface rather than by a rule
  the handler author must remember (the same move `relay` makes with
  answer polymorphism). Rejected: one `!>`-shaped step for everything
  (loses the tail loop for every member, not just the non-resumptive
  ones).
- **Product state, immutable, row-shaped** — chosen for multi-shot
  correctness (State.handle's own argument) and so the result's layout
  is a contract. Rejected: a mutable cell per accumulator (breaks
  re-running a captured continuation; only safe when no multi-shot
  member is in the row, which the type cannot yet say).
- **A hand-written fused loop is the gate, before any generic code** —
  chosen because one measurement is a hypothesis and the win's size is
  a cost-model prediction until the box says so; the hand-written
  loop is also the ceiling the inline version is held to. Rejected:
  building `Fused.run` first and measuring after (the shape of the
  loop would be argued from, not measured against).
- **The one-at-a-time road stays** — chosen because it is already one
  pass for a single continuation-aware effect, and because stepping a
  program between layers (relay in stages, inspection) is a feature
  Free exists for. Fusion is an added road, not a replacement.

## Results

**Stage 0, measured 2026-09-09 (handler-fusion-gate, b19539e0): the gate
is NOT cleared.** `Fused.scala` (hand-written loops for `State + Writer`
and `Throws + State + Writer`), `TestFused` (agreement with BOTH
nestings on generated programs, aborts included, 1M ops stack-safe,
the residual re-runnable — 4/4), `FusionBenchmark` (JMH, N = 1000,
2 forks × 5 iterations, plus `-prof gc`). The box was not quiet
(Chrome at 70% CPU, load 4–7), so the numbers are per-fork MINIMA over
two rounds, the rule bench-refresh wrote down; B/op is load-proof.

| lane (µs/op, min) | nested | fused | ratio | B/op nested | B/op fused | saved |
|---|---|---|---|---|---|---|
| State + Writer, foldLeft (`nestedSW` / `fusedSW`) | 35.0 | 28.3 | **1.24x** | 364 953 | 332 896 | 32 057 |
| the other nesting (`nestedWS` / same fused) | 33.9 | 28.3 | **1.20x** | 354 241 | 332 896 | 21 345 |
| State + Writer, RIGHT-nested twin (`nestedSWr` / `fusedSWr`) | 18.8 | 16.7 | **1.13x** | 181 369 | 149 312 | 32 056 |
| Throws + State + Writer, no raise (`nestedTSW` / `fusedTSW`) | 38.6 | 29.9 | **1.29x** | 365 082 | 322 273 | 42 809 |

Two effects: 1.13–1.24x, under the 1.3x bar in both bind shapes. Three
effects: 1.29x by minima (the means said 1.39x, from a nested lane
whose fork 2 ran 60–102 µs under load — not a number). Per the gate,
stages 1–2 as designed DO NOT START.

**What the numbers say, and it is not what the cost model said.**

- The rebuild is exactly what was predicted, and it is small. Fused
  saves 32 056 B/op in BOTH bind shapes — 667 forwarded State
  operations × 48 B = one Bind (24) + one closure (24) each, to the
  byte. But that is 9% of the nested walk's allocation (365 KB) and
  6.7 µs of its 35 (left-nested), 2.1 µs of 18.8 (right-nested).
- Rotation was not the eater either. The right-nested twin has no
  rotation and the saving is the same 32 KB; the ratio is SMALLER
  (1.13x) because the walk is cheaper and the constant saving buys
  less of it. The refutation mechanism the spec named ("one pass
  rotates as much as three") is refuted itself.
- The premise was wrong: "each foreign operation rebuilt once per
  pass" is true only of the LAST-handled effect's operations. An
  inner handler consumes its own operations and emits a RESIDUAL of
  the rest, so pass k walks fewer nodes than pass k−1 — total node
  visits for two handlers over 1000 ops are ~1 667, not 2 000, and the
  third pass over a program that never raises walks 667 nodes owning
  none of them for ~11 KB and ~2 µs. The nested runners were never
  paying 2–3x; they were paying 1.1–1.3x, and that is the whole prize.
- Where the time actually is: INSIDE one pass, fused or not — 149 KB
  and 16.7 µs for 1000 operations with nothing to rebuild. Per
  operation: the node visit, the `<|>` split (an `Either` allocated
  per operation, ~20 KB of the 149), the continuation call, and
  `Vector :+` per tell. Fusing passes leaves every one of those in
  place.

**The corrected claim, for whoever picks this up.** Pass fusion is
worth 10–30% and no more on this library's runners, because the
runners already shrink the program as they go. The lever the
measurement points at is the per-operation cost of a single pass,
which every runner pays and fusion does not touch: a row split that
allocates no `Either` (a flat class match — stage 1's dispatch idea,
but aimed at `<|>` itself, for nested and fused alike) is the next
thing to price, and it is a different spec (`split-without-either`,
BACKLOG). The `Fused` loops stay in the tree as the measured ceiling
and the agreement laws stay green; nothing generic is built on them.

## After stage 0: the arc, reordered by the measurement (2026-09-09)

Stage 0 put the cost inside ONE pass, not between passes, and two
thirds of that inside the `Free` encoding itself: a continuation is a
function, so every `k(x)` BUILDS the next node (Inject + Bind + closure)
— the program is re-materialised on every run. No handler composition
removes that. So the order changes; the numbers, not the plan, decide:

- **Stage A — `split-without-either`** (next, small, every runner):
  `<|>` answers an `Either` per operation, and `TypeableK.unapply`
  answers an `Option` per test — two wrappers on the hottest path of
  every runner, fused or not (~20 KB of the 149 KB a fused pass
  allocates for 1 000 ops). Replace the split with an `inline` form
  whose two continuations beta-reduce into the caller — no closure,
  no wrapper — keeping the ONE cast where it is today (the excluded
  middle of the union, trusted kernel, `Effects.scala`), and the type
  test a plain class test with no `Option`. GADT refinement inside
  the F branch must survive (matching `Get()`/`Say(v)` refines the
  answer type; that is what keeps the runners cast-free).
  - [x] the HOT loops split with `split` — `State.handle`,
        `Writer.foldWith`, `relay`, `Effects.handle`, `Handler.union`
        (every `runWith` over a row) — and answer identically on the
        existing suites. The WALKS (`Writer.map`/`widen`, Pipe's
        transducers, the 50-odd other `<|>` sites) stay on `<|>`, which
        loses its Option for them with no churn; converting a walk
        without a lane that measures it would be a rewrite on faith.
  - [x] MEASURED on `Fused.stateWriter` right-nested first (B/op known
        to the byte, 149 312): expected −16…24 KB/op and ≥ 10% time;
        then `nestedSW` and `relayForward` (HandlerBenchmark) to see
        the same saving land in the shipping runners.
  - [x] no cast outside the kernel: `<|>` and `Split.apply` are the
        two functions that cast on a row, both licensed by the one
        `TypeableK.test`; no runner casts. (The count in Effects.scala
        grows by two — the extractor's implicit `x.type & F[A]` made
        explicit — and that is stated here rather than hidden.)
- **Stage B — `handler-fusion-eff`** (the main line): the composite
  `!>` for a row over `Eff`, assembled `inline`, the product
  accumulator in the answer type (`Acc => (Acc, A)`); the program is a
  function of the handler, the handler is one static expression, and
  no tree exists between them. Bar **1.5x** over `Fused.stateWriter`'s
  right-nested 16.7 µs (staged-effects.md measured 1.6–1.9x for this
  shape). Laws as stage 0: agreement with the nested `Free` runners for
  both orders, aborts included. Stated limit, in the spec before the
  code: `Eff` was not stack-safe on left-nested binds (fixed later the
  same day, specs/eff-stack-safety.md), so this was the
  road for for-comprehension-shaped programs; `foldLeft`-built ones
  stay on `Free`.
- Stages 1–2 (`Step`/`Fused.run` over `Free`, `Handler.flat`) stay
  GATED OFF with stage 0's numbers.
- Follow-up, its own spec after B has a number: `direct` blocks emit
  `Free` binds today; targeting `Eff` would give direct-style programs
  the fused run for free.

### Stage A — measured, 2026-09-09 (split-without-either)

Built: `TypeableK.test` (a boolean beside `unapply`; `typeableK`,
`Effect.of`, Pure and Writer's own instance answer it without an
Option), `split[F, G](e)(onF)(onG)` (a value class carrying the test,
`inline apply`, both branches beta-reduced; the two casts on a row now
live in `<|>` and `Split.apply` and nowhere else), `<|>` itself on
`test` (so every one of its 50-odd walk sites loses the Option with no
churn), and the hot loops on `split`: `State.handle`, `Writer.foldWith`,
`relay`, `Effects.handle`, `Handler.union`, `Fused.*`.

The box was never quiet (load 20–80, four sibling gates and docker);
numbers are minima across rounds, B/op from `-prof gc` is load-proof:

| lane | B/op before | after | µs before | after | ratio |
|---|---|---|---|---|---|
| fused State+Writer, right-nested | 149 312 | 122 641 | 16.7 (stage 0) / 15.4 (`<|>`, same tree) | 13.8 | 1.21x / 1.11x |
| fused, left-nested | 332 896 | 306 225 | 27.5 | 25.5 | 1.08x |
| nested `State.run(Writer.run)` right-nested | 181 369 | 170 696 | 18.8 | 17.4 | 1.08x |
| nested, the other order | 354 241 | 327 569 | 33.9 | 31.7 | 1.07x |

−26.7 KB/op is the Either (16 B) plus the extractor's Some (16 B) per
operation — the prediction (−16…24 KB) was under. (The remark that
`scala.reflect.Typeable`'s Some for the told value still costs was
measured afterwards and is wrong: 0 B/op, the JIT scalarises it, and
the fused loop never tests Writer at all — writer-test-no-some.) Time: 7–11% on the hot loops, which clears the 10% bar on the
lane it was set on and misses it by two points on the left-nested one.
One observation was left open here and is now closed — see
"either-scalarised" below: the Either escaped only in `State.handle`'s
loop; `Writer.foldWith`'s wrappers had always been scalarised.

Verdict: stage A holds as a small, uniform, zero-risk gain and lands;
it is not the lever. Stage B is.

### Stage B — measured, 2026-09-09 (handler-fusion-eff): REFUTED, and the arc closes

Built: `Fused.stateWriterInterp[C]` — the composite `!>` for
`State % S + Writer % W` at any Control carrier, assembled inline, the
accumulator threaded through the answer type (`Acc => (Acc, A)`, the
PState trick) — with `runEff` (an `Eff` program run once with it, no
tree) and `runCtrl` (a program written directly against a carrier, at
Func or Cont). Laws green: the `Eff` road agrees with the fused Free
loop on generated programs (`fromFree[Eff]` of the same tree), the
carrier road agrees at Cont and Func.

The same 1 000-op right-nested program, four encodings, minima over
3 forks (load 22–37), B/op load-proof:

| encoding | µs | B/op | vs fused Free |
|---|---|---|---|
| Free tree, fused loop (`stateWriter`, stage A) | 13.7 | 122 641 | 1.00x |
| handler-passing over Func (no tree, runtime recursion) | 16.0 | 184 665 | 0.86x |
| handler-passing over Cont | 18.0 | 200 673 | 0.76x |
| `Eff` + the composite (no tree) | 23.5 | 297 897 | **0.58x** |

The bar was 1.5x; the best tree-free road is 0.86x and `Eff` is 0.58x
with 2.4x the allocation. The premise — "two thirds of a pass is the
Free encoding re-materialising the program, so drop the tree" — is
refuted by the byte counts: a Free node (Inject 16 + Bind 24 + one
closure 24) is CHEAPER than the two closures a CPS bind allocates
(`k => m(f(_)(k))`), and the tail-recursive walk over data beats
closure invocation. What `staged-effects.md` measured at 1.9x was
compile-time unrolling of 24 STATIC operations — real, and it does
not transfer to a program shaped as a loop or a recursion, which is
every program of the size that matters.

**The arc closes here**, with what it landed and what it learned:
stage 0 (pass fusion 1.13–1.29x, gated off) and stage A (the split
without wrappers, −18% bytes, 7–11%) are in the tree; the fused Free
loop at 13.7 ns per operation is the floor this design has, and it is
a good floor. The composite-handler idea was right about the
comonadic class (already one pass) and worth 10–30% on the
continuation-aware class; staging it buys nothing unless the program
is static at the call site. The `direct → Eff` follow-up is dropped
for the same reason and is not filed.

### either-scalarised — resolved, 2026-09-09 (runner-floor item 3)

Per-runner lanes (`SplitBenchmark`), each runner alone over 1 000 of
its own operations, three forms of its loop, `-prof gc`:

| runner | shipping (`split`) | `<|>` (Either) | extractor + Either |
|---|---|---|---|
| `State.handle` | 86 984 | 100 904 | 100 936 |
| `Writer.foldWith` | 197 544 | 197 480 | 197 496 |

So: in `State.handle`'s loop the Either ESCAPED (14 B/op) and `split`
removed it; the extractor's Option never escaped in either loop; and
in `Writer.foldWith`'s loop NOTHING escaped — `split` bought Writer
zero bytes. That is the whole "anomaly": `State.run(Writer.run(p))`
saved State's Eithers over 667 forwarded ops (10.7 KB, to the byte)
and nothing from Writer. Why C2 scalarised one loop's Either and not
the other's is not chased: the byte counts settle what `split` is for.

What the lanes found instead: `Writer.run`'s cost was the `Vector`
appended per tell. A `List` built by prepending and reversed ONCE —
inside the loop's terminal case (`loopWith`'s `finish`), NOT as a
`.map` over the residual — measured, same run:

| | old (Vector) | List, `.map` after | List, finish in the loop |
|---|---|---|---|
| Writer-only, 1 000 tells | 197 544 | 128 104 | **128 024 (−35%)** |
| mixed, 333 tells + 667 State ops | 170 696 | 196 800 (+15%) | **148 696 (−13%)** |

The middle column is the lesson worth more than the win: one `.map`
wrapped around a program that still forwards effects made every
forwarded node left-nested under it, and `resume` rotated each of them
again — 61 KB over 667 operations, more than the accumulator it was
finishing. `loopWith` exists so that a finishing step happens where
the program ends and nowhere else.

