# Cont interpreter optimization

## Overview

The defunctionalized `Cont` (data nodes + tail-recursive `/`) buys stack safety at the
price of an interpretation layer: measured ~1.5x on the fib generator microbenchmarks
vs the function encoding at HEAD 80cc6ac, reduced to ~1.35x (estimated) by the memoized
stepper in `loop` (kept, -9..-10%). The remaining ~4-5 ns/element is per-element node
allocation (`Shift`+`Bind`+`Pure`) plus two `/` dispatch drives. This spec defines the
program to close most of that gap without giving up stack safety or the public API.
Evidence history: `src/jmh/history.tsv` (2026-08-29 rows).

## Interface

Nothing callers depend on may change:

- Public API and signatures stay as-is: `Cont` (`flatMap`, `map`, `apply`, `/`),
  `shift`/`reset`/`take`/`loop`, `Control`, `Effects`/`!>`/`foldCont`, `Put`,
  `generate`, `!.handle`, `Effects.handle`.
- Run semantics of `m / k` are unchanged for every `m` constructible today.
  Construction-time laziness is preserved (see Decisions: no Pure-fusion).
- Stack-safety contract is unchanged: flatMap chains never overflow; a shift body
  that eagerly re-enters its continuation still consumes JVM stack per nesting.
- `src/jmh/history.tsv` stays append-only; every experiment adds rows with sha,
  host load, medians, and a KEPT/REVERTED verdict.

## Behavior

- [ ] Step 0 profile recorded in Results: `-prof gc` B/op for fib10/50/100/1000,
      plus `-XX:+PrintInlining` notes for the `/` and `apply` call sites.
- [ ] Experiment A (fusion) implemented behind the same API; all existing tests pass,
      including the three 1M stress tests (LazyList via State.index, Producer.next,
      tail-resumptive handle with forwarding).
- [ ] New stress test: one leading `shift`, then 1M `flatMap(x => Pure(x + 1))`
      built by foldLeft — exercises fusion up to the budget, then the Bind spill;
      must run without stack overflow and produce the correct value.
- [ ] A/B protocol executed for every experiment: 3 alternating rounds, medians of
      both arms measured in the same session, per-run subshells (`(cd DIR && sbt ...)`).
- [ ] Keep/revert decided strictly by the thresholds in Design; verdict and medians
      appended to history.tsv; a reverted change leaves no code behind.
- [ ] Results section below filled in with before/after medians per workload.

Note on running tests: the suite is munit — plain `sbt test` discovers and runs
all of it; "0 tests run" means something is broken, not verified.

## Design

### Step 0 — attribute the remaining cost (allocation vs dispatch)

`Jmh/run -prof gc -wi 2 -w 1 -i 3 -r 1 -f 1 .*FibBenchmark.*` on the current tree;
record `gc.alloc.rate.norm` per benchmark. Cross-check against the node-count theory
(per element: ~2 enum nodes + ~3 closures ≈ 80-120 B/op expected from the Cont side).
Run once with `-jvmArgs -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining` and note
whether `/`, `apply`, and the composed continuations inline into the benchmark loop.
Decision rule: allocation-dominated → Experiment A first (it removes allocations);
dispatch/inlining-dominated → Experiment B moves up.

### Experiment A — closure fusion with a depth budget

`Shift(f)` already holds a function of exactly the meaning type `(A => S) => R`, so
`flatMap`/`map` can compose functions (the fast HEAD encoding) while the receiver is
a `Shift`, bounded by a fuse budget, and reify to `Bind` past it:

```scala
case Shift[A, S, R](f: (A => S) => R, depth: Int = 0) extends Cont[A, S, R]

val Fuse = 128

def flatMap[B, S2](f: A / Cont[B, S2, S]): Cont[B, S2, R] = this match
  case Shift(s, d) if d < Fuse => Shift(k => s(f(_)(k)), d + 1)
  case _ => Bind(this, f)

def map[B](f: A => B): Cont[B, S, R] = this match
  case Shift(s, d) if d < Fuse => Shift(k => s(x => k(f(x))), d + 1)
  case _ => flatMap(a => Pure(f(a)))
```

`map` needs its own fused case: without it the fused body still allocates `Pure(f(x))`
and drives `/` per element, keeping the slice this experiment targets.

Expected effect on the generator: the per-element tree collapses to a single fused
`Shift`; `put(v).map(_ => g(a))` allocates 1 node + 1 closure instead of
node+node+closure+Pure+extra drive; the outer `Bind(Shift(id), f)` fuses away at
construction, so `/` dispatches on a bare `Shift` (2 type tests). Dispatch-case count
in `/` is unchanged (3 outer shapes) — deliberately, see the Map-node refutation.

Stack-safety argument: one fused segment applies at most `Fuse` nested compositions,
~3 frames each (~400 frames, tens of KB); segments beyond the budget are `Bind` nodes
handled by the tail-recursive `/` exactly as today.

Prediction (written before measuring): -15..-25% on fib10/50/100 vs the current tree,
i.e. near parity with the HEAD encoding; smaller on fib1000 (BigInt-diluted).
Revert if: median gain < 5% on fib10/50/100, or any workload regresses >= 10%
consistently (3/3 rounds) — the Map-node experiment showed this codebase is
JIT-inlining-shape sensitive, so a mixed result is a revert, not a negotiation.

### Experiment B (conditional) — explicit-stack monomorphic runloop

Only if A is reverted or Step 0 shows dispatch/inlining dominance. Replace the
tailrec-match `/` with a single while-loop: an explicit `Array[AnyRef]` stack of
pending continuations (push on `Bind`, pop on `Pure`), int-tag `switch` dispatch
instead of instanceof chains. This is the cats-effect/ZIO runloop shape; it also
subsumes the type-aligned-queue idea (the array is its degenerate fast form).
Prediction: -20..-40% of the remaining gap, stable against inlining shape.
Price: ~60 imperative lines with casts replacing 5 declarative ones — accept only
with a clear measured win (same thresholds as A).

### Measurement protocol (all experiments)

Snapshot the pre-change tree (`cp -R` including target/) as the before-arm; edit the
main tree; 3 alternating rounds of `(cd ARM && sbt -batch 'Jmh/run -wi 2 -w 1 -i 3
-r 1 -f 1 .*FibBenchmark.*')`; verify each log's "in build file" line points at the
intended arm; compare medians; append both arms to history.tsv. Known quirks: JMH
lines carry the `[info]` prefix; the first `Jmh/run` after a clean may fail with
"No matching benchmarks" — rerun; a leading `cd` in a compound command leaks into
subsequent commands — always use per-run subshells.

## Decisions

- **Fusion lives in smart constructors, not new node types** — chosen because the
  Map-node experiment (2026-08-29, REVERTED) showed that widening `/` from 5 to 10
  cases produces JIT-shape-dependent results (fib100 +22% while fib10/fib1000 -16%,
  consistent 3/3, despite strictly less work per element). Rejected: dedicated
  `Map`/`Fused` cases (do not retry without perfasm evidence).
- **No Pure-fusion in flatMap** (`Pure(a).flatMap(f)` stays a `Bind`) — fusing it is
  legal by the left-identity law but moves user code from run time to construction
  time (a 1M left-fold would collapse before `/` is ever called). Preserving
  construction laziness keeps run semantics exactly as today. Revisit only with a
  stated need.
- **`Shift` gains a `depth` field rather than a sibling case** — keeps `/` at 3 outer
  shapes; the ~6 internal `Shift(f)` pattern sites are updated in-repo. Rejected:
  separate `Fused` case (adds dispatch cases — same Map-node lesson).
- **Type-aligned queue deferred** — no current benchmark exercises deep left-nested
  binds, so the change is unfalsifiable today; Experiment B's array stack covers the
  same ground if reached. Prerequisite if ever pursued: a reflection-without-remorse
  style benchmark first.
- **Budget constant 128** — bounds fused-segment stack to tens of KB against default
  JVM stacks; not tunable per call site until a workload demands it.

## Out of scope

- Chunked emission in Producer (changes the granularity semantics of `next`/`?` —
  a product decision, not an interpreter one).
- Staged/partially-evaluated tagless instance (macro-based `Control`/`Effects`
  implementation compiling programs to direct style) — the deepest win, but a
  separate feature deserving its own spec.
- Free-side runloop (`resume`/`next`) — untouched by the regression being addressed.
- Any public API or semantics change.

## Results

To be filled after each experiment: per-workload medians of both arms, verdicts,
and the Step 0 B/op table. Rows also land in `src/jmh/history.tsv`.
