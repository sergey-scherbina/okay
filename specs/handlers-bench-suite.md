# handlers-bench-suite — one shape of Kammar-Lindley-Oury's suite, landed; the rest, named

## Overview

Kammar, Lindley & Oury, "Handlers in action" (ICFP 2013) introduced a
small family of programs — countdown, fibonacci_recursive,
product_early, iterator/generator, nqueens, tree_explore, triples,
parsing_dollars, resume_nontail, handler_sieve — that Koka, Effekt and
OCaml 5's own papers still report numbers on, because each isolates
ONE axis of what an effect handler runtime costs (tail resumption,
multi-shot capture, handler-stack depth, non-tail resumption). None
of the shapes existed in `compare/` before this lane; two are already
covered under other names, and this lane adds the first NEW one
rather than all nine at once.

- **`ChoiceBenchmark`** (existing) is the search/multi-shot family's
  shape — a binary choice nested D deep, every branch collected — the
  same axis `nqueens`/`triples` isolate.
- **`GeneratorBenchmark`** (existing) is the generator family's shape
  — the Nth Fibonacci number unfolded one element at a time — the
  same axis `fibonacci_recursive`/`iterator` isolate.
- **`CountdownBenchmark`** (this lane) is the suite's OWN first,
  simplest lane: N handled state operations, tail-resumptive
  throughout — every runtime's cheapest possible shape, and the one
  every one of the suite's descendants opens with.

## Interface

`compare/src/jmh/scala/okay/CountdownBenchmark.scala`: `stdLoop` (the
hand ceiling — lane rule 1), `okayCountdown` (`State.handle`),
`kyoCountdown` (`kyo.Var`), `catsState` (`cats.data.State`), `zioRef`
(`zio.Ref`, ZIO having no first-class State effect — the same
`Ref`-based idiom `IdiomaticApiBenchmark` already uses for ZIO
elsewhere in this file).

## Behavior

- [x] `okayCountdown`/`kyoCountdown`/`catsState`/`zioRef` all answer
      `N` for `N` increments from 0 — correctness, not just a number
- [x] `stdLoop` is the hand ceiling every other lane is priced against
      (lane rule 1); it reads near the JIT's noise floor, which is
      the RIGHT answer for a counting loop with no externally
      observable per-iteration effect (a fact about the shape, not a
      benchmarking defect — see Results)
- [x] `compare/Jmh/compile` clean, no warnings, after `rm -rf compare/
      target` (jmh-generate-and-run-are-two-invocations's own rule)

## Out of scope

Five shapes, each with why THIS pass did not build it:

- **`handler_sieve`** (a handler STACK N deep, forwarding through
  every layer) — the closest existing measurement is `handle-decompose`
  (docs/benchmarks.md §2, `Effects.handle`'s own forwarding cost per
  operation), but that prices ONE handler forwarding one op, not N
  STACKED handlers each forwarding to the next; a faithful lane needs
  its own construction. Trigger: a consumer stacking more than two or
  three handlers in practice (most of this codebase's own handlers
  compose at most two deep today).
- **`resume_nontail`** (a handler that resumes NOT in tail position)
  — this library's own axis for the SAME question is `!.relay`
  (tail-resumptive, the fast path) against `Effects.handle` (general,
  can resume anywhere, capture, abort, multi-shot); `handle-decompose`
  already prices that boundary directly and by a different, more
  precise construction than translating the suite's own C-like
  "resume in the middle of an expression" idiom would give. Revisit
  if a reader specifically wants the SUITE's own construction rather
  than this library's equivalent one.
- **`tree_explore`, `triples`, `parsing_dollars`** — each a variation
  on the search/multi-shot family `ChoiceBenchmark` already occupies;
  building all three now would be three more instances of one
  already-measured axis. Trigger: a consumer whose search shape is
  NOT captured by `ChoiceBenchmark`'s binary-choice tree (`triples`'s
  three-way search over a shared bound, in particular, prunes
  differently and might read differently — untested, not assumed).
- **A comparison table against Koka/Effekt/OCaml 5's OWN published
  numbers.** Their papers report wall-clock numbers on THEIR OWN
  machines, and this repo's own lane rules (rule 4: "a lane whose
  capacity comes from `availableProcessors` breaks this rule too —
  its number is not comparable with the same lane on another
  machine") would refuse exactly this kind of cross-machine number if
  a competitor lane tried to quote it. The citations are in Overview;
  no number from them is quoted here or would be trustworthy if it
  were.

## Decisions

- **The hand ceiling constant-folding to near-zero is the CORRECT
  measurement, not a broken lane.** `stdLoop` computes `N` by
  incrementing a local `N` times with no side effect visible outside
  the method; an optimizing JIT can prove the whole loop reduces to
  returning `N` and does. Every effectful lane CANNOT be folded this
  way — `State.handle`'s loop walks a `Free` tree, `kyo.Var`/`cats.
  State`/`zio.Ref` each go through their own dispatch — so the gap
  IS the effect machinery's price, exactly as the hand-ceiling
  convention (docs/benchmarks.md) intends. Padding the hand loop with
  a fake side effect just to give the JIT something to keep would
  measure a DIFFERENT, less honest baseline.

## Results

Smoke-tested only (`compare/Jmh/run -wi 1 -i 1 -f 1 -w 1s -r 1s
CountdownBenchmark`, N=100000, on a moderately loaded box — NOT the
quiet-box, multi-fork protocol `per-lane-gated-jmh` prescribes for a
number this document would treat as load-bearing): okay 1575 µs/op,
kyo 1781, zio 1302, cats 3364, std loop below the JIT's measurable
floor. All four effect runtimes land within one order of magnitude of
each other on this shape, consistent with "countdown" being every
runtime's cheapest lane in the literature it comes from. A rigorous,
gated re-measurement is the natural next step if this number is ever
quoted for a decision rather than illustration.
