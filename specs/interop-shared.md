# interop-shared — one copy of what the JVM-language bridges share

## Overview

Three bridges grew side by side on 2026-09-23 — okay-java's `Gather` (JDK
gatherers), okay-clojure's `Transducers`/`Program`/`Ops`, okay-frege's
`Frege`/`Ops` — and each carries its own copy of the same four pieces:

| piece | copies |
|---|---|
| whether an `Object` is an operation of a row F (`Row`) | `Frege.Row`, `Program.Row` |
| the core effects' operations for a foreign language (`Ops`) | `okay.frege.Ops`, `okay.clojure.Ops` |
| a `Stage` driven by PUSHING its tells (`Pos`, `drive`) | `Gather.drive`, `Transducers.drive` |
| a foreign program-as-data walked as a stage or a program | `Frege.stageWith/run`, `Program.stageWith/run` |

One copy of each, placed where every bridge already depends: `Member[F]`
in the core beside `TypeableK`; `Operations` in okay-platform (it needs
the Timer); `Push` and `Foreign` in okay-stream beside `Stage`. The
per-language classes stay as the names users bind to (Frege natives name
`okay.frege.Ops`), as thin facades.

## Behavior

- [x] measured FIRST on the current code, then after, alternating on the
      same box (PRICE lines in the suites, min of runs): a gatherer over
      1e6 elements, a transducer over 1e6, a Frege step, a Clojure
      program step — no pipeline slower beyond the noise of its own runs
- [x] `okay.Member[F]`: found for one signature, built with `|` for a
      union; both bridges use it; `Frege.Row`/`Program.Row` stay as
      ALIASES of it (a type and a val), so every call site and doc
      snippet written against them compiles unchanged
- [x] `okay.Operations`: ask/get/set/raise/choose/sleep, once; the
      language `Ops` delegate
- [x] `okay.Push`: the push driver of a stage; `Gather` and `Transducers`
      both on it (the transducer's accumulator threaded by its `emit`)
- [x] `okay.Foreign`: a program-as-data walker over a `View` (done / the
      step's operation / resume); Frege and Clojure each supply a view
- [x] every existing suite of the three modules green unchanged; the
      mutants of their lanes still fail (the tests did not move)

## Decisions

- **A `View` of methods, not a `Step` object per node.** The walker asks
  `kind(p): Int`, then `payload`/`resume`: nothing is allocated to
  describe a node the language already represents. The cost is that a
  view reads a node's fields twice (Frege's `mem1` thunk, Clojure's
  record lookup) — cheap reads of already-forced values, and the price
  table below shows no loss.
- **Kinds are `inline val` Ints** (Done 0, Await 1, Tell 2, Perform 3,
  Lift 4) so the walker's match is a `tableswitch`, not a type test.
- **The per-language names stay.** Frege natives bind
  `okay.frege.Ops.set :: Long -> Operation Long`; Clojure calls
  `(okay.clojure.Ops/ask)`. Those are the public contract, so they
  became facades over `okay.Operations`, keeping their typed signatures
  (Frege's `Long` arguments, `choose2`).
- **`Foreign.obj` replaces three `boxed` casts** with a match
  (`case r: AnyRef => r`) that casts nothing: an erased value is already
  an Object at run time. `Member.operation` holds the one cast left in
  the bridges, commented once.
- **Placement:** `Member` in the core beside `TypeableK`; `Operations`
  in okay-platform's SHARED sources (Timer exists on all three
  platforms); `Push` and `Foreign` in okay-stream's shared sources — so
  both are cross-built, and a JS or TypeScript program-as-data can reuse
  `Foreign` on Scala.js (backlog: polyglot-typescript).

## Results

- Four alternating rounds on one box (load 7–18), min of 7 runs each,
  base = the lane's own PRICE-tests commit (the tests on the old code):

  | driver | base, ms | shared, ms |
  |---|---|---|
  | gatherer, 1e6 | 27.4–29.6 | 27.9–29.4 |
  | transducer, 1e6 | 29.4–30.8 | 26.7–29.8 |
  | Clojure program, 2e5 steps | 13.7–23.6 | 16.2–23.8 |
  | Frege program, 2e5 steps | 39.8–44.8 | 41.0–43.6 |

  Every range overlaps its base; no driver is slower beyond its own
  noise. The transducer reads slightly faster (its accumulator is now a
  field of the process, not a tuple per drive), within noise.
- 122 tests across okay-java, okay-clojure and okay-frege green,
  unchanged.
- Mutants on the SHARED code, each caught by a test of an earlier lane:
  `Push.drive` ignoring a refused push fails TestGather's "a refused
  push stops the stage mid-element" (1000 tells for a downstream that
  took 3) and TestTransducers' "(take 2) stops the stage mid-element";
  `Foreign.run` resuming with null instead of the answer fails three
  Frege tests (Reader/State from Frege, the multi-shot Choose, the doc
  example). A lesson in method, recorded because it hid a result for one
  run: `sbt "a/test; b/test"` stops at the first red, so a mutant must be
  run against each module alone.

## Follow-up: cancelling a lifted step (interop-lift-cancellation, 2026-09-23)

Filed as a question: does cancelling the fiber stop a lifted Frege IO
(`liftIO`) or a blocking Clojure step? Measured with an instrument that
has its own CONTROL. The lifted action sets a system property after its
sleep, so "the work stopped" is told apart from "the fiber was reported
finished", and the uncancelled run shows the full sleep and the mark.

- Loom (the default where there are virtual threads): a cancel interrupts
  the fiber's own virtual thread, and the lifted action STOPPED.
- `Schedulers.drive()` (pool-threaded): the fiber was reported finished
  at once, but the lifted action RAN ON to completion. A pool cannot
  interrupt a thread it shares. The first cut of the test asserted only
  "the fiber ends promptly" and passed for this broken case: the
  instrument measured the report, not the work.

The fix is in the walker, not the schedulers. `Foreign.View` gains
`liftAsOperation`, and where the program's row accepts it (a row with
`Async`, tested by `Member`), the lifted action becomes
`okay.Interruptible.await`: an `Async` operation that runs the action on
a thread of its own and whose canceller interrupts exactly that thread.
Without `Async` in the row nothing changes. Clojure gained the explicit
blocking step it lacked, `(ok/lift f)`, with the same treatment. Six
tests: control, Loom and pool, for each language.
