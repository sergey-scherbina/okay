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

- [ ] measured FIRST on the current code, then after, alternating on the
      same box (PRICE lines in the suites, min of runs): a gatherer over
      1e6 elements, a transducer over 1e6, a Frege step, a Clojure
      program step — no pipeline slower beyond the noise of its own runs
- [ ] `okay.Member[F]`: found for one signature, built with `|` for a
      union; both bridges use it; `Frege.Row`/`Program.Row` gone
- [ ] `okay.Operations`: ask/get/set/raise/choose/sleep, once; the
      language `Ops` delegate
- [ ] `okay.Push`: the push driver of a stage; `Gather` and `Transducers`
      both on it (the transducer's accumulator threaded by its `emit`)
- [ ] `okay.Foreign`: a program-as-data walker over a `View` (done / the
      step's operation / resume); Frege and Clojure each supply a view
- [ ] every existing suite of the three modules green unchanged; the
      mutants of their lanes still fail (the tests did not move)

## Decisions

## Results
