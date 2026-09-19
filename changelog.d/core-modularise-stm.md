## core-modularise-stm - three of the five "STM" things were named STM and were not

Stage 5 of specs/core-modules.md, the last seam it had filed, and the
spec was wrong about what the seam was.

THE BLOCKER DID NOT EXIST. The spec said the lane was blocked because
"`Providing.Facts` is backed by `TMap`". `TMap` is a heterogeneous map
with TYPED keys - the T is *typed*, not *transactional* - and `Facts`
uses `get`, `updated`, `foreach` and `isEmpty`. Checking that turned up
two more of the same kind:

    TMap, TDict      heterogeneous maps with typed keys
    Refs             run-time state cells, the pair to Keyed;
                     zero mentions of TRef, Tx or Stm
    okay.sql.Tx      a typestate marker for DATABASE transactions

Three coincidences in one cluster of five. Stage 4 had just written
down that a type is not a dependency until you check what is used of
it; this is the same lesson from the other end - a NAME is not a
dependency either.

THE REAL EDGE WAS IN A PLATFORM DIRECTORY, the miss stage 1 already
paid for once with `Parallel.scala`: Scala Native's `Platform.scala`
holds its scheduler state in a `TRef[State]`, on purpose, with a
comment saying why.

AND IT DECIDED THE CUT. `TRef.modify` is, in its own doc's words, "the
one-cell transaction": a CAS loop that retries a pure function until it
installs, wakes watchers and never parks. It needs no `Tx` and no
runtime. So `TRef` is the interface and STAYED (`TRef.scala`, 127
lines); what left is the machinery that commits SEVERAL cells together
- the `Tx` language, the TL2, direct and simulated runtimes, and the
two platform files that install `given Stm[Async]`.

THE COMPILER THEN PRICED THE SPLIT, and the number is the point. Of
the eight modules that name something STM-ish:

    TRef, the cell                    6 modules
    the transactional runtime         1, and only in a test

The one is okay-stream's `TestStmChannel`, a single
`Stm[Async].atomically` showing a channel's cell can be read inside a
transaction; okay-stream's own MAIN code needed nothing. okay-http,
okay-ops, okay-resilience, okay-sql and okay-ui needed nothing. So the
whole 73-module family took ONE `dependsOn`, scoped `test->compile`.
A memory written weeks ago had said the consumers were "all single-cell
TRef.modify"; this is that claim checked by a compiler.

The core is now 46 files and 11 229 lines, from 74 and 21 914 before
stage 1: 49% gone across five stages, and no consumer's import or call
site was edited in any of them.

Spec: specs/core-modules.md.
