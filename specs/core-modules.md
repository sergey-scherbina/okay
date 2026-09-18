# Core modules — what stays in the core and what becomes a module

## Overview

The core (`src/main/scala`) is **74 files, 21 914 lines**, and it is
the one project every other module depends on. The question asked
(operator, 2026-09-18) was whether streams and channels should leave
it, and then the general form: what else can leave, and what must
stay.

The answer is not a taste question, because the dependency graph
already contains it. It was read in both directions, with comments
STRIPPED rather than grepped around — the first pass over the same
files found "dependencies" that were all prose, which is exactly the
shape [[instrument-needs-its-own-control]] warns about: a believable
number about the wrong thing. The stripper is
`scratchpad/strip.py`-shaped (block comments, then `//` outside
string literals); every count below is from stripped source.

## The law the graph already contains

**An interface stays in the core; the machinery that implements it
leaves.**

This was not chosen. It fell out of the measurement three times
independently, at three unrelated seams:

1. The whole control layer — `Cont`, `Free`, `Effects`, `Monad`,
   `Delim`, `State`, `Direct`, `Par`, `Resource`, `Logic`, `Throws`,
   `Validated`, `Static` — mentions `Channel`, `Source`, `Chunks`,
   `Pipe`, `Queues` **zero times in code**. Every hit was a comment.
   The only two real edges are `Async` -> `Handoff` (63 lines, a
   rendezvous primitive) and `Writer` -> `Stream` (218 lines, the
   typeclass whose entire interface is `uncons`). Both are
   interfaces.
2. `Optic.aggregateWith` and `Bulk.aggregate` both reach for
   `Aggregator`, from opposite sides of the library. `Aggregator` is
   an interface; the aggregators built on it are not.
3. `Delim` reaches into the workflow cluster exactly once, for
   `Replayable` (78 lines, a marker), not for `Wf` (848).

So the cut line is never "this file is about streams". It is "is
this the thing callers are typed against, or the thing that answers".

## Measurements

Core today, and what each candidate cluster weighs:

| cluster | lines | share of core | blocked by |
| --- | --- | --- | --- |
| streams + channels + buffers + `Tables` | 6 136 | 28% | nothing |
| workflow (`Wf`, `Proc`, `ProcMacro`) | 1 979 | 9% | nothing |
| data (`Sketch`, `Tables`, `Windows`, `Uid`, `Hlc`) | 1 139 | 5% | `Tables` rides with streams first |
| optics (`Optic`, `Fuse`, `Focus`) | 1 244 | 6% | `State.zoom` is typed on `Lens`, `Proc.procArrow` on `Optic.Arrow` |
| STM (`Stm`, `TMap`, `TDict`, `Refs`) | 734 | 3% | `Providing.Facts` is a `TMap`; `Stm.sim` is typed on `Sim` |

Consumers: **29 of the repo's 73 modules** name something in the
stream cluster. That number is an upper bound from identifiers alone
(`Source`, `Buffer` and `Pipe` are words other libraries use too);
the build settles it, and the honest procedure is to move first and
add `dependsOn` only where the compiler asks.

## Stage 0 — the split-package probe (DONE)

Everything below rests on one assumption that was worth a compiler
rather than a belief: **may one package `okay` live in two
artifacts, with `given` instances on both sides?** If yes, no
consumer's import changes and the whole migration is a build file.

Probed on Scala **3.9.0**, the version this build uses, with three
separate compilations into three output directories:

- artifact A: `package okay` with `given Shape[Int]`, a top-level
  `def`, and an `extension`.
- artifact B: `package okay` again, compiled against A only,
  defining `given Shape[Boolean]` and its own top-level `def`.
- a consumer in `package user`, compiled against A and B, doing
  `import okay.*` and `import okay.given`.

RESULT: all three compile clean and the consumer runs, printing
`int-from-core`, `bool-from-stream`, both top-level defs, a class
from B and A's extension method. Given resolution, wildcard import
and `import okay.given` all cross the artifact boundary in both
directions.

WHAT THIS DOES NOT SAY: it was run on the JVM. Scala.js and Scala
Native linking is not exercised by the probe and is left to the
gate, which builds all three platforms.

## Stage 1 — `okay-stream` (this lane)

Moves, as `git mv`, from `src/main/scala` to `okay-stream/src/main/scala`:

`Channel` 1002, `Pipe` 584, `SentinelChannel` 502, `AdaptiveFifo`
502, `Source` 491, `Chunks` 423, `Queues` 336, `Growing` 335, `Ring`
311, `ChunkBuf` 290, `Tables` 248, `Buffer` 211, `Segments` 197,
`Fifo` 196, `AbruptChannel` 188, `Pipeline` 163, `Bulk` 157.
Plus `src/main/scala-jvm/BulkJvm.scala` (14).

`Tables` is in the list for one measured reason and not because it
is "about data": it is the only file outside the cluster that uses
`Chunks` and `Bulk` in code. It is the query planner over `Bulk`, so
it follows `Bulk`. Stage 3 may re-home it to `okay-data`; that is a
move between two modules and costs nothing in the core.

STAYS in the core, and this is the whole point of the stage:
`Stream.scala` (218, the `uncons` typeclass and its `LazyList`/`List`
instances) and `Handoff.scala` (63). Together 281 lines, so the core
keeps the interfaces `Writer` and `Async` are typed against and loses
5 855 lines of machinery.

The module is a `crossProject(JVMPlatform, JSPlatform,
NativePlatform)` with `CrossType.Pure`, mirroring the core's own
source layout including the JVM-only, cross and native test
directories — JS and Native **replace** `Test /
unmanagedSourceDirectories` rather than adding to it
([[shared-test-helper-is-cross]]), and the new module must replace
them the same way or its shared suite will be compiled on platforms
that cannot run it.

`ChannelBenchmark` moves to the new module's `src/jmh`, because the
core cannot depend on a module that depends on it.

## Stage 2 — `okay-workflow` (not this lane)

`Wf`, `Proc`, `ProcMacro`, 1 979 lines, already a leaf: no core file
uses them. `Replayable` stays, because `Delim` is typed on it.

## Stage 3 — `okay-data` (not this lane)

`Sketch`, `Windows`, `Uid`, `Hlc` and the aggregators, with
`Aggregator` itself staying in the core by the law above. `Tables`
re-homes here from `okay-stream`.

## Not yet — optics and STM

Both are extractable in principle and neither is extractable by
moving files, because in both the core is typed against the
implementation rather than an interface:

- `State.zoom` takes a `Lens`, and `Proc.procArrow` is an
  `Optic.Arrow`. The classes (`Strong`, `Choice`, `Arrow`,
  `Traversing`) are interfaces and belong in the core; the concrete
  optics and the `Fuse` macro are machinery. Separating them is a
  lane of its own.
- `Providing.Facts` is backed by `TMap`, and `Stm.sim` is an
  instance for `Sim`. Two seams, both small, both real.

Filed rather than done: a lane that cuts one of these seams is worth
more than the lines it moves, because it is the law being applied
where it is not yet obeyed.

## Behavior

- [ ] one package `okay` across two artifacts resolves `given`s both
      ways on 3.9.0 (stage 0, probe)
- [ ] the core no longer mentions `Channel`, `Source`, `Chunks`,
      `Pipe`, `Queues`, `Bulk` in code, on any platform
- [ ] `Stream` and `Handoff` stay in the core and `Writer`/`Async`
      still compile against them
- [ ] every module that used the cluster compiles with no import
      edited — only `dependsOn` added
- [ ] the moved suites run on JVM, and the cross suite on JS and
      Native, exactly as before the move
- [ ] `ChannelBenchmark` still generates and runs from its new home

## Decisions

**Why one module and not two ("okay-queue" + "okay-stream").** The
per-producer FIFO promise binds `AdaptiveFifo` to `Channel`'s drain —
that is the whole subject of the `growing-channel-order` work, where
the defect turned out to live in the drain and not in either buffer.
A module boundary drawn between them would run straight through an
invariant that is already hard to state. One module now; splitting
later costs a `git mv` and a `dependsOn`.

**Why the package name does not change.** Because stage 0 says it
does not have to, and a rename would touch every consumer's imports
for no gain. `okay-stream` is an artifact name, not a namespace.

**Why `dependsOn` is added by the compiler's demand, not by grep.**
The 29-module count comes from identifiers, and `Source`, `Buffer`
and `Pipe` are words that java.io, scala.collection and okay-http all
use. Adding a dependency a module does not need is invisible until it
costs the build a serialisation edge.
