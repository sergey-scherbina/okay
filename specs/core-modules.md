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

## Stage 2 — `okay-workflow` (DONE)

`Wf` (980), `Proc` (551) and `ProcMacro` (698) with their thirteen
suites. `Replayable` stays, because `Delim` is typed on it.

It was a leaf and it behaved like one. The survey ran with the full
instrument this time — comments stripped, every platform source
directory walked, the grep keyed on the TOP-LEVEL SYMBOLS the files
define rather than their names, and the test scan reading string
literals — and the core named `Wf`, `Proc` or `ProcMacro` **zero
times in code**. Both halves compiled on the first attempt, and the
whole family needed exactly **one** `dependsOn`: okay-persist.
okay-ui uses `Proc` in a suite and got it transitively.

It is a crossProject, not a JVM module, because the three files sat
in the shared source directory and therefore compile on JS and Native
today; a JVM-only module would have been a silent loss of that. The
suites stay JVM, which is the shape they already had.

The core is now **52 files and 13 555 lines**, down from 74 and
21 914 — 38% gone in two stages, with no consumer's import edited.

## Stage 3 — `okay-data` (DONE)

`Sketch` (309), `Uid` (201) and `Hlc` (173), with their four suites.
`Windows` and `Tables` did not come here after all — they went with
the streams in stage 1 and stay there, because what they are built on
is `Stage` and `Bulk`.

**`Aggregator` stayed, and this is the law's fourth independent
confirmation.** Ten modules are typed on it (okay-cluster alone names
it 118 times); `Sketch`, which implements approximate aggregators
against it, had **ZERO consumers among the 73 modules**. The core was
carrying 309 lines for nobody. That asymmetry — an interface with ten
users beside an implementation with none, in the same subject — is
the cleanest example of the rule this spec opened with.

`Uid` and `Hlc` had exactly two consumers each, okay-crdt and
okay-security, and the survey named both before the compiler did.
They were the two `dependsOn` edges the whole family needed.

TWO THEMES IN ONE MODULE, deliberately: approximate aggregation, and
coordination-free identity. Splitting them now makes two modules of
309 and 374 lines with one consumer group each, which is structure
ahead of need. The trigger to split is either theme growing a second
file.

**A STALE TEST TITLE FELL OUT OF THE MOVE.** The core's
`TestReplayable` had a test called "Resource and Uid are refused:
acquiring twice and a fresh id are not replays" whose body only ever
summoned the Resource row. Uid was in the title and in nothing else.
The title is now what it tests, and the Uid half is asked for the
first time, in okay-data's `TestUidReplayable` — with the control the
repo's rule asks for: pointed at a replayable row the assertion
FAILS, so it is the refusal that carries it.

## Stage 4 — `okay-optics` (DONE)

`Optic` (720), `Fuse` (469), `Focus` (55) and the optic spelling of
zooming, with fourteen suites and the `ArrowLaws` helper.

**THE SEAM WAS HALF GONE BEFORE ANYONE TOUCHED IT.** This was filed
as blocked on two edges, `State.zoom` typed on `Lens` and
`Proc.procArrow` typed on `Optic.Arrow`. Stage 2 moved `Proc` out with
the workflow, so the second one left on its own — a lane closing
another lane's blocker without either knowing.

**AND THE REMAINING ONE WAS NEVER ABOUT OPTICS.** `State.zoom` took a
`Lens[S, S, A, A]` and used, of the whole optic, `l.get` and `l.set`.
So the core now carries `State.zoomWith(look: S => A, put: A => S => S)`
— an interpretation of one effect into another, spelled in the core's
own terms — and okay-optics gives the lens spelling back:

    extension (st: State.type)
      def zoom[S, A, X, F[+_]](l: Lens[S, S, A, A])(p: X ! State % A + F) =
        State.zoomWith[S, A, X, F](s => l.get(s), a => s => l.set(a)(s))(p)

`State.zoom(lens)(prog)` therefore still compiles character for
character. NO API BREAK, where stage 1 had to accept one at the
`Producer.concat` wall: an object cannot be reopened across
compilation units, but a singleton type can be EXTENDED, and that is
the difference. The spec's own estimate had said a rename was the
lane's one real cost; it was wrong, and the escape was in the
language.

`PState.zoom` was already `l[Zooming[X, R]](m)`, one line, so only the
`Optic.Strong` instance for the carrier moved. The `Zooming` alias
names a `Cont` and nothing else, and stayed.

TWO `dependsOn` edges: okay-workflow and okay-lex, both
`compile->compile;test->test` because `ArrowLaws` is typed on
`Optic.Arrow` and their suites run it. okay-codec, okay-ui, okay-sql,
okay-http and okay-persist all use optics and needed nothing —
transitivity again.

The core is now **46 files and 11 553 lines**, from 74 and 21 914:
**47% gone across four stages**, with not one consumer's import or
call site edited.

## Not yet — STM

`Providing.Facts` is backed by `TMap`, and `Stm.sim` is an instance
for `Sim`. Two seams, both small, both real. Worth doing for the same
reason optics was: it is the law applied where it is not yet obeyed.
And stage 4 is the precedent for how — look at what the core actually
USES of the thing it is typed against, before assuming the type is
the dependency.

## Behavior

- [x] one package `okay` across two artifacts resolves `given`s both
      ways on 3.9.0 (stage 0, probe)
- [x] the core no longer mentions `Channel`, `Source`, `Chunks`,
      `Pipe`, `Queues`, `Bulk` in code, on any platform
- [x] `Stream` and `Handoff` stay in the core and `Writer`/`Async`
      still compile against them
- [x] every module that used the cluster compiles with no import
      edited — only `dependsOn` added
- [ ] the moved suites run on JVM, and the cross suite on JS and
      Native, exactly as before the move
- [ ] `ChannelBenchmark` still generates and runs from its new home

## Results

**The 29-module estimate was wrong, and wrong in the useful
direction: EIGHT modules needed an edge.** `okayActor`, `okayJava`,
`okayLex`, `okayLive`, `okayReactive`, `okaySpark`, `okayZio` and
`okayFs2`, and nothing else. The rest of the 73 compile untouched
because an sbt `dependsOn` is TRANSITIVE for `compile->compile`: a
module that reaches channels through okay-actor or okay-lex gets
okay-stream with them. The estimate had counted identifiers in
sources, which is the right instrument for "who uses this" and the
wrong one for "who must declare it".

**The survey missed three files, and each miss had its own cause.**
Worth writing down, because the same two mistakes are available to
anyone repeating this for stage 2 or 3:

1. `Parallel.scala` lives in `src/main/scala-jvm-native/`, and the
   first survey walked `src/main/scala/` only. PLATFORM SOURCE
   DIRECTORIES ARE PART OF THE MODULE. It also turned out to be a
   file that had to be CUT rather than moved: `parMap` and
   `retryChunks` are chunk combinators, while `parAll`,
   `parTraverse`, `retry` and `supervised` need only Async and a
   Scheduler. They are now `ParallelChunks.scala` and
   `Parallel.scala` in their respective modules.
2. `Generate.scala` and `Lines.scala` were missed because the survey
   grepped for the names of the FILES that move — `Chunks`, `Pipe` —
   and these use the names of the TYPES inside them: `Chunk`,
   `Stage`, `Produce`. The fix was to enumerate the top-level symbols
   the moved files actually define and grep for those; the list is
   35 names, and it is the instrument stage 2 should start from.
3. `Windows.scala` and `Tables.scala` reach the stream module for one
   function each (`paneStage` on `Stage`, the query planner on
   `Bulk`). Both moved whole rather than being cut, because unlike
   `Parallel` neither half stands on its own here.
4. A FOURTH cause that only the GATE could find, because no compiler
   can: `TestErrorMessages` names `okay.Channel` inside a
   `compileErrors` STRING. The file compiles perfectly without the
   type — the reference is data until the macro runs — so the suite
   went green at `Test/compile` and failed at `sbt test` with a
   comparison failure. Stripping comments is not enough; a symbol
   survey has to look inside string literals too, and the one place
   that matters here is `compileErrors`.

**`Chunk` stayed in the core after all.** The spec above put the
whole chunked cluster in the module, and then `Producer.concat`
refused to compile — it is typed on `Chunk[X] ! Produce + G` and five
modules call it. Looking at what `Chunk` IS settled it:
`type Chunk[+A] = ArraySeq[A]`, a one-line alias for a standard
collection. That is an interface by the law above, so it moved next
to `Producer` in `Generate.scala`, and everything that FILLS a chunk
stayed in the module. No call site changed.

**Two test harnesses were shared rather than copied.** Two scheduler
laws use a `Channel` as their blocking device, so they had to follow
it; `SchedulerFamily` (the member list and `each`) is now a trait in
the core's JVM test sources, which okay-stream's tests see through
`test->test`. Copying the list would have been three lines shorter
and would have let a scheduler added later reach one suite and
silently not the other. `BenchCross` moved WHOLE instead, because
three of its five lanes are stream lanes and its 90-line harness
(`lane`, `only`, `platform`) does not divide.

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
