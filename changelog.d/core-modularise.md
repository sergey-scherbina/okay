## core-modularise - streams and channels leave the core, and the rule that let them

The operator asked whether streams and channels should move out of the
core into their own module, and then the general form: what else can
leave, and what must stay. The graph already held the answer, so the
first work was reading it rather than arguing about it.

THE RULE, WHICH WAS NOT CHOSEN. **An interface stays in the core; the
machinery that implements it leaves.** It fell out of the measurement
three times, at three unrelated seams. The whole control layer - Cont,
Free, Effects, Monad, Delim, State, Direct, Par, Resource, Logic,
Throws, Validated, Static - names a channel, a source or a chunk ZERO
times in code; every apparent reference was a comment. The only two
real edges are `Async` to `Handoff` (63 lines, a rendezvous) and
`Writer` to `Stream` (218 lines, a typeclass whose whole interface is
`uncons`). Then `Optic.aggregateWith` and `Bulk.aggregate` turned out
to reach for `Aggregator` from opposite ends of the library, and
`Delim` to reach the workflow cluster once, for `Replayable` (78
lines) rather than for `Wf` (848).

A FIRST PASS FOUND DEPENDENCIES THAT WERE ALL PROSE. Grepping around
comments instead of stripping them is the shape
`instrument-needs-its-own-control` warns about: a believable number
about the wrong thing. Every count in specs/core-modules.md is from
stripped source.

WHAT MOVED: `okay-stream`, a crossProject over JVM, JS and Native -
Channel, SentinelChannel, AbruptChannel, Source, Pipe, Pipeline, Bulk,
Chunks, ChunkBuf, Queues, Ring, Growing, Fifo, AdaptiveFifo, Segments,
Buffer, Tables, Lines, Windows, and the chunked half of Parallel. The
core went from 21 914 lines to about 15 800.

THE PACKAGE DID NOT CHANGE, AND THAT WAS PROBED FIRST. Before a file
moved, three separate compilations on Scala 3.9.0 established that one
package `okay` may live in two artifacts with `given` instances on both
sides: `import okay.*` and `import okay.given` resolve in both
directions, at compile time and at run time. So not one consumer's
imports were edited.

EIGHT MODULES NEEDED A `dependsOn`, NOT THE 29 PREDICTED. okayActor,
okayJava, okayLex, okayLive, okayReactive, okaySpark, okayZio, okayFs2.
The rest of the 73 compile untouched, because an sbt dependency is
transitive for `compile->compile`. The 29 came from counting
identifiers in sources, which answers "who uses this" and not "who
must declare it".

THREE FILES THE SURVEY MISSED, EACH FOR ITS OWN REASON, all caught by
the compiler and all written into the spec: `Parallel.scala` lives in a
PLATFORM source directory the first walk did not enter, and had to be
cut in half rather than moved; `Generate.scala` and `Lines.scala` use
the names of the TYPES inside the moved files (`Chunk`, `Stage`,
`Produce`) rather than the names of the files, so the survey's
instrument was rebuilt around the 35 top-level symbols that actually
left.

`Chunk` STAYED. The plan had the whole chunked cluster leaving, and
then `Producer.concat` refused to compile - five modules call it, and
it is typed on `Chunk`. `type Chunk[+A] = ArraySeq[A]` is a one-line
alias for a standard collection, which is an interface by the rule
above, so it sits next to `Producer` now and everything that FILLS a
chunk is in the module.

Two scheduler laws use a Channel as their blocking device and had to
follow it, so `SchedulerFamily` is now a shared trait rather than a
copied member list: copying would have been three lines shorter and
would have let a scheduler added later reach one suite and silently
not the other.

Filed, not taken: `okay-workflow` (Wf, Proc, ProcMacro, already a
leaf) and `okay-data`. Optics and STM are extractable in principle and
not by moving files - `State.zoom` is typed on `Lens`, `Providing.Facts`
on `TMap` - and cutting one of those seams is worth more than the lines
it moves.

Spec: specs/core-modules.md. Commits bbf8eaf8 (spec and the stage-0
probe), e09f59ed (the move), a35000d4 (what the measurement refuted).
