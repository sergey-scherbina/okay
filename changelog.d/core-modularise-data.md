## core-modularise-data - an interface with ten users beside an implementation with none

Stage 3 of specs/core-modules.md. `Sketch` (309 lines), `Uid` (201)
and `Hlc` (173) became `okay-data`, with their four suites. The core
is now 49 files and 12 872 lines, from 74 and 21 914 before stage 1.

THE CLEANEST CASE THE RULE HAS PRODUCED. Surveying this cluster put
two numbers side by side, in the same subject and the same file
neighbourhood:

    Aggregator, the interface            10 modules use it
    Sketch, an implementation of it       0 modules use it

okay-cluster alone names `Aggregator` 118 times. Nothing outside the
core had ever named `Sketch`, so the core was compiling 309 lines of
HyperLogLog, count-min and quantile sketches for everybody and for no
caller. `Aggregator` stayed; `Sketch` left. That is the rule stages 1
and 2 were built on, stated by the code rather than argued for.

`Uid` and `Hlc` had exactly two consumers each - okay-crdt and
okay-security - and this time the survey named both BEFORE the
compiler did, which is what the instrument rebuilt in stage 1 was for.
They were the only two `dependsOn` edges the whole 73-module family
needed.

A STALE TEST TITLE FELL OUT OF THE MOVE, and it is the kind that only
moves like this find. The core's `TestReplayable` carried a test
called "Resource and Uid are refused: acquiring twice and a fresh id
are not replays" whose body summoned exactly one row, Resource. Uid
was in the title and in nothing else, so the file compiled fine when
Uid left and said nothing. The title now says what it tests, and the
Uid half is asked for the first time in okay-data's
`TestUidReplayable` - with the control this repo's rule asks for:
pointed at a replayable row the assertion FAILS, so it is the refusal
carrying the test and not the shape of it.

AND A SEAM CLOSED ITSELF. Optics was blocked on two edges,
`State.zoom` typed on `Lens` and `Proc.procArrow` on `Optic.Arrow`.
Stage 2 took `Proc` away, so one of the two is simply gone; measuring
what is left found the zoom family has ZERO callers outside the core's
own tests - 25 mentions, all in three suites. The optics lane is now
cheap and its only real cost is a spelling, because `State.zoom`
cannot stay `State.zoom` in another artifact. Written into the spec
rather than taken here.

Spec: specs/core-modules.md. Commit 2c728933.
