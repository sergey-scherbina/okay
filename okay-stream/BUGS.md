# Bugs — okay-stream

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## windowjoin-trim-spins — `TestWindowJoin`'s agreement test never returns; the CI runner's family gate hangs on it
<!-- status: fixed
     fixed-in: aa7ca9f5e
     lane: jvm (WindowJoin.scala, the machine; seen on the JVM fork, the Native run had not reached it)
     area: okay-stream/src/main/scala/WindowJoin.scala `trim`/`arrive`, from Source/Pipe pulls
     gate: TestSourceJoinWithin (the endless-sides tests on Loom), TestWindowJoin (three platforms)
     found-by: freer-base-remeasure (2026-09-30), waiting for the box to go quiet
     reporter: claude session_01FeSfUJ4Z4YBWrEERDnoghu, room seq 2026-09-30/28
     owner: stream-join-windowed (cd91b05ee), told in the room
     confirmed: yes — windowjoin-spin-fix (claude session_01UkcD4rZuMcWszYis8fWcGN), 2026-09-30 13:20 -->

**Owner's reading (windowjoin-spin-fix).** Both dumps had the munit
thread at `TestSourceJoinWithin.scala:41` — the early-stop test's
`joinEither()` on the `own` iteration — not in `TestWindowJoin`, whose
six tests are a list and had already passed. The loop `trim` was found
in was not stuck: it was RUNNING, one left row after another, because
the join's RIGHT side had stopped arriving and the join was told 25
million left rows for three right ones (`ProbeReadyMergeStarve`, the
ring's positions). Two defects, one in this file, one not:
(1) `trim` filtered the whole per-key buffer on EVERY arrival; with the
watermark standing still (the right side silent) nothing ever expired
and each arrival rescanned a buffer one row longer — quadratic, which
is the 286 s of CPU. Fixed here: eviction from the front only,
amortised O(1), the full filter once per `within` of watermark advance.
(2) The right side stopped because the `either` merge under it starves
a side on a scheduler with owned workers — `ready-merge-side-starves`
below, older than this lane. The endless-sides tests are pinned to Loom
until it closes; the join's laws are pinned by `TestWindowJoin` (no
scheduler) and the bounded `Source` cases on every scheduler.

**Seen.** The CI runner's `scripts/gate.sh family all` (pid 48118,
started 10:28 on 2026-09-30, gating stream-join-windowed and the lanes
after it) sat 65+ minutes with no new output. Its watchdog printed
`quiet for 480s but the workers burned 51x s and sbt 49 s of CPU —
still working` every window, which was true: one child, the forked JVM
for `okay-stream`'s tests (pid 63346, `-Xmx1g`), was RUNNABLE at 104%
CPU the whole time. `okay.TestWindowJoin` had printed its first four
tests on the JVM; the fifth, "agreement with joinSorted on a bounded
input whose timestamps all fall within reach" (TestWindowJoin.scala:86,
twenty seeded random rounds through `drive(fresh(within, within),
evs)`), never returned.

**The stack** (`jcmd 63346 Thread.print`, kept at
`.work/ci/diag/20260930-windowjoin-spin-63346.threads.txt` in the main
checkout, gitignored): thread `okay-own-1-0`, cpu=3935 s, RUNNABLE in

    okay.WindowJoin.trim(WindowJoin.scala:65)
    okay.WindowJoin.arrive(WindowJoin.scala:102)
    okay.WindowJoin.left(WindowJoin.scala:110)
    okay.WindowJoin$.$anonfun$3(WindowJoin.scala:157)
    okay.Pipe$package$.loop$5$$anonfun$3(Pipe.scala:624)
    okay.Pipe$package$.pull$3(Pipe.scala:610)
    okay.Pipe$package$.loop$5(Pipe.scala:624)   … repeated, deep

`trim` is `buf.filterInPlace(h => h.at + within >= wm)` over one key's
held rows; the frames above it are the pull loop feeding `left` a row
at a time. Which of the two loops fails to advance is the owner's to
read — the dump is the whole evidence this entry has.

**Why it matters beyond one test.** The gate watchdog is built to
catch a SILENT hang (no output AND an idle tree); a hang that burns
CPU looks like work to it, so the runner never reaches a verdict,
never pushes, and holds the bench-window token that every benchmark
on the box waits for. A rerun of the named suite would spin the same
way, so the runner's flake road does not apply either; the fix or a
revert of cd91b05ee is what frees the box.

**Second sighting, 2026-09-30 12:00, the other road.** The runner's
next family gate (pid 56588) ran `TestWindowJoin` GREEN, all six tests
(the agreement test in 0.051 s), and froze half an hour later after
`TestFeedCancel`'s last line: the fork's pool worker `okay-own-1-0`
(pid 73993, 101% CPU, 34 min) was RUNNABLE in the same loop, reached
this time from the Source/Writer road —

    okay.WindowJoin.trim(WindowJoin.scala:65)
    okay.WindowJoin.arrive(WindowJoin.scala:102)
    okay.WindowJoin.left(WindowJoin.scala:110)
    okay.WindowJoin$.$anonfun$3(WindowJoin.scala:157)
    okay.Pipe$package$.loop$5 / pull$3 (Pipe.scala:624 / 610) … deep
    okay.Freer.resume(Free.scala:152)
    okay.Writer$.loop$6(Writer.scala:167)

— dump at `.work/ci/diag/20260930-stream-spin-73993.threads.txt`.
So the machine test is not the only entry, and a green `TestWindowJoin`
does not clear it: a `Source.joinWithin`-driven program (the suites
around that point are `TestDocExamplesWindowJoin` and
`TestSourceJoinWithin`) leaves the join spinning on the ONE pool
worker, and every later suite that needs the pool waits for ever.
Killed by PID again on the operator's standing word; the runner cannot
push the range 5e587d84f..98fab9c2e until this is fixed or reverted.

**Repro.** `scripts/gate.sh "okayStreamJVM/testOnly okay.TestWindowJoin"`
on master at ebffb4a8a or later; watch the fifth test. If it passes
alone, the seeds that hang are the ones to find — the test's own
`note(s"seed …")` lines name them once a flight recorder is read.

## ready-merge-side-starves — a ready merge of two endless sides stops delivering one of them; the hot side's ring ends "full" to its pusher and "empty" to its popper
<!-- status: open
     lane: jvm (`Schedulers.own.build` and the adaptive default; not seen on Loom)
     area: okay-stream ReadyMerge.scala / SentinelChannel.scala / Ring.scala — the merge's poll-then-park over a single-consumer ring
     gate: none — `ProbeReadyMergeStarve` (ignored) reproduces in ~20 rounds of 300, under 11 s
     found-by: windowjoin-spin-fix (2026-09-30), chasing windowjoin-trim-spins
     reporter: claude session_01UkcD4rZuMcWszYis8fWcGN
     owner: unassigned — source-merge-via-ready / ready-merge's author, or the next lane in the channel
     confirmed: yes, on master at ebffb4a8a and on a01fcf9c7 (before channel-route-per-producer) -->

**Seen.** `Source.of(LazyList.from(0)).either(Source.of(LazyList.from(1000000)), capacity = 4)`
folded until the THIRD `Right`, under `Schedulers.own.build` or the
default scheduler, each round a fresh merge under `sch.fork`: rounds
16–27 of 300 never return (8 s watchdog), five runs in five. Under Loom
the same 300 rounds pass. `TestReadyMerge`'s endless-merge test never
saw it because it takes N elements of EITHER side.

**State at the starvation** (the sides' channels caught by hand,
`SentinelChannel.debugState` plus the ring's positions and stamps,
instrumentation not landed):

    left:  size=1 hasReady=false receivers=0 senders=0 head=24259162 tail=24259163
           stamps=[24259008,24259042,24259043,24259047] cap=4
    right: size=0 hasReady=false receivers=1 senders=0 head=1 tail=1 stamps=[4,1,2,3] cap=4

The right side pushed ONE element ever, it was popped, the merge is
registered on it (`receivers=1`) and its feeder is neither parked as a
sender nor running on any worker. The left ring's head and tail are
consistent with each other and some thirty laps AHEAD of every stamp:
slot 0's stamp says "free for a push at …008" though tail passed …008
long ago, slots 1–2 say "published at …041/…042" though head passed
them, slot 3 "free at …047". So the pusher reads d < 0 (full) and the
popper reads d < 0 (empty) and both wait for ever; the merge consumer
polls both idle sides in `settleIdle`'s ladder, every other worker is
parked. Ruled out with the ring instrumented: no two pops and no two
pushes ever overlapped (a nesting counter in `pop`/`popMany` and
`push`/`pushDeciding`), and the left feeder's own code never ran on two
threads (a counter in the source's `map`). Not seen: what moves `head`
and `tail` past four slots without republishing their stamps, given
one popper and one pusher at a time. A `popMany` whose `sink` throws
leaves exactly such holes, one lap deep; thirty laps is something else.

**Repro.** Un-ignore `ProbeReadyMergeStarve` and
`scripts/gate.sh "okayStreamJVM/testOnly okay.ProbeReadyMergeStarve"`.
It prints the scheduler's counters, both channels' state, the
feeder-overlap verdict and the live worker stacks.

**Why it matters.** Any `either`/`merge`-built operator whose consumer
waits for a specific side — `Source.joinWithin`, a `zip` over a merge,
a fold for one tag — can wait for ever on the default scheduler when
the other side is hot. `TestSourceJoinWithin` runs its endless-sides
tests on Loom until this closes.

