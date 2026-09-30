# Bugs — okay-stream

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## windowjoin-trim-spins — `TestWindowJoin`'s agreement test never returns; the CI runner's family gate hangs on it
<!-- status: open
     lane: jvm (WindowJoin.scala, the machine; seen on the JVM fork, the Native run had not reached it)
     area: okay-stream/src/main/scala/WindowJoin.scala `trim`/`arrive`, from Source/Pipe pulls
     gate: none yet — the test that hangs IS the gate, and a hang is not a red the runner can bisect
     found-by: freer-base-remeasure (2026-09-30), waiting for the box to go quiet
     reporter: claude session_01FeSfUJ4Z4YBWrEERDnoghu, room seq 2026-09-30/28
     owner: stream-join-windowed (cd91b05ee), told in the room
     confirmed: no -->

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

**Repro.** `scripts/gate.sh "okayStreamJVM/testOnly okay.TestWindowJoin"`
on master at ebffb4a8a or later; watch the fifth test. If it passes
alone, the seeds that hang are the ones to find — the test's own
`note(s"seed …")` lines name them once a flight recorder is read.
