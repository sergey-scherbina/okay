## jvm-parallel - the bound does not cost the JVM, and now that is measured

`gate-bound-test-fanout` set `ThisBuild / Test / parallelExecution :=
false` to stop 165 runner processes deadlocking a 14-core box. JS and
Native needed it — there a test CLASS is an operating-system process.
The JVM did not, and the comment said so and left it unpriced: "one
line instead of forty is the reason, and the measurement below is what
pays for it". That measurement did not exist. It does now, and it
refutes the worry.

`family jvm`, 59 projects, three alternating rounds, `set` in BOTH
arms so neither pays for the other's reload:

| parallelExecution | rounds | min |
|---|---|---|
| false (serial inside a module) | 145, 118, 126 | **118 s** |
| true | 204, 128, 141 | 128 s |

Serial wins all three PAIRED rounds and the minima by 8%. The reason
is not subtle once seen: fourteen modules already run at once, so
turning test classes loose inside each one oversubscribes a box the
coarse parallelism already fills. The one line is right for all three
platforms, not a compromise the JVM absorbs.

FIVE INSTRUMENT FAILURES STAND BEHIND THOSE SIX NUMBERS, and they are
worth more than the numbers. A success detector grepped for a line sbt
never prints, so every round read as a failure. A peak-process sampler
matched `^node$` box-wide and counted a sibling's build. A `pkill -f`
matched the text of my own background command and killed the
measurement's wrapper. A liveness check grepped for a worktree path in
a command line that never carries one. And the first A/B ran `affected
master` in a worktree that IS master, so all four runs executed zero
modules and timed sbt's startup — 15, 33, 21, 18 s, perfectly
plausible and perfectly meaningless.

Every one of them produced a believable number. That is the lesson:
the instrument needs its own control BEFORE the subject gets measured
— a run whose module count is printed beside its time, a detector
checked against a known pass, arms that differ in one thing only.
