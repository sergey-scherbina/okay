## fleet-stop-race - TestFleet waits for the Stop to land before it ticks

okay-agent's "Stop ends at the current tool" failed once in the whole
`affected` gate of 2026-09-29 and once alone right after, both at the same
line: after `send(a, Stop)` and a tick, agent `a` never reached
`Interrupted`. `Fleet.send` answers that the actor's mailbox took the
message, not that the actor applied it. When the tick won, the runner's
checkpoint 2 read "go on" and parked for a third tick the test never
sends. The Pause case in the same suite already waits for its phase
before ticking; the Stop case now waits for `Stopping` the same way. The
library is unchanged: "at the current tool" is the next checkpoint after
the Stop lands. 5/5 green alone after the change.

The same gate (5964 tests, JVM then JS and Native, the whole family since
build.sbt moved) had no other red: TestModels and TestDelta, the two
environment reds of the morning, are fixed and skipped-with-a-reason
respectively (intent-model-reproducible, delta-jdk-skip).
