## dataflow-staging-topic - exactly-once from log to log, and the state that survives

Stage 11 of specs/dataflow.md. The engine's output goes to a topic,
the coordinator dies between the append and the journal commit, and
the output holds every pane once.

THE DIFFERENCE FROM STAGE 9 IS ONE WORD: SURVIVES. `TestStaged` closed
the commit window with a two-phase writer and proved the LOGIC - a
successor asked to redo an epoch the writer already applied drops it.
What it could not prove is that the knowledge survives the death: its
`applied` counter is a field of an object in the test's own JVM, and
the simulated death never took it away. A real coordinator's death
takes everything.

So the writer's store is a TOPIC and THE DEDUP STATE IS THE OUTPUT. A
resumed process is built with no memory at all - which is what a new
process is - and learns the high-water epoch by reading the tail. The
writer's atomicity is the log's append, which is the one thing a log
gives that a cell does not.

SEEN FAILING: making the recovery read nothing reddens exactly the two
tests that exist for it, one of them naming the pane that appears
twice. And because a dedup test that passes because nothing was ever
repeated is worth nothing, the suite asserts that some successor WAS
asked to redo a landed epoch.

THE METHOD THE SPEC'S BOX ASKED FOR IS NOT THERE, and the module
boundary is the reason rather than an omission: `Sink.stagingTo(topic)`
in okay-cluster would drag okay-persist into a compile graph that
stops at okay-codec on purpose. The seam that exists is the right one -
`Sink.staging` already takes the two moments and the topic writer is
forty lines in whatever module owns the store.

AND ONE BOX WAS A DUPLICATE OF ANOTHER: "a resumed coordinator's
workers open at the journal's epoch" is the assertion box 2 has been
making since it landed, so one property held the stage open twice.
Same shape as the roadmap summaries this spec had corrected hours
earlier.

AND IT LANDED OVER A RED MASTER, which is worth recording because the
cause is the one AGENTS.md warns about. `proc-form-consumer` (10a0bf0d)
had landed three E198s in okay-ui's `TestFormProc.scala` - an unused
`u: Unit` parameter in three private defs whose shape `Proc.direct`'s
door dictates - and a full gate caught them only after a rebase pulled
the file in. A warm gate on okay-ui would not have seen them: "a green
verdict covers what was compiled, and a warm build compiles less than
you think". Announced in the room and fixed
here rather than left red while we coordinated - and the lane that
landed it fixed it on master in the same minutes (3712c8f6), so the
rebase took THEIRS and dropped mine. That is the coordination working:
a red master was named, and the agent who could fix it best did.

The same cold compile turned up three FEATURE warnings in okay-ui's
`TestUiFormValidation.scala`, invisible without `-feature` for the
reason `test-hygiene-tails` records - a colourless val goes through
`selfColor`, an implicit CONVERSION, and the file had no language
import. okay-ui's test scope is clean under the flag now too.
