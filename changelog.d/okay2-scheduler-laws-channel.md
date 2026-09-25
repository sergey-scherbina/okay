## okay2-scheduler-laws-channel - the channel scheduler laws and the park-order check, for okay2

The two scheduler laws the Scala 3 core keeps beside its channels
(okay-stream's TestSchedulerLawsChannel), ported to okay2-stream over
okay2-platform's `SchedulerFamily`: cancel wins on a blocking send, and
`adaptive` survives a fiber that blocks inside its one worker. And
TestParkInterruptOrder, which okay2 did not have: every park site
(`block`, `blockAccepted`, `await`) reads the interrupt before the
answer, checked exactly on an already-interrupted caller.

The send law is rewritten rather than copied, because as written it
does not test what it says: it sends by `ch.send` — `Async.await`,
parked in `block`, never in `blockAccepted` — and sleeps where it
should force, so a fiber's answer after `cancel()` is the cancellation
whatever the park did. Measured by mutants of okay2's `Platform`: a
`block` reading `filled` before the interrupt passed the copied law
twice (TestSchedulerLaws caught it); the rewritten law forces the
window with a spinning registration inside `blockAccepted` and turns
red on the `blockAccepted` mutant (loom), as does TestParkInterruptOrder.
No source change. In the Scala 3 core the same law has the same
weakness, but its own TestParkInterruptOrder covers `blockAccepted`
directly, so nothing there goes unchecked.
