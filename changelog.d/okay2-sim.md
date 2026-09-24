## okay2-sim - deterministic concurrency simulation in okay2

okay's `Sim` in okay2 (specs/okay2.md stage 14): fibers on one seeded
scheduler, `SimChannel` with the real channel's contract, a virtual
clock, deadlock as an outcome, a replayable fault plan, and the decision
trace that makes a run reproducible. The Scala 3 suite, including the
runCmd close race — lost under some seed by the old rule, never by the
fixed one over 200 seeds — plus 100 000 yields and 1 000 fibers. No cast
in the scheduler: continuations are held at `Any` and a channel's typed
queue takes them by contravariance.

Docs: docs/okay2.md section 18.
