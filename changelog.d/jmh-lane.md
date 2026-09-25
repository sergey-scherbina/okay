## jmh-lane (ci-staged stage D) - the same lock discipline for benchmarks

Operator (2026-09-25), one level up from tests: the shared box is a
finite pool of CPU and RAM, and a JMH benchmark loads it exactly as a
test gate does — with the extra failure mode that contention does not
fail the run, it makes the NUMBER wrong, silently. Generalizes
ci-staged/ci-runner's shape (specs/ci-staged.md stage D; the
`ci-staged` skill's new A-7) from tests to benchmarks.

`scripts/quiet.sh`: `gate-retry.sh`'s own `quiet()`/`kill_tree`
factored into a file both it and the new `jmh-lane.sh` source — one
threshold set (`busy-sbt=0`, `load<15`, `free>=10GB`), not two that
could drift (policy's own P-6). Verified identical readings from
`gate-retry.sh --probe` and `quiet.sh --probe`, from the main checkout
and a worktree.

`scripts/jmh-lane.sh "<sbt Jmh/run command>" [attempts]`: a lock
separate from `ci-runner.sh`'s (`.work/jmh/lock` vs `.work/ci/lock` —
`quiet()` itself keeps a lane and a whole-build gate from overlapping,
since a live gate is a busy-sbt process either one sees), waits for
quiet before EVERY attempt (not only the first — a
contamination-triggered retry must not blindly re-run into the box
that just ruined the previous one), runs the command via a bare `$SBT`
call (matching `scripts/ab-defaults.sh`'s own existing precedent —
`gate.sh`'s pass/fail parsing is tuned for a test summary line, not a
JMH result table), and checks quiet AGAIN right after: a busy reading
discards the result as contaminated and retries the same lane, up to
5 attempts by default.

`scripts/jmh-lane-selftest.sh`: 6 cases against a fixture directory (a
fake `quiet.sh` popping scripted quiet/busy answers off a queue file,
a fake `sbt` that always succeeds — never real system load, never a
real JMH run). Found one real bug before it shipped: the first draft
waited for quiet only ONCE, before the whole retry loop, so a
contamination-triggered retry re-ran immediately into whatever the box
was doing next, defeating the point.

AGENTS.md's Benchmarks bullet now names the script; the `performance`
skill (agent-plugins) points at `ci-staged`'s A-7 instead of restating
the discipline.
