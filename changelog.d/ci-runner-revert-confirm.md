## ci-runner-revert-confirm - a red must be confirmed before it costs a revert

Two real reverts landed on 2026-09-25 on a red that never repeated,
both infrastructure noise, not a fault in the reverted change:
`mrjar-jdk25-ci-gap` (a `TestSignals` timeout under load) and
`stack-safety-json` (a Native test binary killed by signal 9, an
`okayAsyncNative` accept timeout — the module did not even depend on
what the lane touched). Both were caught and re-landed by hand
afterward. Two fixes, filed separately and both closed here because
they cover different shapes of the same failure:

**Zero, before anything else runs** (`ci-runner-reverts-on-infra-red`):
a RED whose log names no test at all (`gate.sh`'s own "a failure this
script does not recognise" shape — checked with the exact `grep -q
"==> X"` test `gate.sh` itself uses) is a SIGNAL, not a verdict —
treated like `gate: KILLED`/`gate: STALLED`: no bisect, no revert,
retried on the next kick. This is the shape `stack-safety-json` hit.

**Two, after a culprit is named** (`ci-runner-revert-needs-confirmation`):
before reverting the sole or bisected culprit, wait for a quiet box
(the same `quiet()` `gate-retry.sh`/`jmh-lane.sh` already share) and
re-run ONLY that commit's own scoped `affected` gate, alone, once more.
GREEN there is a flake — logged, NOT reverted, NOT pushed; the next
whole-build turn re-tests the range fresh, and nothing was lost by not
reverting. This is the shape `TestSignals` hit: a real test named, a
real `==> X`, but not reproducible on its own.

`scripts/ci-runner-selftest.sh`: 13 cases now (was 11) — `9b` for the
infra-noise pre-check, `10b` for a confirmed-green flake on the simple
path, and `10`/`11`'s existing revert paths now also assert the
confirmation ran. A real bug caught while wiring this in:
`scripts/ci-runner.sh` called `quiet` (for the new wait-before-confirm
step) without ever sourcing `scripts/quiet.sh` — an undefined function,
never previously exercised because nothing called `quiet` from
`ci-runner.sh` itself before this lane.
