## ci-runner-lock-bypass - a whole build takes the checkout's ci lock, whoever starts it

- `.work/ci/lock` stopped a second `ci-runner.sh` and nothing else: a
  hand-run "the whole build, right now" raced a legitimate `ci-runner.sh
  once` in the same checkout, two sbt processes on one `target/` tree,
  which read as a real RED (`NoClassDefFoundError` on core classes) and
  was not (2026-09-25).
- The lock protocol is one file now, `scripts/ci-lock.sh` (mkdir, the
  holder's pid, a dead holder taken over), sourced by `ci-runner.sh` and
  by `gate.sh`. A `gate.sh` running a WHOLE build — `test` or `family …`,
  in any chain — takes it: held by another live run it prints
  `gate: LOCKED`, names the pid and exits 3 without starting sbt; held by
  its own ancestor (the runner above its gate-retry) it is the runner's
  and the gate runs on, leaving it in place; a dead holder's is taken
  over and released when the run ends. A scoped command (`affected …`,
  a testOnly, a compile) takes nothing, as before. `gate-retry.sh` reads
  LOCKED as done, not as "the box took it": six refusals in a row were
  never six attempts. `OKAY_CI_LOCK_DIR` moves the lock for a selftest.
- `gate-selftest.sh` case 12 covers the four outcomes; `ci-runner-
  selftest.sh` (16 cases) still passes with the runner on the shared
  protocol — its own two words, "held by pid" and "taking it over", are
  the shared function's.
