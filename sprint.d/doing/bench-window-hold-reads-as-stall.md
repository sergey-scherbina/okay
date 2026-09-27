- [ ] bench-window-hold-reads-as-stall — PRIORITY: HIGH (it can stop
      the CI runner's push). `gate.sh` holds its start while a benchmark
      is queued (bench-window: at most OKAY_BENCH_GATE_MAX_WAIT, 900 s)
      and writes ONE line for the whole hold; `gate-retry.sh` kills a
      gate whose log has not grown for GATE_STALL_MIN, 10 min. So any
      gate run through gate-retry while a benchmark sits queued for more
      than 10 min is killed as STALLED before sbt starts, three attempts
      in a row, rc=99 "no verdict" — seen 2026-09-26 19:02-19:42 on
      effect-row-recursion-cost's gate (three attempts, each "bench
      window: a benchmark is queued ... holding"; each "STALLED: nothing
      written for 10 min"). `scripts/ci-runner.sh` gates `family all`
      through the same gate-retry, so the runner hits it too whenever a
      lane's A/B is queued. THE FIX, smallest first: the bench-window
      hold loop prints a heartbeat line every minute ("still holding for
      benchmark(s) <pids>, <n>s") so the log grows and the stall
      watchdog reads it as alive; alternatively gate-retry's watchdog
      treats a trailing "gate: bench window" line as not-yet-started.
      Pin it with a gate-retry selftest: a held gate longer than
      GATE_STALL_MIN must reach its verdict. (2026-09-26, found by
      effect-row-recursion-cost)
