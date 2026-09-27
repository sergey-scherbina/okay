## bench-window-hold-reads-as-stall — a gate held by the bench window says it is alive

A gate that starts while a JMH lane is queued waits up to 15 minutes
(bench-window) and wrote ONE line for the whole hold, while
`gate-retry.sh` kills a gate whose log has not grown for 10 minutes: a
long hold was killed as STALLED, three attempts, no verdict (a
sibling's gate, 2026-09-26 19:02-19:42), and the ci-runner's
whole-build gate goes through the same road. The hold now writes
`gate: bench window: still holding for benchmark(s) <pids>, <n>s of
900s` every 30 s (`OKAY_BENCH_HEARTBEAT`), half the watchdog's
one-minute window. `bench-window-selftest.sh` case 8 runs the real
`gate-retry.sh` over a gate held 150 s with `GATE_STALL_MIN=1` and
requires its verdict — watched red on the old code (STALLED, rc 99;
a 75 s first cut did not reproduce, the hold's own first line counts
as growth in the first window), green now; case 7 counts the
heartbeat. AGENTS.md says what a held gate looks like.
