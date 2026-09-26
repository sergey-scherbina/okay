## bench-window — a benchmark gets its quiet box by protocol

Readers–writers with writer preference (Courtois, Heymans & Parnas
1971) over files and pids: every `gate.sh` holds a token in
`$TMPDIR/okay-bench/gates/` while it runs; a queued JMH lane files a
request in `want/`; a gate that STARTS while a request is live waits —
at most `OKAY_BENCH_GATE_MAX_WAIT` (15 min), then starts anyway and
says so — while running gates finish untouched, and the lane starts
when no token is live and `quiet` holds. A dead pid's file blocks
nobody. `jmh-lane.sh` now queues behind a held lock (an hour,
`JMH_LANE_LOCK_WAIT`; 0 refuses as before) instead of refusing, which
had pushed every caller into its own retry loop. `sh
scripts/bench-window.sh --status` names who holds what. Found by
ready-merge: 101 lane attempts in an hour never met a quiet box.
Selftests: bench-window-selftest (sh and bash), jmh-lane-selftest 4–4d,
gate-selftest 10; two mutants watched failing. AGENTS.md's benchmark
paragraph says what a held gate looks like. Stage 2 (demote gates to
E-cores instead of draining) is backlog `bench-window-demote-measure`.
specs/bench-window.md.
