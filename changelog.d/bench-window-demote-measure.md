## bench-window-demote-measure — gates no longer wait for benchmarks: they move to the efficiency cores

Stage 2 of bench-window, measured and landed. Control lane beside 14 CPU
burners on this 10P+4E box: 44.4 / 42.7 us with the burners in the
background QoS class (`taskpolicy -b`), 44.8 / 44.2 alone, 69.3 / 84.2
with the same burners in the foreground; a real core test gate did not
reach the lane either way. So a gate that meets a queued or running
benchmark now demotes itself (sbt and its forks inherit the class) and
runs on instead of waiting; `jmh-lane.sh` demotes the gates already
running before each attempt, judges quiet by the foreground only, and
restores every marked gate (`taskpolicy -B`) when it is done.
`OKAY_BENCH_DEMOTE=off` (or no `taskpolicy`) keeps stage 1's wait.
Selftests: bench-window 9-10, jmh-lane 4e; AGENTS.md says what a demoted
gate prints. specs/bench-window.md, rows in
`src/jmh/history.d/…-bench-window-demote-measure.tsv`.
