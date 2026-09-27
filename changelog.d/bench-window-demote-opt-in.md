## bench-window-demote-opt-in — HOTFIX: gates wait for benchmarks again; demotion is opt-in

bench-window stage 2 made a gate that met a queued benchmark move
itself to the efficiency cores and run on. With siblings' JMH lanes
queued back to back the queue never emptied, so a gate demoted at its
start stayed demoted end to end, and a whole affected matrix on 4
E-cores lost nine timeout-bound tests in seven modules (TestGenerate's
1M values: 243 s against its 120 s limit). The default is stage 1 again
— a starting gate waits, at most 15 min, with the 30 s heartbeat —
and `OKAY_BENCH_DEMOTE=on` keeps demotion for a caller who knows its
gate has no timeouts to lose. AGENTS.md and specs/bench-window.md say
what a demoted gate's reds look like.
