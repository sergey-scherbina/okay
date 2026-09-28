## ready-merge-chunk-forward — poll, then park: a merge registers a side only when it has nothing else to do; the chunked roads stay on the shared channel

The third try at the chunk-on-ring bimodality counted what the two
refuted receive-side fixes had never varied — WHEN a side registers —
and found the storm: on the rebuilt chunked ring road a slow fork
registered a side ~205 times per op, 160 of them while the other side
still had work, 99 answered asynchronously as a one-chunk hand-over on
the producer's thread; the merge itself parked 8 times. Landed:
`Async.Await(register, poll = null)` — a registration that can be asked
without registering; `Channel.receiveManyNow` (SentinelChannel answers
its ring, two reads and no allocation when empty; the default answers
null); `Channel.drained`/`drainedChunks` pass it; `ReadyMerge` holds a
pollable side idle (typed `Held[X]`, no cast), polls it when a turn
passes and when the ring runs dry — `PollSpins = 100` more times — and
registers it only then. Four laws in `TestReadyMerge` (19 green), the
first watched red on the old code. Measured: registrations-with-work
160 → 0.0 in every fork, wakes 99 → 2, fast forks 188-197 us; the
elementwise road 1.01x / 0.96x / 0.98x at capacity 64 / 256 / 1024
against registering as before, same JVM code. NOT landed: the chunked
roads onto the ring — 2-4 of 10 forks stayed at 215-247 against the
shared channel's 195-204 (control, same session, 0/10), a caught-up
consumer waiting on the producers; the bar as written was not met
twice, so stage 2 waits for the next landing: the operator accepted
the 1.06x mean for one mechanism and asked for the hybrid wait (spin,
micro-sleep, block) first — the item stays on the sprint with the
plan. Refuted on
the way, by a counter: the merge living on a producer's thread after a
park. Rows in
`src/jmh/history.d/2026-09-27T201816Z-ready-merge-chunk-forward-probe.tsv`;
specs/ready-merge.md (the stage, Results); docs/guide.md §6.

### second landing — the hybrid wait as three givens, and the chunked roads on the ring at an accepted 1.06x

The operator accepted a 1.06x mean for one mechanism and asked for the
hybrid wait first. Built as three layers, each a `given`: `Pause`, the
platform's rungs (`spin`, `yieldNow`, `nano` = `parkNanos`, `block`,
and `threads`; JVM/Native real, JS empty — a test substitutes a
counting one); `Wait`, the strategy as a closed loop over the rungs
(`Register`, `Spin(polls)`, `Ladder(spins, yields, sleeps)` the default
at 100/50/4, `Cycle(spins, yields, cycles)`); `Merge`, the mechanism
(`Ready` on the ring, the default; `Shared` on one queue — kept as a
door by choice, the operator's ask, not deleted). `Source.merge`,
`mergeFlushing`, `either`, `mergeReady` take them `using`; every call
site compiles unchanged. Measured on the chunked ring road
(`okayChunked` k=16, 10 forks): the ladder 200.4 ± 3.8 against the
shared channel's 200.0 ± 2.0 with no fork above 225 — the tail that
stayed at 2-4/10 with every spin-only wait is gone; the cycle, the
operator's refinement, 208.0 ± 7.8 with 3/10 at 220-247 (kept as a
choice, not the default). `parkNanos(1)` measured 10-12 us on this box,
the floor of the timer and the window in which two producers make ~50
chunks. Laws count rungs and polls, not time: answered on the yield
rung without a registration (poll 121), registered after the whole
ladder (157 polls), `Register` at once, `Spin(10)` in 13, a counting
platform sees 100/50/4/1 and 400/200/4/1, `Merge.Shared` joins the
same multiset, the chunked failure law holds on both mechanisms, JS
registers at once. The bar of the second stage: below. Rows in
`src/jmh/history.d/…-ready-merge-chunk-forward-hybrid.tsv`;
specs/ready-merge.md (the second stage); docs/guide.md §6, typepedia.

