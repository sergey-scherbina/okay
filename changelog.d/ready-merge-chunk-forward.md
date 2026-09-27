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
consumer waiting on the producers; the bar was not met twice, stage 2
was not run, the item is refuted with its reopen condition. Refuted on
the way, by a counter: the merge living on a producer's thread after a
park. Rows in
`src/jmh/history.d/2026-09-27T201816Z-ready-merge-chunk-forward-probe.tsv`;
specs/ready-merge.md (the stage, Results); docs/guide.md §6.
