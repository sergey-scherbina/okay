# The chunked merge on the adaptive default: a feed waits a monitor tick

Status: implemented, 2026-09-29. Owner lane: `adaptive-chunked-merge-cost`.
Found by merge-flush-on-ring-gap (specs/ready-merge.md, the gap stage's
Results).

## Symptom

Since the default scheduler became `adaptive` (c29a5820d, 2026-09-28)
`ChunkFlushBenchmark.okayChunked` (k = 16, 2 x 2000 elements) reads
351 us against Loom's 201.5 on the same build, and every chunked lane
pays 1.5-1.9x, on the ring and on the shared road alike.

## Diagnosis, measured (rows in src/jmh/history.d, this lane)

- CPU profiles (async-profiler, both arms): the producers do the SAME
  CPU work per op (0.021 vs 0.0205 samples/op); under `adaptive` they are
  idle ~45% of the op while the consumer spins in `Wait.Ladder.until`
  (19% of its CPU). Time is lost waiting, not working.
- The gap is not proportional to the element count
  (`ChunkMergeScaleBenchmark`): parity at n = 250, +110-140 us at 2000,
  +150-230 us at 16000 — a roughly FIXED price per merge, paid once the
  merge is longer than it.
- REFUTED: the helper rule keeping both feeds on one worker —
  `adaptive.forLongTasks` (helpAfter 0, spreadAbove 0) reads 331 against
  the default's 331 at n = 2000. The rule is consulted every 16th
  COMPLETED task, and a feed is one task that runs for the whole merge.
- REFUTED: a resumed feed running on the consumer's thread — no producer
  frame appears on the benchmark thread in the CPU profile.
- CONFIRMED: `adaptive.monitorEvery(10 us)` reads 198 at n = 2000 (Loom
  191-227) and 1322 at 16000 (Loom 1333-1360). The merge forks its two
  feeds from the consumer's thread, OUTSIDE the pool; an outside fork
  wakes a worker only when nobody is awake, so the second feed waits in
  the submission queue until the monitor has seen it at the head for a
  whole tick (adaptive-outside-long-fibers-serial's rule): 100-200 us.
  That rule was built for 70 ms fibers, where a tick is nothing; a merge
  is over in ~200 us.

## The fix

The caller KNOWS a feed is long-lived — it runs for the stream's whole
life — which is the one thing the scheduler cannot know at fork time
(specs/schedulers.md rejected a per-fork signal for exactly that
reason: "both decide before anyone knows whether the fiber is long").
So the knowledge travels with the fork:

- `Scheduler.forkLong(prog)`: a fork the caller declares long. The
  default is `fork` — Loom, the event loop, `threads`, `drive` and every
  scheduler outside this repository are unchanged.
- `own`/`adaptive` override it: the same `fork`, then ONE parked worker
  is woken if there is one. Nothing is added to `fork` itself, to the
  per-task path, or to a scheduler with no parked worker.
- The CHUNKED feeds use it: `chunkedSide`, `chunkedMerge` (the shared
  chunked road) and `bufferChunked`'s `feedBatched`. The elementwise
  `buffer` and `Channel.merge` keep `fork` (Results: measured worse with
  it), and so do the flusher (mostly asleep) and the one-shot tail send.

## Behaviour

- [x] LAW, red first: on `own.unmonitored`, two fibers forked from
      OUTSIDE with `forkLong`, each spinning until the other has started,
      both start (a second worker took the second); the same pair with
      `fork` does not within the timeout (the first worker is awake, so
      the second fork woke nobody)
- [x] `forkLong`'s default is `fork` (a scheduler that does not override
      it behaves exactly as before)
- [x] the chunked lanes on the default: `okayChunked` within 1.06x of
      Loom, alternating arms, and the elementwise `MergeCapBenchmark` at
      cap 64 no worse than before
- [x] must not regress: `AdversarialBenchmark.forkJoin10k_okay` and
      `OwnMonitorBenchmark.spawnJoinSeq` (they use `fork`, which is
      untouched — a check, not an expectation)

## Rejected

- A 10 us monitor tick by default: it closes the gap but it is a daemon
  thread looking ten times as often for every `own` scheduler, to fix a
  problem only callers that fork long producers have. Not measured for
  its cost; not needed once the caller says what it knows.
- Waking on every outside fork (`wakeAbove(0)`): measured 287-307 us, a
  partial fix, and it is the per-fork signal `own` was measured without.

## Results (2026-09-29)

Rows: `src/jmh/history.d/2026-09-29T141148Z-adaptive-chunked-merge-cost.tsv`.
Every lane through `scripts/jmh-lane.sh`, arms alternating.

**Two cuts, the first half right.** `forkLong` as `fork` + `activateNext()`
took `okayChunked` from 351 to 291 us (1.49x Loom): better, not fixed.
`activateNext` wakes the first worker whose `parked` is true, and a
worker clears `parked` only when it RUNS, so the three wakes a merge
issues in a row (the first feed's `fork`, its `forkLong`, the second
feed's) could all land on the SAME sleeper. The second cut claims the
worker it wakes (`Worker.waking`, a CAS, cleared by the worker once
awake), and only `forkLong` claims — `fork`, the helper rule and the
monitor wake as before.

| lane (k = 16) | default, before | default, after | Loom, same session |
|---|---:|---:|---:|
| `okayChunked` | 351.0 | **185.4** (185.5 / 185.4) | 212.9 (229.3 / 196.6) |
| `okayChunkedShared` | 380.5 | **198.5** | 202.2 |
| `okayChunkedFlush` | 329.7 | **189.5** | 211.6 |
| `okayChunkedFlushShared` | 376.6 | **200.7** | 227-270 (one noisy round) |
| `ChunkMergeScaleBenchmark.chunked` n = 2000 | 331-334 | **182.2** | 191-228 |
| the same, n = 16000 | 1440-1570 | **1342.3** | 1333-1360 |

**The elementwise `buffer` keeps `fork`.** With `forkLong` on it too,
`MergeCapBenchmark` at cap 64 read 101.3 / 101.1 / 101.8 on the default
against Loom's 81.1 / 82.4 / 80.3 (1.25x, three alternating rounds); with
`fork` 92.6 / 89.3 against 80.8 / 80.6 (1.13x), which is where it was
before this lane (1.16x). A 64-slot ring blocks both spread feeds at
once, and a callback drive's resume costs more than Loom's unpark. That
1.13x is a different mechanism and stays open: backlog
`adaptive-elementwise-small-ring`.

**`fork` did not move.** `OwnMonitorBenchmark.spawnJoinSeq` (monitor
100us, own), the merge-base against this lane in the same session, three
alternating rounds: base 103.5 / 112.4 / 113.8, lane 112.8 / 115.5 /
112.4 — medians 112.4 against 112.8. Both are well above the 86.8 of
2026-09-27; that rise is on master already, not this lane's, and is
noted in `scheduler-flip-remeasure`. `AdversarialBenchmark.forkJoin10k_okay`
(work 100, 4x4, default) 2195.6 ± 139, against 2078-2246 recorded.

**Refuted on the way** (kept so nobody re-runs them): the helper rule
(`forLongTasks` 331 against the default's 331) and a resumed feed
running on the consumer's thread (no producer frame on the benchmark
thread's CPU profile).
