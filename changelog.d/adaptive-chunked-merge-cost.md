## adaptive-chunked-merge-cost — Scheduler.forkLong: the chunked merge on adaptive back to Loom's time

The `adaptive` default (c29a5820d) made `merge(chunked = true)` 1.5-1.9x
slower than Loom on both roads. Profiled and bisected by knob: the merge
forks its two feeds from the consumer's thread, outside the pool; the
second waited in the submission queue until the monitor had seen it at
the head a whole tick (100-200 us), which a ~200 us merge pays in full.
The caller knows its feeds are long, so it now says so:
`Scheduler.forkLong` (default `fork`; on `own`/`adaptive` a sleeping
worker claimed and woken at once). `okayChunked` 351 -> 185 us (Loom
197-229), `okayChunkedShared` 380 -> 198 (Loom 202); `fork` unchanged
(spawnJoinSeq 112.4 vs 112.8 against the merge-base). The elementwise
`buffer` keeps `fork` — measured worse with it; its 1.13x at cap 64 is
filed as `adaptive-elementwise-small-ring`. Laws in TestOwnMonitor (red
first); diagnostic `ChunkMergeScaleBenchmark`; docs/schedulers.md;
specs/adaptive-chunked-merge-cost.md; okay2 parity filed.
