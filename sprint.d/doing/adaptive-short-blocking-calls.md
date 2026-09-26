- [ ] adaptive-short-blocking-calls — PRIORITY: MEDIUM. In the five-way
      benchmark's blocking TCP lane (64 lanes, a 1 ms server,
      docs/benchmarks.md §4a) `Schedulers.adaptive` reads 5.4 batches/s
      against 117 for the default Loom scheduler and 121 for CE — the
      same collapse Kyo shows there (4.9), where the benchmark's author
      traced it to queued children collecting on a few workers and slow
      adaptation to short blocking calls. `adaptive` is `own` plus a
      worker when a fiber blocks; for 64 concurrent 1 ms calls the
      worker arrives too late or too few. FIRST STEP: a thread dump and
      the adaptive scheduler's own counters during that lane (the
      five-way harness: compare/five-way/run.sh with runtime
      okayAdaptive and `-p transport=blocking`), then decide between
      detecting the block sooner and handing a blocked worker's deque
      to a fresh one. The callback transport is unaffected (142, the
      best on the row). (2026-09-26, five-way-okay)
      PROBED (2026-09-26, the five-way clone's ThreadProbe, 64 lanes x
      4 calls of a 1 ms sleep): Loom reaches 64 concurrent calls, 8.5
      ms a batch; `adaptive` peaks at 5 concurrent calls on 5 threads,
      167 ms a batch. CAUSE, read in Platform.scala: the 64 lane fibers
      are forked from inside one worker (its deque, no signal — the
      same gate as own-few-long-tasks-serial), that worker blocks, and
      the stuck-check adds a thread only when NOTHING has completed for
      `stuckAfter` (100 ms by default). Lanes keep completing every few
      milliseconds, so the check sees progress and adds almost nothing.
      "No progress" is the wrong test when a few blocked workers hold
      most of the pending work: a worker in one task longer than
      `stuckAfter` with a non-empty deque is the signal to act on.
      CHOSEN (2026-09-26): the same monitor, which on a watched scheduler also
      starts overflow workers when nobody is parked; specs/schedulers.md,
      "Two defects: local work nobody was told about".
