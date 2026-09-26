- [ ] own-few-long-tasks-serial — PRIORITY: MEDIUM. `Schedulers.own`
      runs a burst of a FEW LONG fibers on one thread. Five-way
      benchmark, 8 workers x 4 096 items at work 64
      (docs/benchmarks.md §4a): `own` 1 847 ops/s against 3 466 on the
      default Loom scheduler — exactly the serial time (4 096 x ~135 ns
      = 0.54 ms). PROVEN by a thread probe (2026-09-26, the five-way
      clone's ThreadProbe): distinct threads running the work = 8 on
      Loom, 1 on `own`, 1 on `own.forLongTasks` (both thresholds 0),
      1 on `adaptive`. CAUSE, read in Platform.scala: a fiber forked
      from a worker is pushed on that worker's deque with no signal,
      and the helper rule — the only thing that wakes a sleeper for
      local work — is evaluated once per 16 completed tasks
      (`(windowRan & 15) == 0`); this burst is ~10 tasks, so the check
      never runs and the thresholds never matter. The same design is
      why `own` wins sequential spawn/join (14 486 against Kyo's
      6 371): keeping a tiny child home is right, the counting gate is
      what misses a long one. THE ROAD, measured against both lanes
      (spawn/join must not regress): decide by TIME as well as count —
      e.g. evaluate the rule when a task ends after running longer than
      `helpAfter` while the deque is non-empty (one nanoTime per task
      already read at the checkpoint, so read it at task end only when
      size > 0), or a coarse per-worker "local work waiting since"
      stamp a sleeper-waking tick can see. (2026-09-26, analysis of
      five-way-okay)
      CHOSEN (2026-09-26): a sysmon-style monitor per own scheduler that wakes
      parked workers for a worker stuck in one task with work waiting;
      specs/schedulers.md, "Two defects: local work nobody was told about".
