- [ ] scheduler-default-decision — AFTER own-managed-blocking: a
      measured answer to "should `Schedulers.auto` be `adaptive` where
      Loom exists", today `loom`. The case FOR: the default is what
      every `spawn`/`par`/`supervised` user gets, and it reads 472
      ops/s where `own` reads 14 486 (five-way spawn/join), loses §4
      (100 fibers: 24.0 vs kyo 18.5, on Loom), and `adaptive` with
      `overflow = 64` already beat Loom on the blocking-TCP lane
      (7.4 vs 8.5 ms per batch). The case AGAINST, which the decision
      must price and not wave away: Loom's blocking is free and
      UNBOUNDED, `adaptive`'s is bounded by `overflow` — more blocked
      fibers than overflow workers is a deadlock the JDK's pool has
      too and Loom does not; and a program whose fibers block on
      things the library does not own (JDBC) sees the stuck-check's
      latency, not managed blocking's. HOW: one table, all lanes
      matched pairs, `loom` vs `adaptive` (post managed-blocking) as
      the given: §4 (100 fibers, outside AND inside), §4b (10k inside
      and outside, work=100 and 10000), five-way (spawn/join, workers
      work=0/64, TCP blocking with a latency histogram, runtime entry),
      cancel 1 000 parked, `DirectParallelBenchmark.parallel8`, and
      the Wroclaw 8-core row (the headline; it must not move down). A
      correctness column beside the numbers: `TestSchedulerLaws`,
      `TestAdaptiveScheduler`, `TestReadyMerge`'s cancel laws (they
      run on `own` and Loom since ready-merge-own-cancel-window; the
      window still open on the callback drive is backlog
      `ready-merge-cancel-under-consumer-ops`), and one new law: N =
      overflow + 1 fibers blocking at once on the library's own doors
      still finish (managed blocking must grow past overflow for
      LIBRARY blocking, or the decision says the bound out loud).
      Either answer is a landing: the default flips with docs
      (docs/async.md, the effects guide) and the table in
      specs/schedulers.md, or it stays with the table as the reason.
      Rejected in advance: deciding on spawn/join alone (that is the
      mismatched-pair mistake of default-scheduler-shape, refuted
      2026-09-08). Spec: specs/schedulers.md Decisions. (2026-09-27,
      perf-plan)
