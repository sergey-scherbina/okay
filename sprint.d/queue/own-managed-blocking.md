- [ ] own-managed-blocking — a fiber that blocks on an `own`/`adaptive`
      worker says so BEFORE it parks, and the scheduler activates a
      spare worker at once instead of noticing 5-100 ms later
      (`ForkJoinPool.ManagedBlocker`'s protocol, ours). WHY: `own` is
      2.3x kyo and 30x the Loom default on sequential spawn/join
      (five-way 2026-09-26: own 14 486, kyo 6 371, default 472 ops/s)
      and is not the default only because a blocking call holds a
      worker; today the stuck-check (`Platform.scala:687-699`, on the
      timer wheel every `stuckAfterMillis`: 5 ms on `platform`, 100 ms
      on `adaptive`) is the only remedy, so every `join()`,
      `receiveBlocking`, `Nio` park inside a fiber costs that latency
      before help arrives. `specs/schedulers.md:43` says `adaptive`
      "moves a fiber to loom when it blocks" — NOT BUILT (nothing can
      move a running platform-thread stack); this is the honest
      version of that row, and the row is corrected by this lane.
      HOW: `CanBlock.block`/`blockAccepted` (Platform.scala:57, :119)
      are the ONLY doors the library blocks through, and a worker is in
      `Owned.current` (ThreadLocal, :447): before the park, if the
      current thread is an `Owned` worker, `owner.blocking(w)` — mark
      the worker blocked, wake or start one spare (bounded by
      `overflow`, as the stuck-check is), and on return
      `owner.unblocked(w)` — the spare steps down when it next runs
      dry, as overflow workers should but today never do (they are
      never retired: `live` only grows). Third-party blocking (JDBC, a
      `Thread.sleep`) is still the stuck-check's; say so. SECOND PART,
      same files: `completed.incrementAndGet()` (:516) runs on EVERY
      task whenever the stuck-check is on — a contended global atomic
      per task across 14 workers on exactly the schedulers meant for
      real programs (`platform`, `adaptive`); replace with a plain
      per-worker counter the monitor/stuck-check SUMS on its tick
      (`Worker.ran` is already there, plain). VERIFY: (1)
      `forkJoin10k_okayOwnInside` on `adaptive` vs `own`, before and
      after — if adaptive lags own by >3% before, the counter is the
      cause and the after must close it; (2) the five-way TCP-blocking
      lane on `adaptive`: a latency histogram per batch, the
      5-100 ms stair gone; (3) spawn/join and `spawnJoinSeq`
      (`OwnMonitorBenchmark`) unchanged within noise — the block door
      must cost nothing on the non-blocking path (a ThreadLocal read
      is already paid there); (4) `TestSchedulerLaws`/`TestOwnMonitor`
      green, plus a law: a fiber blocking inside a worker with
      `overflow = 1` does not stall a sibling fiber for longer than a
      task. Rejected: the per-task clock read (schedulers.md, refuted —
      on the hot path, decides too late) and a shorter stuck interval
      (one completion per interval defeats it). Spec:
      specs/schedulers.md, a new Decision + Results. Gates
      `scheduler-default-decision`. (2026-09-27, perf-plan)
