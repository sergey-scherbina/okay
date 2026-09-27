## scheduler-default-decision - the default scheduler stays Loom, measured

- The question was whether `Schedulers.auto` should hand out `adaptive`
  instead of `loom` on a JVM with virtual threads. It stays `loom`. The
  arms were the same code with only `-Dokay.scheduler` changed, run in
  two alternating rounds (specs/schedulers.md, "The default").
- `adaptive` wins every fork/join and cancel lane. It takes 0.49-0.93 of
  Loom's time on §4, §4b, cancel 1 000 and `parallel8`, and is 26x faster
  on the five-way sequential spawn/join.
- It loses the two shapes a default cannot lose. Five-way blocking TCP
  runs at 0.45x, because 64 blocked fibers share 28 threads. On the
  Wrocław headline, eight long fibers forked from `main` take 355-605 ms
  against Loom's 106-111.
- The bound is now a law in `TestManagedBlocking`. `adaptive` survives
  exactly `n + overflow` fibers blocked in the library's doors. One more,
  with its releaser queued behind them, waits until something outside
  the scheduler releases one. On Loom the same program finishes.
- With `adaptive` as the given, the scheduler laws, `TestManagedBlocking`
  and `TestReadyMerge` all pass. The one red asserts that the property is
  unset.
- Added `AdversarialBenchmark.forkJoin10k_okayInside`, the inside shape
  on the default given, for the pair. Filed
  `adaptive-outside-long-fibers-serial` (backlog, okay-core) for the
  Wrocław loss.
- Docs: docs/schedulers.md, "Why the default is Loom, measured". Rows:
  `src/jmh/history.d/2026-09-27T192315Z-scheduler-default-decision.tsv`.
