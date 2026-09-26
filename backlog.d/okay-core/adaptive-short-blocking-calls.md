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
