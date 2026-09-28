- [ ] drive-poll-then-park-measure — the runner-level poll-then-park
      landed (drive-poll-then-park, 2026-09-28) on the operator's ask
      without its measurement. MEASURE, one lane per `jmh-lane.sh`, 5-10
      forks: `ChunkFlushBenchmark.okayChunkedShared` (its consumer is the
      blocking runner, now waiting by `Wait.Ladder` before it parks)
      against the second landing's row (202.8 ± 4.0) — expected parity,
      its consumer never catches up; `ChunkFlushBenchmark.bufferDrained`
      (one `buffer(1024)(s).drained`, no merge) with `given Wait =
      Wait.Register` against the default — the lane where a catch-up was
      a registration and a hand-over, expected the gain; and the
      elementwise `MergeCapBenchmark` cap 64/256/1024 (the ring merge's
      own consumer under `toLazyList` now also waits at the runner for
      the merge's park). Record in a history file; the spec's stage
      carries the box. (2026-09-28)
