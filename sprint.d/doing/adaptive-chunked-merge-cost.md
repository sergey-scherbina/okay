- [ ] adaptive-chunked-merge-cost — the default flip to `adaptive`
      (c29a5820d, 2026-09-28) costs the CHUNKED merge 1.5-1.9x on
      both roads: `ChunkFlushBenchmark` k=16, same build, arms alternating
      by `-Dokay.scheduler`, 5 rounds (merge-flush-on-ring-gap,
      2026-09-29): `okayChunked` 351.0 vs 201.5 under loom (1.74x),
      `okayChunkedShared` 380.5 vs 203.2 (1.87x), `okayChunkedFlush` 1.51x,
      `okayChunkedFlushShared` 1.81x. The elementwise merge pays 1.16x at
      cap 64 only (`MergeCapBenchmark`, parity at 256/1024), and the
      flip's own re-run table (scheduler-default-rerun) had no chunked
      lane, so this went unseen. Rows:
      src/jmh/history.d/2026-09-29T115453Z-merge-flush-on-ring-gap.tsv.
      FIRST: where it goes — a feed fiber per side producing chunks
      runs long between parks, which is the shape `adaptive` treats
      differently from Loom (adaptive-outside-long-fibers-serial); read
      it with a profile of both arms before guessing. DONE WHEN the
      chunked lanes are within 1.06x of loom on the default, or the
      default's doc says what a chunked merge pays and why. PRIORITY:
      HIGH — `merge(chunked = true)` is a headline row (docs/benchmarks.md
      §6b) measured under loom. Related: scheduler-flip-remeasure.
      (2026-09-29)
