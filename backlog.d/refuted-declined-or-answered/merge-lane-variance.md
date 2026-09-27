- merge-lane-variance — ANSWERED 2026-09-27 (see the end). REPRODUCED THREE TIMES, TWO FIXES
      REFUTED, and one trap in the experiment itself recorded.
      `ChunkFlushBenchmark.okayChunked` (`merge(chunked = true)`)
      swings 4x on unchanged code. Per-fork, 5 iterations each,
      2026-09-10/11:
        run A  322 330 460 542 301 237   (min 231, max 700 per iter)
        run B  451 456 984 497 260 391   (min 227, max 1079)
        run C  217 358 351 554 347 406   (min 207, max 828)
      It is not warm-up: the iterations inside a fork often hold
      steady at one level and the FORKS differ, which is a per-JVM
      decision (thread placement on a 10P+4E box is the obvious
      suspect and is not yet tested — pin with `taskset`-equivalent
      or measure with `-jvmArgs` fixing the scheduler's parallelism).
      REFUTED, so nobody re-takes them: (1) replacing the fused
      road's `through(s)(Stage.unchunk)` with `Writer.expand` — run B
      is that build, and it is no better; (2) making the fused flag
      DELEGATE to `a.chunked() merge b.chunked()` when there is no
      flush — run C is that build, also no better.
      THE TRAP, worth more than either: run A read the composed lane
      at a steady 188-199 in the same session and it looked like the
      road was the difference. It may be the ORDER — JMH runs lanes
      sequentially, `okayChunked` was FIRST while the box was still
      settling, and the composed lanes ran minutes later. Any retry
      must alternate the two lanes A/B/A/B in separate invocations on
      a quiet box before concluding anything about the roads.
      ANSWERED 2026-09-27, by the lanes of merge-chunked-via-ready and
      ring-chunk-bimodal-forks, all through `jmh-lane.sh` and the bench
      window (quiet at both ends, contaminated tries discarded and
      retried): `okayChunked` on the shared-channel road, 10 forks, read
      198-205 us — ONE mode, ±2% — and 5-fork rounds 198-249 with one
      contaminated round. The 4x swings of 2026-09-10/11 were not seen on
      a quiet box: they were the box, which is what bench-window now
      prevents. (Note: `okayChunked` ignores ChunkFlushBenchmark's `k`
      @Param, so its "k = 16/256/1024" rows are three repeats.) A second
      MODE does exist on the RING road for chunks, and it is a real
      mechanism, not noise: ring-chunk-bimodal-forks,
      specs/source-merge-via-ready.md.

