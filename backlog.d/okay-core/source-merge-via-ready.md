- [ ] source-merge-via-ready — ONE merge mechanism: `Source.merge` as
      buffer-each-side + `Source.mergeReady`. Measured (ready-merge-
      numbers, 2026-09-26, specs/ready-merge.md Results): with a fiber
      per side in both, the ring join read 98.6/99.0 us against
      `Source.merge`'s one shared two-part channel at 114.3/106.6 on
      2x500 — 0.86x/0.93x, bars separate both rounds, and the tighter
      lane. What it must keep: `capacity` counting elements, `chunked`
      (the batch road, `Chunks.merge`'s 10.7 us is a different lane and
      stays), `flushAfter`, the `A | B` result, drain-on-close and
      fail-after-drain (`TestChannel`, `TestStreamSource`,
      `TestMergeOrder`, `TestMergeEnds`). Re-measure MergeBenchmark,
      MergeCapBenchmark and ChunkFlushBenchmark through `jmh-lane.sh`
      before and after; the bar is no named loss. TRIGGER: operator's
      call — this changes the default merge every Source user gets.
      (2026-09-26, ready-merge-numbers)
