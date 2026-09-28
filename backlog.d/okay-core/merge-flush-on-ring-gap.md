- [ ] merge-flush-on-ring-gap — `Source.merge(chunked = true, flushAfter
      = Some(ms))` on the ring (`Merge.Ready`, the default since
      ready-merge-chunk-forward's second landing) read 238.1 ± 13.8 us
      against the shared road's 212.0 ± 7.0 on `okayChunkedFlush` (5 forks
      per arm, one round): 1.12x, over the 1.06x the operator accepted,
      where the unflushed lane read 1.03x. The likely cost: the ring road
      runs a flusher fiber PER SIDE (`chunkedSideOf` with a window builds
      `flusherFor` twice) where `Channel.mergeChunked` runs one over the
      shared queue — ChunkFlushBenchmark's own comment on `okayChunkedFlush`
      already suspected the flushers at 23% over `okayChunked`. THE LANE:
      (1) re-measure with 10 forks per arm, arms alternating, through
      `scripts/jmh-lane.sh`, plus `okayChunkedFlushShort` on the ring (its
      shared arm read ±91 us — the 1 ms flusher makes it timing-bound;
      say so or fix the lane); (2) if the gap holds: one flusher for both
      sides on the ring (a shared timer that flushes whichever side has a
      partial chunk), or `Merge.Shared` by default for the flushing shape
      only — both are one `given`/one branch away now that `Merge` is a
      value. Also unmeasured before that landing: the elementwise lanes on
      `Wait.Ladder` (last reading, spin-100: 84.8 / 68.3 / 63.0 at cap
      64 / 256 / 1024) — re-read them here. (2026-09-28, ready-merge-chunk-forward)
