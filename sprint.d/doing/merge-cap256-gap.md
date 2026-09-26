- [ ] merge-cap256-gap — `Source.merge` via `mergeReady` reads ~10%
      SLOWER than the old shared-channel road at capacity 256 only
      (85-90 us against 74-82 on `MergeCapBenchmark`; faster at 64,
      parity at 1024; specs/source-merge-via-ready.md Results). Refuted
      already: one-element turns (a quantum of 64 is in and left the
      gap) and the per-side buffer type (`relaxed.parts(1)` = the old
      part type, same number). The old arm at 256 was itself ±9 in 5 of
      6 attempts, so first establish whether the gap is the old road's
      GOOD forks: many forks (`-f 10`) of both arms, then the per-fork
      spread. Then `-prof gc` (a CLQ node per wake-up? an AtomicReference
      cell per registration?) and a count of producer parks per run at
      256 on both roads. TRIGGER: a user merging at a mid capacity, or
      anyone touching ReadyMerge's wake path. (2026-09-26,
      source-merge-via-ready)
