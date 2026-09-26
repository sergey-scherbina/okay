- [ ] ring-chunk-bimodal-forks — `mergeReady` over per-side CHUNK
      channels (the reverted merge-chunked-via-ready road) ran bimodal
      per fork on `ChunkFlushBenchmark`: ~195-210 us or ~246-266 us,
      the slow mode in 40-60% of forks, where the shared chunk channel
      held ~200-215. A per-fork split is a JIT decision's signature
      (see memory "inlining threshold has four faces"): the elementwise
      ring did NOT show it (merge-cap256-gap's forks were tight). To
      chase it: rebuild the road from the reverted commit on
      feature history, `-f 10`, then `-jvmArgsAppend
      -XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining` on one fast and
      one slow fork and diff what `ReadyMerge.step`/`Writer.expand`
      inlined. TRIGGER: someone wanting the chunked road on the ring
      again (one mechanism end to end). (2026-09-27, merge-chunked-via-ready)
