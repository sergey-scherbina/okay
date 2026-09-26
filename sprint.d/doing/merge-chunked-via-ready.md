- [ ] merge-chunked-via-ready — the elementwise `Source.merge` is
      buffer-each-side + `mergeReady` since source-merge-via-ready; the
      chunked roads (`chunked = true` with or without `flushAfter`,
      `mergeFlushing`, `either`/`eitherFlushing`) still build one shared
      `Channel.mergeChunked`/`mergeFlushing` channel. Moving them means a
      `bufferChunked` WITH a timed flusher per side (the flusher is a
      second sender into that side's channel today), then `mergeReady`
      over the two chunk sources and `Writer.expand` to unchunk.
      Measure against `ChunkFlushBenchmark` and `MergeBenchmark`'s
      chunked rows — through jmh-lane, many forks: that lane is the one
      `merge-lane-variance` records swinging 4x. TRIGGER: the operator's
      "one mechanism" asked of the chunked road too, or a defect found
      in the shared chunked channel. (2026-09-26, source-merge-via-ready)
