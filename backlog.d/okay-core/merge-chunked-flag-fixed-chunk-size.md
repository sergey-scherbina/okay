- [ ] merge-chunked-flag-fixed-chunk-size — `Source.merge(chunked =
      true)` always chunks at `Source.ChunkSize = 16`
      (`private[okay]`, not a parameter of the flag), so the coroutine-
      pairing-vs-`Writer.expand` question (merge-chunk-size-curve-
      inverted, closed 2026-09-23) can never be exercised at the chunk
      sizes where it actually mattered (the original note's curve
      rose past k ≈ 256; 16 measured at ~0 overhead either way). The
      COMPOSED road (`l.chunked(k).merge(r.chunked(k)).unchunked`)
      already lets a caller pick k and already carries the fix; the
      FUSED flag does not. TRIGGER: someone wanting the fused flag's
      convenience (one call, no manual chunk/unchunk) at a chunk size
      chosen for their own workload — add `chunkSize: Int =
      Source.ChunkSize` to `merge`'s parameter list, thread it through
      `Channel.mergeChunked`, and only THEN does re-measuring
      `okayChunked` at k = 256/1024 answer whether the fix helps the
      flag-based road the way it already helps the composed one.
      NUMBER FIRST — do not assume parity at k=16 predicts anything
      at k=1024.
