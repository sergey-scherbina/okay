- [ ] **merge-knows-its-producers** — `Channel.merge`, `mergeChunked`,
      `mergeFlushing` (two producers) and `Channel.buffer`,
      `bufferChunked` (one) all build `Channel[A](capacity)`, the
      `growing` default whose reason to exist is a producer count
      unknown at construction — and whose one-shot swap is the place a
      producer's own order can break once. A seam between streams
      knows its sides: two parts from the start for a merge
      (`relaxed.parts(2).each(capacity)`), a plain ring for a buffer.
      No adoption, no swap, per-source order exact by construction.
      spec: specs/channel-known-producers.md (the operator's question
      — a channel as a stream written and read from two sides — and
      what it buys).
      HOW: `Channel.forProducers(n, capacity)`, one place; the old arm
      behind `-Dokay.channel.known=growing` / `OKAY_CHANNEL_KNOWN` for
      the A/B. Law: `TestMergeOrder` (exact per-side order, rounds
      with a fresh consumer thread). Numbers: MergeBenchmark,
      MergeCapBenchmark, ChunkFlushBenchmark.okayChunked, the
      IdiomaticApiBenchmark buffer rows — both arms alternating, own
      JVMs. DONE WHEN the law is green, the pairs are matched or the
      loss is named, and docs/queues.md's table has the merge row.
      NOT IN SCOPE: `Channel.apply`'s default (unknown count, growing
      is right there); the timed flusher's second producer thread.
