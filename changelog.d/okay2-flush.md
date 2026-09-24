## okay2-flush - Flush and ParallelChunks in okay2-stream

okay-stream's Source-level half, which okay2-fast-channels left for
later:
- `Flush.now` and `Flushing[W]`. A plain `Source` already is one, by
  contravariance.
- `Flush.map`, `mergeFlushing`, `eitherFlushing`, and
  `Channel.mergeFlushing` over a program-walking `feedFlushing`.
- `ParallelChunks.parMap` and `retryChunks`, JVM and Native only.

specs/okay2.md stage 36. 9 tests.

Docs: docs/okay2.md section 12.
