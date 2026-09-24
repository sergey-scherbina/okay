- [ ] okay2-flush — the Scala 3 core's `Flush` effect for okay2-stream:
      `Flush.now`, `Flushing[W]` (`Unit ! (Flush + (Writer % W + Async))`),
      `mergeFlushing`, `mapFlushing`, and `ParallelChunks` — the
      Source-level half of what `okay2-fast-channels` listed, split off
      when that lane landed the mechanisms (2026-09-24): they are an
      effect over sources, not a channel mechanism, and the channels
      they feed are now the Scala 3 ones. okay-stream's Source.scala and
      Channel.scala (`mergeFlushing`, `feedFlushing`) are the spec.
