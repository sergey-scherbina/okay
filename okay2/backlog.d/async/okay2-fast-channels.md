- [ ] okay2-fast-channels — okay-stream's ring-buffered channels for
      okay2: `SentinelChannel` (termination travelling in the buffer as
      a mark), `Buffer` and its implementations `Ring`, `Segments`
      (unbounded, a linked list of fixed arrays), `Growing` (a ring that
      becomes partitioned when producers contend), `AdaptiveFifo`
      (parts per producer), `AbruptChannel` (drain-on-close traded away),
      `Queues` (the builder: strong/relaxed/adaptive/fifo), `Flush`/
      `Flushing`/`mergeFlushing`, `ParallelChunks`, and `Channel.apply`
      choosing by capacity as the Scala 3 core does (~2 600 lines, every
      mechanism a measured lane there). `TestChannelLaws` in two tiers
      (core, drain) over every implementation is the acceptance; the
      Scala 3 core's laws are the spec. Until then `Channel.apply` is
      `StmChannel`, which is what the contract is defined by. (2026-09-24)
