- [ ] dataflow-source-log — stage 11: `Flow.topic` over okay-persist
      partitions that SEEK by epoch, and `Sink.stagingTo(topic)` whose
      append is the commit. Exactly-once from log to log on the
      repository's own primitive; verifiable on one machine. FIRST.
