- [ ] staging-large-epochs — exactly-once output larger than one record
      (operator, 2026-09-26). The durable staging writer appends ONE record
      per epoch (specs/dataflow.md stage 9), so an epoch must fit in one —
      measured ~64 KB, against Kafka's 1 MB default — and a scoring job
      whose epoch emits many rows cannot use it. `Sink.stagingTo(topic)` is
      deliberately absent (`[~]`). Needs: an epoch written in chunks under a
      Kafka transaction (or chunked with a commit marker a reader honours),
      committed at the epoch's commit. Gate: an epoch of 100 MB of results
      staged to a Live Kafka topic, the coordinator killed between chunks,
      a reader with `read_committed` sees each epoch whole or not at all.
