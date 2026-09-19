# okay-kafka

One poll, one chunk: Kafka as a chunked async stream whose offsets make it the family's canonical REPLAYABLE effectful source.

**Depends on:** `okay` (JVM), kafka-clients (the pure Java client).

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-kafka.md`](../docs/modules/okay-kafka.md) | what it is, and the reasoning |
| [`specs/parallel-resilience.md`](../specs/parallel-resilience.md) | the design and its decisions |
