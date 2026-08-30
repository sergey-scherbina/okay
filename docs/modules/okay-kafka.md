# okay-kafka

One poll, one chunk.

- `source(consumer)` — `Chunk[ConsumerRecord] ! (Produce + Async)`:
  each emitted chunk is one poll's batch; between chunks the virtual
  thread parks in `poll`; empty polls are not emitted.
- `commit(consumer)` after a processed chunk = at-least-once; a
  supervised consumer restarted from committed offsets re-reads only
  the uncommitted tail — offsets are what make the source REPLAYABLE
  (the P2 capability).
- `sink(producer)(chunk)` — one batch per program, flushed.
- `managedConsumer` / `managedProducer` — lifecycles under the
  Resource region.

Tests run over MockConsumer/MockProducer — no broker needed.
