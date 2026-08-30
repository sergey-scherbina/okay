# okay-kafka

> One poll, one chunk: Kafka as a chunked async stream whose offsets
> make it the family's canonical REPLAYABLE effectful source.

Depends on: `okay` (JVM), kafka-clients (the pure Java client).

## Guide

**The batch is the chunk.** Kafka already hands you batches — a poll
returns many records. `source(consumer)` emits each poll's batch as
one `Chunk[ConsumerRecord]` in a `Chunk ! (Produce + Async)` stream:
between chunks the virtual thread parks inside `poll` (Loom-style
waiting — no callback plumbing), and empty polls are simply not
emitted.

**At-least-once by construction.** `commit(consumer)` after a
processed chunk commits its offsets. A supervised consumer (P2's
`supervised` applies as-is) restarted after a crash re-reads only the
uncommitted tail: offsets are exactly the REPLAYABLE-source
capability specs/parallel-resilience.md gates chunk-retry on — here
the broker holds the lineage instead of a pure program value.

**Lifecycles under Resource.** `managedConsumer`/`managedProducer`
put the clients under the Resource region: closed at the scope's
end, on handled aborts, on exceptions.

## Tutorial

```scala
import okay.kafka.KafkaInterop.*

Resource.run:
  for
    c <- managedConsumer[String, String](props, topics)
    _ <- !.widen(processChunks(source(c)))   // pull, fold, whatever
  yield ()

// process-then-commit = at-least-once:
def processChunks(s: KafkaChunks[String, String]) =
  eachChunk(s) { chunk =>
    handle(chunk)
    commit(c)
  }

// producing: one chunk, one batch, flushed
sink(producer)(records)
```

Tests run over `MockConsumer`/`MockProducer` — no broker, no
testcontainers: `assign` + `updateBeginningOffsets` + `addRecord`,
and `MockProducer(...).history` on the way out.

## API reference

| member | signature | meaning |
|---|---|---|
| `KafkaChunks[K, V]` | `Chunk[ConsumerRecord[K, V]] ! (Produce + Async)` | the source type |
| `source` | `(consumer, timeout?) => KafkaChunks[K, V]` | one poll, one chunk; parks between |
| `commit` | `(consumer) => Unit ! Async` | commitSync after a processed chunk |
| `sink` | `(producer)(records) => Unit ! Async` | one batch, flushed |
| `managedConsumer` / `managedProducer` | under `Resource` | lifecycle in the region |

## Gotchas

- Union order is free (`! (Produce + Async)` ≡ `! (Async + Produce)`
  — ACI), but explicit type arguments may be needed when handlers
  take the row apart.
- The mock-based tests import `okay.+` explicitly — satellite scopes
  do not see the core's package-level type aliases without it.
- Rebalance listeners / seek-to-offset recovery are yours to wire;
  the module keeps to the stream discipline.
