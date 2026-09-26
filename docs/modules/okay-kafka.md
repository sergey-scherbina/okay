# okay-kafka

> One poll, one chunk: Kafka as a chunked async stream whose offsets
> make it the family's canonical REPLAYABLE effectful source.

Depends on: `okay` (JVM), kafka-clients (the pure Java client).

## Guide

**The batch is the chunk.** Kafka already hands you batches — a poll
returns many records. `source(consumer)` emits each poll's batch as
one `Chunk[ConsumerRecord]` in a `Source[Chunk[ConsumerRecord]]` stream
(each batch told; `Chunk ! Produce + Async` until 2026-09-19):
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

### A dataflow epoch larger than a record, exactly once

A stream's staging sink (okay-cluster, `Sink.staging`) hands each
epoch over when it is final, and the obvious writer appends it as ONE
record — which caps an epoch at Kafka's 1 MB. `EpochLog` writes an
epoch of any size as chunks inside ONE transaction, so a
`read_committed` reader sees it whole or not at all; a writer that
dies mid-epoch is fenced by its successor (same transactional id),
whose `initTransactions` aborts the open epoch, and the successor
learns the last committed epoch from the log:

```scala
EpochLogJob.log = EpochLog(bootstrap, topic, s"$topic-writer", chunkBytes = 4096)
```

and the stream's sink moves each epoch into it:

```scala
Wire.tumblingStaged(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)((epoch, panes) =>
  val _ = log.nn.move(epoch, panes.iterator.map(p => pane.encode((p.start, p.key, p.value)))))
```

`move` answers `false` for an epoch already committed — stage 9 may
move one twice across a restart. `EpochLog.epochs(bootstrap, topic)`
reads every committed epoch back. 100 MB in one epoch, its writer
killed half way, is `TestEpochLog`'s Live test (about 8 s on a local
broker).

### A topic as a source of the cluster engine

`KafkaSource` makes a topic's Kafka partitions a job's partitions. The
job's parameters are the topic's spans, taken once at submission —
`[from, until)` per partition — and its flow is the source:

```scala
def flow(s: KafkaSpans, parts: Int): Flow[KafkaRecord] = KafkaSource.flow(s, parts)
```

```scala
val spans = KafkaSource.spans(bootstrap, topic)
val got = Cluster.stream(KafkaSumJob, spans, 4, Vector(dying, Cluster.local, Cluster.local), take).runWith
```

A partition is opened at its position and SEEKS there, so a worker that
dies mid-epoch is replaced by one that reads exactly the records the
coordinator's journal has not folded — `TestKafkaSource` streams a
million records over four partitions with a worker killed mid-epoch and
counts every record once. The positions live in the engine's journal
(fenced, with the fold); nothing is committed to a consumer group. The
job's width must be the topic's. A transactional or compacted topic has
offsets with no record; the source refuses that gap by name, and
`KafkaSource.spans(bootstrap, topic, contiguous = false)` reads it by
skipping records instead of seeking.

## API reference

| member | signature | meaning |
|---|---|---|
| `KafkaChunks[K, V]` | `Source[Chunk[ConsumerRecord[K, V]]]` | the source type (`= Unit ! Writer % Chunk[..] + Async`) |
| `source` | `(consumer, timeout?) => KafkaChunks[K, V]` | one poll, one chunk; parks between |
| `commit` | `(consumer) => Unit ! Async` | commitSync after a processed chunk |
| `sink` | `(producer)(records) => Unit ! Async` | one batch, flushed |
| `managedConsumer` / `managedProducer` | under `Resource` | lifecycle in the region |
| `KafkaSource.spans` / `KafkaSource.flow` | `(bootstrap, topic, contiguous?) => KafkaSpans`; `(spans, parts) => Flow[KafkaRecord]` | a topic as the engine's source, positions seeked |
| `EpochLog(bootstrap, topic, transactionalId, chunkBytes?)` | `.move(epoch, rows): Boolean`, `.committed` | a dataflow epoch of any size in one transaction; a repeat is skipped |

## Gotchas

- Union order is free (`! Writer % W + Async` ≡ `! Async + Writer % W`
  — ACI), but explicit type arguments may be needed when handlers
  take the row apart.
- The mock-based tests import `okay.+` explicitly — satellite scopes
  do not see the core's package-level type aliases without it.
- Rebalance listeners / seek-to-offset recovery are yours to wire;
  the module keeps to the stream discipline.
