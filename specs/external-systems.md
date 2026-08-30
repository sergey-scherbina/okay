# P4 — External systems (okay-kafka, okay-spark, okay-flink, okay-jdbc)

## Overview
One small module per system. Ordered by leverage: Kafka first (its
consumer polling is chunked by nature — a poll returns a batch, which
is exactly a Chunk), then the Spark/Flink bridges (nearly free through
the Aggregator triple), then JDBC.

## okay-kafka
- Source: a topic/partition as `Chunks[Record] ! Async` — one poll,
  one chunk; offsets make it a REPLAYABLE source (the P2 capability),
  so per-chunk retry rewinds and re-reads.
- Sink: `Chunks[Record] => Unit ! Async` with Resource-managed
  producer lifecycle; delivery semantics documented (at-least-once
  first; transactions later).
- Consumer lifecycle (subscribe/rebalance/close) under Resource.

## okay-spark / okay-flink
- The bridge is the P1 contract: any `Aggregator[In, Acc, Out]`
  exports (zero, seqOp, combOp) — handed to Spark's `aggregate` /
  `Aggregator` API and Flink's `AggregateFunction` directly. One
  definition, local Chunks execution or cluster execution.
- Datasets/DataStreams as sources into Chunks for local tails of a
  pipeline; no attempt to run okay programs ON the cluster in P4
  (that is okay-cluster's question).

## okay-jdbc
- A query as `Chunks[Row] ! Async` (fetch-size = chunk size), the
  connection/statement/resultset stack under Resource (release in
  reverse — the region does this already); writes batched per chunk.

## Behavior
- [ ] kafka: an embedded/testcontainer round-trip — produce N, stream
      N, chunk sizes follow poll batches; a killed consumer fiber
      resumes from committed offsets (per-chunk retry)
- [ ] spark: the SAME Aggregator value yields equal results run
      locally over Chunks and on a local-mode Spark session
- [ ] jdbc: a large result set streams in constant memory; the
      Resource region closes the stack on abort and on cancellation
