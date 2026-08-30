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
- Encoders: Spark's TypeTag-reflection derivation does not exist in
  Scala 3, and the Kryo fallback loses both codegen and pushdown — so
  okay-spark derives ExpressionEncoder-grade encoders as a SCHEMA
  ALGEBRA (see codecs.md): the Mirror-derived Schema[T] is folded by a
  Spark algebra into StructType + row (de)serializers, staged at
  compile time into straight-line field access.
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
- [x] kafka over MockConsumer/MockProducer: one poll = one chunk in
      order, commit records the position (restart resumes there —
      at-least-once), sink sends and flushes, Resource closes the
      consumer; a real-broker (testcontainers) round-trip is a later
      hardening step
- [x] spark: the SAME Aggregator value yields equal results run
      locally over Chunks and on a local-mode Spark session (variance
      over 8 partitions, zip, aggregateByKey, the typed Dataset column)
- [x] jdbc: 250 rows stream as 64,64,64,58-row chunks (fetch-size
      windows, constant memory by construction); the Resource region
      closes the connection after a handled abort (tested on H2)
