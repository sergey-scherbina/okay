# okay-persist

> The durable log: a named, partitioned, append-only log of records
> with offsets as resume tokens — the one persistence primitive the
> `Durable` journal, event-sourced UI sessions and resumable SSE all
> turn out to be. Designed to its distributed extent, built in
> stages (specs/persist.md).

Depends on: `okay-codec` (the typed view and stats rides Schema).

## Guide

**Bytes in the engine, Schema at the edge.** The engine stores
`Record(offset, timestamp, key, value)` with key and value as bytes;
what they mean belongs to the consumer and its Schema. Traits, not
effect rows: the log is infrastructure a handler owns —
`Durable.tools(inner, journal)` set the precedent.

**The partition is the unit.** Of order, of durability, of
replication. Offsets are dense within a partition; the keyed
`append` routes by a platform-stable FNV-1a hash, so the same key
always lands in the same partition and per-key order is free. One
session = one key (ui-durable), one run = one key (the agent).

**Durability is a decision, not a promise.** `Ack.Received` (in
memory), `Ack.Durable` (fsync'd), `Ack.Replicated` (a quorum,
stage 2) — chosen per append, the `Durable.OnRepeat` pattern one
layer down.

**Reads are total.** `Topic.Read.Records` or
`Topic.Read.TooEarly(begin)` — history dropped by retention is an
answer, not an empty vector; damage on disk truncates rather than
throws (a torn tail is the normal crash artifact, and recovery
truncates it and appends over the never-acknowledged offset).

**Engines.** `MemoryStore` (cross-platform: tests, and short runs
where a journal is overhead) and `FileStore` (JVM): segment files
with a self-describing versioned header, CRC32C-framed records,
torn-tail truncation on recovery, retention by whole segments. A
newer segment format is refused loudly; an unknown one is never
guessed at.

**Observability is a value.** `Store.stats` — begin/end offsets,
bytes, segment counts per partition — a plain case class, so an
endpoint, a log line and a test assertion are the same thing.

The staged roadmap — streaming/tailable reads, compaction (which IS
the snapshot store), consumer offsets, `Durable.Journal` over a
topic, replication with epochs and a high-water mark, interop
engines (Kafka/JDBC) behind the same trait — lives in
specs/persist.md with its decisions and refuted alternatives.
