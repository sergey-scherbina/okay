# okay-persist: the durable log

## Overview

The one persistence primitive this stack has been circling: a named,
partitioned, append-only log of schema-encoded records, durable on
disk, replicable across nodes, with offsets as resume tokens. Three
backlog items already need exactly this shape and none of them names
it: the `Durable` journal in okay-agent (memory-only today, its own
doc comment promising "a file or a table behind the same three
methods"), `ui-durable` (event-sourced sessions: journal intent-first,
recover by refold, snapshot by writing S), and `mcp-resumable-sse`
(a server-side event journal addressed by `Last-Event-ID`). A fourth
— okay-rag's `Persist` — produces the bytes and washes its hands of
where they go.

The requirement this module answers is the business one, stated up
front rather than discovered later: scaling, partitioning and
sharding, replication, fault tolerance, manageability, monitoring,
schema evolution and versioning. The design is therefore laid out to
its full extent HERE, and built in stages — each stage a working
system, none of it a dead end the next stage has to dig out of.

The foundation is already settled and is not relitigated here
(specs/llm-agentic.md, "Durability: replay, not serialization"): the
journal is the foundation — correctness, any program shape, the full
history — and a snapshot is an OPTIMIZATION that bounds how much of
it is replayed. This module is that decision made physical: the
journal gets a disk, the disk gets partitions, the partitions get
replicas.

## The model

Four nouns, from the bottom up:

- **Record** — what the log stores: `(offset, timestamp, key, value)`.
  Key and value are BYTES; `Schema`/CBOR live at the edge, not in the
  engine (the same layering as okay-rag's `Persist`: one derived
  Schema serves CBOR to ship and JSON to look at). The offset is
  assigned by the log, dense within a partition, and is THE resume
  token — the `Durable` seq, the SSE `Last-Event-ID`, and the UI
  refold position are all this number.
- **Partition** — the unit of ORDER, of durability, and of
  replication. An append goes to one partition; offsets are ordered
  and dense within it and mean nothing across partitions. Everything
  that scales, scales by adding partitions; everything that is
  guaranteed, is guaranteed inside one.
- **Topic** — a named set of partitions plus a policy (retention,
  compaction, replication factor). Sharding is `hash(key) mod
  partitions`: the same key always lands in the same partition, so
  per-key order is free and a consumer of one partition sees every
  event for the keys it owns. One session = one key is the ui-durable
  story; one run = one key is the agent story.
- **Store** — a named collection of topics behind one small trait,
  with the engine (memory, file, replicated, interop) chosen at
  construction and invisible above it.

## Interface

Traits, not effect rows. In this stack an effect is what a PROGRAM
requests of the world; the log is infrastructure a HANDLER owns —
`Durable.tools(inner, journal)` already takes its journal as a plain
argument, and that precedent is right. Programs stay written against
their own effects; the store appears where handlers are built.

One deliberate asymmetry, recorded since the 2026-09-01 audit:
`Topic` is SYNCHRONOUS where the newer seams (`Sql`, `Docs`,
`Cache`) speak `Async`. `Topic` is the local ENGINE SPI — memory
and files, where an async signature would be theater — and
everything that crosses a wire wraps it: the streaming/tailable
readers (stage 1) and the remote client (persist-wire) speak Async
at their own layer. An engine is not an access path; specs/sql.md's
async-in-the-trait decision governs access paths.

```scala
package okay.persist

/** what the log stores; key may be empty (unkeyed append) */
final case class Record(offset: Long, timestamp: Long,
                        key: Array[Byte], value: Array[Byte])

/** the durability DECISION, per append — a guarantee is not on offer,
 *  a named choice is (the Durable.OnRepeat pattern):
 *  Received  = in the leader's memory, fastest, lost on crash
 *  Durable   = on the leader's disk (fsync'd)
 *  Replicated = on a quorum of replicas' disks */
enum Ack { case Received, Durable, Replicated }

/** one topic, already resolved; all methods address one partition */
trait Topic:
  def partitions: Int
  def append(partition: Int, key: Array[Byte], value: Array[Byte],
             ack: Ack): Long                          // the offset
  def read(partition: Int, from: Long, max: Int): Topic.Read
  def end(partition: Int): Long                       // next offset
  def begin(partition: Int): Long                     // first retained

object Topic:
  /** a read names its failure instead of returning silence: asking
   *  for history that retention dropped is an answer, not an empty
   *  vector pretending nothing was ever there */
  enum Read:
    case Records(records: Vector[Record])
    case TooEarly(begin: Long)

trait Store:
  def topic(name: String, partitions: Int = 1,
            policy: Policy = Policy.default): Topic
  def topics: Vector[String]
  def stats: Store.Stats      // plain values, see Operations
```

Conveniences over the core, not part of it: `append(key, value)`
routing by key hash; a chunked streaming read
`Chunk[Record] ! (Produce + Async)` in the `JdbcInterop` shape
(each chunk one `Async` operation, constant memory for any log
size); a typed view `Topic.of[A](using Schema[A])` that encodes and
decodes at the edge and returns damage as data, never throws.

`Policy` names the segment roll size, retention (by size, by age, or
forever), compaction (off, or keep-latest-per-key), and — from
stage 2 — the replication factor. All of it per topic, because an agent journal (forever,
compact never) and a metrics stream (24h, who cares) are both honest
topics.

## Storage engine

Per partition, the shape databases and Kafka both settled on, because
it is what a disk is good at:

- **Segments**: append-only files, rolled at a size bound. A segment
  starts with a header — magic, FORMAT VERSION, topic, partition,
  base offset — so a segment is self-describing and the on-disk
  format can evolve under a version check rather than a guess.
- **Frames**: length-prefixed record with a CRC32C. On recovery the
  tail is scanned from the last index point; a frame whose length or
  CRC does not check out ENDS the log there — damage truncates
  rather than throws, the rule everywhere else in this stack
  (`Persist.unpack`, the cluster's dropped frame). A torn tail is
  the normal crash artifact, not an exception. From format v2 a
  frame carries its OFFSET explicitly (v1 derived offsets as base
  plus position, which only works while offsets are dense) —
  compaction punches holes in the sequence, so the frame must say
  which record it is. The engine writes v2 and reads both.
- **Index**: sparse offset→file-position entries per segment,
  rebuilt from the segment on recovery if missing or damaged — the
  index is a cache, never a source of truth.
- **fsync** is the `Ack` decision made physical: `Received` returns
  before the write syncs, `Durable` after. Group commit batches
  concurrent `Durable` appends into one sync.
- **Retention** deletes whole segments from the front; `begin`
  moves. **Compaction** (keyed topics) rewrites old segments keeping
  the latest record per key — which is exactly the snapshot story:
  a fold that writes its state S under its key makes the compacted
  topic BE the snapshot, and refold starts from `begin`, not zero.
  Snapshotting stays an optimization a consumer opts into by
  writing a value, precisely as the settled decision requires.
  The two are exclusive per topic: `Policy.compact` switches
  size/age retention OFF — dropping whole segments from the front
  of a compacted topic would delete the latest value of every key
  that went quiet, the exact records compaction exists to keep.
  Compaction preserves offsets and `begin`, changes no record, and
  runs as an explicit `compact(partition)` call (the force-compact
  admin call of Operations); the active segment is never compacted.
  Crash-safety is the atomic-rename shape: survivors are written to
  a temporary file, fsync'd, renamed over the head segment, and
  only then are the superseded closed segments deleted — a crash
  in the window leaves segments whose records reads already skip,
  because a read serves offsets monotonically and every survivor
  carries a later or equal offset than what the leftovers hold.

`MemoryStore` implements the same trait for tests and for short
interactive runs where the journal is overhead (the llm-agentic spec:
durability is switched on for a run, not paid always).

## Delivery semantics

Stated, not promised away. Exactly-once EXECUTION does not exist
(Durable.scala says why: the crash window between "request left" and
"answer recorded" is not locally distinguishable); what exists is
at-least-once delivery plus idempotent outcomes:

- **Producer**: an append retried after a lost ack may duplicate. A
  producer that cares registers `(producerId, seq)`; the partition
  remembers the last seq per producer within a window and drops
  replays — the retry carries the SAME identity, `Durable.WithKey`
  one layer down.
- **Consumer**: a consumer commits its offset AS A RECORD to an
  offsets topic (the log's own machinery; no second store to make
  durable). Commit after processing = at-least-once, before =
  at-most-once; the consumer picks per its own idempotency, and the
  spec says so instead of selling "exactly-once delivery".
- **Journal contract**: `Durable.Journal.complete` becomes an
  APPEND (a completion record referencing the intent's offset), and
  `all` a fold of the partition. The in-place update
  `MemoryJournal.complete` performs cannot survive an append-only
  disk, and should not: intent-first only means anything if the
  intent physically precedes the answer.

## Replication and fault tolerance

Designed now, built as stage 2 — on the parts okay-cluster already
proved (CBOR frames over remote channels, one source for both ends,
a dead peer THROWS and that is the whole protocol):

- **Unit**: the partition. Each has one LEADER and N-1 FOLLOWERS
  (`Policy.replicas`); appends go to the leader, followers pull in
  offset order — replication is just a consumer that writes what it
  reads, which is why the log is the right primitive to replicate.
- **High-water mark**: the highest offset acknowledged by a quorum.
  `Ack.Replicated` returns at quorum; reads never serve past the
  high-water mark, so a reader cannot observe a record that a
  failover will unwrite.
- **Epochs**: leadership carries an epoch number, monotonic per
  partition, stamped on every replicated frame. A deposed leader's
  appends are FENCED by epoch — the split-brain answer, and the
  reason failover can be manual before it is automatic.
- **Membership and election, honestly**: okay-cluster today has
  static configuration and explicitly defers consensus. So stage 2
  ships replication with STATIC assignment and operator-driven
  failover (an admin call: promote follower F at epoch e+1 — safe
  because epochs fence and the high-water mark bounds loss to
  nothing acknowledged). Automatic election needs consensus, which
  is a real project and gets its own spec (stage 4) rather than a
  paragraph pretending Raft is a detail. Nothing in stages 0–2
  bakes in an assumption stage 4 has to break: election changes who
  MAY write an epoch, not what an epoch is.
- **The interop hatch, because business does not wait for stage 4**:
  the `Store` trait is also implementable over engines that already
  did the twenty years of work — okay-kafka's interop (partitions,
  replication, election all inherited), a JDBC table per the
  `Journal` doc-comment's promise, object storage for cold
  segments. Same seam as the P4/P7 decision this repo already made
  once (specs/cluster.md: bridges AND our own runtime, one
  interface): a deployment that needs multi-node durability today
  runs `Store` over Kafka; the own engine grows under the same
  trait and the consumers never hear about it.

## Evolution and versioning

Three separate things that version, each with its own rule:

- **Record values**: `Schema` evolution rules already in force in
  okay-codec (optional fields decode absent; damage is data). On
  top, an ENVELOPE convention for journal-grade topics: a small
  version int beside the payload (concretely, the `Typed` view
  writes a four-byte big-endian version before the CBOR bytes);
  readers upcast old versions through pure `v→v+1` functions at
  decode — byte-level `payload(v) => payload(v+1)` steps, with a
  `Typed.step` helper lifting an `Old => New` function over two
  Schemas into one. A version the reader
  does not know is an explicit error value, not a crash — the
  Durable fingerprint lesson: drift is caught loudly at the exact
  record, not fed to the wrong code.
- **The disk format**: the segment header's format version. A newer
  engine reads every older segment version it ever shipped
  (migration = rewrite segments at leisure, or just let retention
  age them out); an older engine refuses a newer segment loudly.
- **The wire** (stage 2): replication frames carry the protocol
  version in the handshake, the same discipline as okay-cluster's
  frames.

## Operations: manageability and monitoring

A system a business runs is one an operator can SEE and STEER, so
this is interface, not afterthought:

- **Stats are values**: `Store.stats` returns a plain case class
  tree — per topic/partition: begin/end offsets, bytes, segment
  count, last append timestamp; per consumer group: committed
  offset and LAG (end minus committed, THE number that says a
  consumer is drowning); per replica (stage 2): follower offset and
  replication lag, current epoch, leader. A value with a Schema, so
  it serves equally as a JSON endpoint (okay-http, one route), a
  log line, or a test assertion. No metrics framework dependency;
  an exporter is a consumer of a value.
- **Ops events are a topic**: leader changes, truncations, segment
  rolls, fencing rejections append to a meta topic — the log is its
  own audit trail, and "what happened at 3am" is a `read`, not a
  grep across machines.
- **Admin is the trait, plus hygiene**: create/describe topics on
  `Store`; truncate-to-offset, force-compact, offset reset for a
  group, promote-follower (stage 2) as explicit admin calls —
  each an appended ops event, so administration is itself
  journaled. An okay-http admin surface is a thin route over these
  calls, in the module only as a satellite (okay-persist stays
  server-framework-free).
- **Health**: liveness = the meta topic accepts an append;
  readiness = every partition this node leads is open and past
  recovery. Two booleans with reasons, as values.

## Backup and restore (stated, since the design already implies it)

Append-only makes backup boring, which is the point:

- a CLOSED segment never changes, so incremental backup is copying
  new segment files — to an object store (specs/blob.md) or plain
  rsync; the active segment joins next round, after it rolls.
- RESTORE is placing files back and letting recovery scan them —
  the same code path as every startup, so restore is exercised by
  the ordinary test suite daily, not by an incident yearly.
- POINT-IN-TIME is truncation: restore, then truncate to the last
  offset before the damage; a snapshot copy bounds how much
  history a restore must replay (the WAL+checkpoint shape, told a
  third time).
- a DOCTOR tool (filed: persist-backup) runs the recovery scan
  against a BACKUP — CRCs, header chains — answering "is this
  backup restorable" before anyone needs it to be.

Replication (stage 2) is availability, not backup: an epoch fence
protects against split-brain, not against `rm -rf` or a bad
deploy writing garbage — the copies above answer those.

## Staging

Each stage lands alone, tested, useful; consumers bind to the trait
at stage 0 and never rebind.

- **Stage 0 — the trait and the honest engines** (this claim):
  `Record`/`Ack`/`Topic`/`Store`, `MemoryStore`, and the
  single-node file engine: segments, CRC framing, torn-tail
  truncation, recovery, retention by segment. JVM first; the trait
  and `MemoryStore` are cross-platform (JS gets memory now, a Node
  `fs` engine when something needs it).
- **Stage 1 — the consumers prove the seam**: `Durable.Journal`
  over a keyed topic (complete-as-append); mcp-resumable-sse
  reading by `Last-Event-ID`; ui-durable journaling its session
  fold. Plus keyed routing, the typed `Schema` view, streaming
  reads, consumer offsets, compaction (= snapshots).
- **Stage 2 — out of one machine**: replication (static
  assignment, epochs, high-water mark, quorum acks,
  operator-driven failover), stats for replicas, the wire
  protocol. Idempotent producer window. The same frames serve a
  REMOTE `Topic` client (persist-wire): a Native or Node consumer
  reaches a persist node directly — no JVM, no JDBC in between.
  Openness is a stated property: the segment format and the wire
  are documented surfaces, not internals. The wire AUTHENTICATES
  via specs/security.md (bearer / API key) with per-topic
  capabilities — a client is offered only the topics it may see,
  the ui rule ("the tree is the capability list") retold for logs
  — and encrypts via the one transport seam, specs/tls.md.
- **Stage 3 — interop engines**: `Store` over Kafka via okay-kafka;
  JDBC-backed topic; segment offload to object storage.
- **Stage 4 — elected leadership**: consensus, its own spec, its
  own honest comparison (Raft vs delegating election to a
  stage-3 engine that has it).

## Behavior

- [x] append then read returns the records in order with dense
      offsets; `end` is the next offset, `begin` the first retained
      (file + memory)
- [x] poll-on-end: a reader that has consumed to `end` and reads
      again from there sees an append made after its first read —
      the contract tailing stands on (ui-durable, resumable SSE),
      tested in both engines rather than assumed by the consumer
- [x] a process killed between append and ack leaves the partition
      readable: the record is either wholly present or wholly
      absent, never a corrupt log (torn frame truncates on recovery)
- [x] recovery after a torn tail serves every earlier record intact,
      and the next append continues the offset sequence (dense over
      restart)
- [ ] a damaged index is rebuilt from segments; log content decides,
      never the index (index deleted between runs; reads agree)
- [x] keyed appends land deterministically: same key, same
      partition, order preserved per key across concurrent writers
      (routing pure and platform-stable; interleaved writers)
- [x] retention drops whole segments from the front; `begin`
      advances; reading from before `begin` says so explicitly
      rather than returning silence
- [ ] compaction keeps the latest record per key; a refold from
      `begin` of a compacted topic equals the fold of the full
      history (stage 1)
- [ ] `Durable.Journal` over a topic: intent and completion are
      separate records; recovery folds them; the crash-window entry
      (intent, no completion) surfaces for the policy exactly as
      MemoryJournal does today (stage 1)
- [ ] a consumer commits offsets to the offsets topic and resumes
      from its commit after a restart — the Last-Event-ID shape
      (stage 1)
- [ ] an append retried with the same (producerId, seq) after a
      lost ack lands once (stage 2)
- [ ] a follower serves reads only up to the high-water mark; an
      `Ack.Replicated` append returns only at quorum (stage 2)
- [ ] a deposed leader's append is rejected by epoch fencing, and
      the rejection is an ops event (stage 2)
- [ ] a record written with schema v1 reads under v2 through the
      upcast; an unknown version is an error value naming the
      offset, not a throw (stage 1)
- [x] an older engine refuses a newer segment format loudly
      (forged newer header refused, naming the file and both
      versions; the reads-the-older-format half becomes testable
      only when a v2 exists)
- [x] `Store.stats` reports begin/end/bytes/segments per partition
      (consumer lag: stage 1; replica lag: stage 2)

## Out of scope

- transactions (atomic multi-partition writes, read-committed) —
  the consumer-side idempotency story covers the named consumers;
  revisit only with a concrete case in hand
- queries and secondary indexes — it is a log; an index is a
  consumer that builds one (okay-rag is literally this already)
- multi-region/geo-replication; encryption at rest (file-system
  concern until a requirement names it)
- automatic partition splitting — partition count is set at topic
  creation in stages 0–2; re-partitioning is a copy into a new
  topic, stated plainly
- being a cache — reads are for recovery, resumption and audit;
  hot-path state lives in the fold's memory, which is the whole
  event-sourcing point

## Decisions

- **A log, not a key-value store** — every named consumer (Durable,
  ui-durable, resumable SSE, offsets, ops audit) needs ordered
  history with resume points; KV is the DERIVED thing (compaction =
  keep-latest-per-key). Rejected: a `get/put` trait (loses order,
  which is the one property everything here leans on; and a KV
  store cannot express intent-first).
- **Traits, not an effect row** — the store is handler
  infrastructure, not something programs request; `Durable.tools`
  set the precedent. Rejected: `Persist[A]` effects in signatures
  (abstraction tax on every consumer for a capability none of them
  wants to vary per operation).
- **Bytes in the engine, Schema at the edge** — the engine must not
  care what it stores or it can never be generic over consumers;
  okay-rag's `Persist` proved the layering and `SBytes` exists
  because of it. Rejected: `Topic[A]` all the way down (couples
  disk format to Scala types; kills mixed-schema topics like the
  ops stream).
- **Partition = the unit of order, durability, replication** — one
  concept scales, shards, and replicates, instead of three;
  per-key order falls out of hashing. Rejected: global total order
  (a single writer bottleneck by construction — the thing that
  cannot ever scale out).
- **Replication = a consumer that writes what it reads** — no
  second machinery: followers use the read path, epochs and the
  high-water mark are the only additions. Rejected: state transfer
  of snapshots (re-imports the serialized-continuation problem the
  settled decision already rejected).
- **Static assignment before consensus, interop before both** —
  stage 2 is useful (durability across two nodes, fenced manual
  failover) without pretending election is easy; Kafka behind the
  same trait serves the business that cannot wait. Rejected:
  Raft-first (months of engine work before the first consumer
  benefits); Kafka-only (the P7 decision — bridges AND our own —
  is repo precedent, and a dependency this central deserves an
  exit).
- **complete-as-append** — in-place update cannot survive an
  append-only disk and quietly breaks intent-first; a completion
  record referencing the intent keeps the journal's three states
  readable off one fold. Rejected: mutating the intent record
  (would require an update-in-place engine feature that exists
  only to weaken the model).
- **Stats as plain values with Schemas** — observable via one
  okay-http route, testable as equality; no framework dependency.
  Rejected: a metrics library (a dependency and a push model for
  what is naturally a pulled value).
- **Frames carry their offset from format v2** — compaction makes
  offsets sparse, so base-plus-position stops being an offset; the
  eight bytes per record buy the property everything above resumes
  on. The v1 read path stays, which also makes the evolution
  promise ("a newer engine reads every older segment version it
  ever shipped") testable rather than aspirational. Rejected:
  a separate format for compacted segments only (two write paths,
  and a reader that must know which kind it holds).
- **Dropped history stops a stream by declared decision** — the
  streaming readers take an `OnTooEarly` argument (`Fail` throws
  naming `begin`, `Resume` continues from it), the
  decision-not-promise pattern once more: a tailing consumer whose
  history aged out must choose between loud failure and a stated
  jump, not receive one silently. Rejected: silently starting at
  `begin` (the exact silence `Topic.Read.TooEarly` exists to
  forbid).

## Results

Stage 0 landed.

- **Module**: `okay-persist`, cross-built JVM/JS/Native
  (CrossType.Pure, depends on okay-codec for stage 1's typed view).
  Shared: `Record`, `Ack`, `Policy`, `Topic` (+ `Topic.Read`,
  FNV-1a key routing), `Store`, `MemoryStore`. JVM only:
  `FileStore` (java.nio). JS and Native run the shared contract
  suite over `MemoryStore`; a file engine there waits for a
  consumer that needs one.
- **Format**: segment header (magic `OKPS`, format v1, topic,
  partition, base offset); frames `[len][crc32c][ts][keyLen][key]
  [value]`, CRC over the body. Recovery scans the last segment,
  truncates the torn tail, continues appending at the last good
  frame — the never-acknowledged offset is reused, densely.
- **Tests**: 23 JVM (the 8-test engine contract — order, density,
  poll-on-end, routing, retention/TooEarly, stats — against memory
  AND file, plus 7 file-only: reopen, torn tail, CRC damage, format
  refusal, segment roll + retention, multi-segment reads, disk
  topic listing), 8 JS, 8 Native — green.
- **Deferred, deliberately**: the sparse offset index — reads scan
  within one segment, bounded by `segmentBytes`; measure before
  adding the cache (the index is designed above as a rebuildable
  cache precisely so it can arrive late). Benchmarks land with the
  first real consumer (stage 1), where an honest workload exists
  to measure; the numbers row goes to `src/jmh/history.tsv` as
  usual.
- **For the next agent**: the seam agreed with the ui/mcp lane —
  their journal is a topic (offset = Seq = Last-Event-ID), their
  snapshot store is a compacted keyed topic; a thin `Snapshots`
  put/latest convenience is wanted in stage 1.
