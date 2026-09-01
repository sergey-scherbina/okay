# okay-persist

> The durable log: a named, partitioned, append-only log of records
> with offsets as resume tokens — the one persistence primitive the
> `Durable` journal, event-sourced UI sessions and resumable SSE all
> turn out to be. Designed to its distributed extent, built in
> stages (specs/persist.md).

Depends on: `okay-codec` (the typed view and stats ride Schema) and
the core (the streaming reads speak `Chunk ! (Produce + Async)`).

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

**Stage 1 — the consumers' toolkit.** `Topic.compact(p)` keeps the
latest record per key (offsets stay, holes appear; a compacted
topic never retains away — the two are exclusive). `Topic.of[A]`
is the typed view: a four-byte version envelope over CBOR, byte-
level `v→v+1` upcasts (`Typed.step`), and every failure an explicit
`Decoded.Bad(offset, error)`. `Offsets` commits consumer positions
as records to a compacted topic and refolds them on restart — the
Last-Event-ID shape; `lag` is the drowning number. `Snapshots` is
put/latest over a compacted keyed topic (the ui lane's refold
anchor). `Streams.stream`/`Streams.tail` read chunked —
`Chunk[Record] ! (Produce + Async)`, a caught-up tail parks on the
platform timer — and dropped history stops a stream by declared
`OnTooEarly` decision, never silently. In okay-agent,
`TopicJournal` is `Durable.Journal` over a keyed topic: intent and
completion as separate records, recovery as a refold.

**Stage 2 — replication, transport-agnostic.** `Replicated` is a
coordinator over N replica Stores behind the same Topic trait:
the follower push/pull IS the read path, the high-water mark (the
quorum-th largest replica end) bounds what any reader can see,
`Ack.Replicated` returns only at quorum (NoQuorum otherwise), the
Leader handle fences deposed epochs, promote catches the
successor up before it leads, and `produce(producerId, seq, ...)`
is the idempotent window. Promotions and fenced appends land on
the ops topic — the log audits itself.

**The wire.** `Wire.Server(store, auth)` serves any Store over
documented `[len][CBOR]` frames; `Wire.Remote.connect(host, port,
token)` answers the GRANTED topic set — the capability list is
the offer — and Async programs for append/read/begin/end.
Refusals are by name, TooEarly crosses unchanged, and auth is a
function okay-security plugs into. Plaintext until wire-tls,
stated.

Landed since: stage 2's core (Replicated — hwm as the visibility
bound, quorum acks that refuse honestly, epoch-fenced leadership,
the producer window), stage 3 interop engines (SqlStore over the
Sql seam, KafkaStore inheriting the twenty years), and the
conveniences that ride the one primitive:

| | |
|---|---|
| `Snapshots` | a compacted keyed topic as the checkpoint store; refold from the snapshot's offset |
| `Offsets` | consumer positions as commits on a compacted topic |
| `Configs` | managed config (specs/conf.md stage 2): put/latest/at/history — the audit IS the log, rollback IS a read |
| `Doctor` | "is this backup restorable", answered offline by an INDEPENDENT reader of the documented segment format: a torn tail on the LAST segment is normal and named; damage in a CLOSED one condemns the copy |

Backup/restore live on the blob side (`okay.blob.Backup` — the
dependency arrow persist→blob would cycle through http): closed
segments copy incrementally, restore is placing files back for the
ordinary startup path. Elected leadership stays specced
(persist-raft) with its decisions and refuted alternatives.
