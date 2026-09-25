# okay-docs

> The document seam: get/put/delete by key with compare-and-set as
> DATA, bounded queries over declared secondary indexes, per-item
> atomicity — the one new seam specs/data.md allows itself.

Depends on: the core, `okay-codec`, `okay-persist`. Cross-built:
the seam and the own engine (`TopicDocs`) on every platform; the
three foreign engines — `MongoDocs`, `DynamoDocs`, `CassandraDocs`
(packages `okay.docs.mongo`, `.dynamo`, `.cassandra`) — on the JVM
only, which is where the JVM side's extra dependencies come from:
mongodb-driver-sync, the Apache Cassandra java-driver-core,
`okay-blob` (SigV4) and `okay-http`. They were three satellite
modules until docs-adapters-merge (2026-09-23). The two vendor
drivers are `Provided` since ops-docs-vendor-drivers (2026-09-25):
they do not travel to a program that depends on the PUBLISHED
okay-docs, so a program that builds a `MongoDocs` or a
`CassandraDocs` names the driver in its own build, as a JDBC user
names theirs. Inside one sbt build (a `ProjectRef` consumer) they do
still travel through `dependsOn` to a project that depends on
okay-docs itself; okay-ops no longer does (ops-docs-edge): the
`okay_docs_*` counters are rendered here, `Docs.prom`, and handed to
`Ops.router(more = Vector(() => Docs.prom(...)))`.

## Guide

**Cond is the load-bearing part.** `Always | IfAbsent |
IfVersion(v)` — conditional writes are how WithKey and optimistic
concurrency are spelled at this seam, and `PutResult` answers
applied-or-not as data: `Stale` carries what the engine holds NOW,
so a lost-ack retry reconciles without a second read.

**The own posture is the log.** `TopicDocs` folds a compacted
keyed topic: the version IS the record offset (the log hands out
CAS tokens for free), a delete is the tombstone, and a cold node
refolds the same store. `grants` answers Strong — a single fold
has no weaker truth to offer.

**Foreign engines keep their own CAS.** `MongoDocs` maps every
conditional write to ONE server-side operation (find-and-modify, a
filtered delete) — never read-then-write hope; declared index
fields materialize as real Mongo indexes, and a query on an
undeclared field refuses by name rather than scanning.

**No multi-document transactions, deliberately.** A multi-item
change is a journaled sequence of conditional writes — the Durable
policies over CAS, the saga pattern with machinery this stack
already has.

## Engines

Every engine passes the SAME `DocsSuite` contract; the seam's whole
claim is that nothing above notices which one answered. The foreign
ones run it Live (`sbt integrationTest`) against a dockerized server
and skip where none answers.

**`TopicDocs` — the own engine.** A fold of a compacted keyed topic;
see above.

**`MongoDocs` — the first foreign engine** (docs-seam), on
mongodb-driver-sync. Foreign engines keep their OWN CAS: the adapter
maps the seam's conditional writes onto Mongo's native
compare-and-set rather than reimplementing versions above it — the
same honesty as the S3 engine's etags and Kafka's ops.

**`DynamoDocs` — the engine the seam was designed around and had
never met** (docs-dynamo): condition expressions and eventual reads.
No AWS SDK: the adapter speaks DynamoDB's JSON protocol over the one
http client, signed by okay-blob's SigV4 with service `dynamodb`. The
document is CBOR under `d`, the version a number under `ver` advanced
by `ADD ver :one`, and every conditional write is ONE UpdateItem or
DeleteItem carrying a condition expression (`attribute_not_exists(id)`,
`ver = :ver`); a ConditionalCheckFailedException answers `Stale` with
the current version. Declared index fields are `ix_<field>` attributes
and global secondary indexes the query walks. `grants` names the two
read modes DynamoDB has: `One` is an eventually consistent read,
`Quorum` and `Strong` are `ConsistentRead`. Live against
dynamodb-local.

**`CassandraDocs` — the engine where consistency is a dial**
(docs-cassandra), through the Apache java driver: a `Quorum` request
is granted a quorum, `Strong` is ALL, `One` is one — nothing
upgraded, nothing pretended. Every conditional write is ONE
lightweight transaction — `INSERT … IF NOT EXISTS`, `UPDATE … IF ver
= ?`, `DELETE … IF ver = ?` — whose `[applied]` row carries the
CURRENT version when it refused, so `Stale` answers with what holds
now. `Cond.Always` is a bounded read-then-CAS loop (no atomic
increment on a regular column). The document is CBOR under `d`, the
version a bigint under `ver`; declared index fields are `ix_<field>`
columns with a secondary index each. Live against cassandra:5.
