# The data landscape: two postures, few seams

## Overview

The ask, stated plainly: NoSQL, OLAP, warehouses (Snowflake-class),
data lakes, vector stores, Kafka, Spark — everything a business
actually runs against — supported "по-взрослому", with scaling and
reliability, in TWO MODES everywhere: a client's system we are
restricted in (read-only or limited writes, no creating our own
tables, transactionality mandatory where it exists) and our own
system where we choose.

The answer that keeps this from becoming twenty vendor modules: the
landscape is classified by ACCESS SHAPE, not by product name, and
one rule governs every product: **a vendor enters the codebase only
as an implementation of an existing seam — never as a new API
surface.** The seams already exist, built one per shape:

| Shape | Seam | Where |
|---|---|---|
| rows + SQL | the `Sql` seam, typed by Schema (drivers: JDBC, pg wire, R2DBC hatch) | specs/sql.md, specs/jdbc.md |
| ordered events | `Topic`/`Store` (the log) | specs/persist.md |
| key → document | `Docs` — the ONE new seam this spec adds | below |
| vector search | `VectorStore` | specs/rag.md |
| cached view | `Cache`/`View` | specs/cache.md |
| heavy compute (aggregation) | the `Aggregator` triple bridges | specs/external-systems.md |
| foreign compute (call-shaped: R, Python) | `REval`/`PyEval` operations + handler | specs/r.md, specs/py.md |
| per-message-ack queues | ingress/egress bridges to topics (no seam) | specs/data.md, Queues |
| files / objects | `Blob` | specs/blob.md |
| config/secrets | `Conf`/`Secrets` | specs/conf.md |

Every class below gets the same five questions answered: which seam;
the FOREIGN posture (their system, our restrictions); the OWN
posture; the transactions/consistency honesty; the scaling story.
Cross-cutting and non-negotiable everywhere: credentials per
specs/conf.md (references, handler construction only), observability
as plain values with Schemas, damage as data, and every
at-least-once edge paired with an idempotency story at the far end
(the Durable policies — this spec applies them a fourth, fifth and
sixth time rather than inventing anything).

## The two postures, defined once

- **FOREIGN**: the engine is theirs. No DDL, possibly read-only,
  rights end where the DBA says. Our obligations: `verify`-class
  drift checks at startup wherever metadata exists; transactions
  used WHERE THE ENGINE HAS THEM and their absence stated where it
  does not (no pretending); idempotent writes via the engine's OWN
  constraint machinery (unique keys, conditional writes, load ids);
  their scaling is their offer — we consume it honestly (streaming
  reads at constant memory, replayable sources where offsets
  exist).
- **OWN**: the log is the source of truth and engines are
  MATERIALIZATIONS — a queryable table, a document view, a vector
  index are consumers folding topics (the cache-view/Snapshots
  machinery). Scaling and reliability are the persist staging
  (partitions now, replication stage 2, interop engines stage 3);
  a specialized engine (warehouse, vector store) joins as a
  materialization target when the workload outgrows what we serve,
  and the topic it folds from does not change.

## The classes

### Relational OLTP — settled

specs/jdbc.md is this class, both postures, verbatim. Anchor here
only for the table above.

### Kafka and stream systems — settled in two halves, one gap

Foreign: okay-kafka already has the honest shape — a poll is a
chunk, offsets make the source REPLAYABLE (the
parallel-resilience capability), commit-after-chunk is
at-least-once said out loud. Own: okay-persist IS this shape
(deliberately), and its stage 3 names `Store` over Kafka for
deployments that need Kafka's scale under our seam.
The gap worth filing: the PRODUCER side's idempotence/transactions
(exactly-once into their cluster) — `kafka-eos` in the backlog;
until then the sink is at-least-once and consumers dedup, which the
interop's doc already states.

### Key/value and document NoSQL (Mongo, DynamoDB, Cassandra) — the one new seam

The access shape SQL does not cover: get/put/delete by key,
bounded queries over secondary indexes, per-item atomicity. One
small trait:

```scala
trait Docs[A]:                       // A has a Schema; id is the key
  def get(id: Id): Option[A] ! Async
  def put(id: Id, a: A, cond: Cond = Cond.Always): PutResult ! Async
  def delete(id: Id, cond: Cond = Cond.Always): PutResult ! Async
  def query(q: IndexQuery, max: Int): Chunk[(Id, A)] ! (Produce + Async)
```

- **`Cond` is the load-bearing part**: `Always | IfAbsent |
  IfVersion(v)` — compare-and-set, which every serious document
  store offers (Dynamo condition expressions, Mongo find-and-modify,
  Cassandra LWT, etcd CAS). Conditional writes are how WithKey and
  optimistic concurrency are spelled in this class; `PutResult`
  says applied-or-not as data.
- **Consistency is a declared decision** (the `Ack` pattern):
  `One | Quorum | Strong` requested per operation, and what the
  engine actually grants exposed, like isolation in specs/jdbc.md.
- **Transactions, honestly**: per-item atomicity is the contract;
  multi-document transactions are NOT in the seam even where an
  engine advertises them (Mongo's have sharp constraints; Dynamo's
  cost double) — a multi-item change is a journaled sequence of
  conditional writes (Durable steps with WithKey/Reconcile), which
  is the saga pattern with our existing machinery and no new
  concepts.
- FOREIGN: their collections/tables, our `Docs` adapter per engine,
  drift checked where schemas exist (Mongo validators; else decode
  totality is the check). OWN: a compacted keyed topic + `View`
  ALREADY IS a document store (`latest(key)`) — the default until
  scale demands an engine, and then the engine is a
  materialization.

### OLAP and warehouses (Snowflake, ClickHouse, BigQuery, Redshift, Trino) — the JDBC seam plus a load posture

The read side needs NOTHING new: every one of these speaks JDBC,
and the existing streaming read at fetch-size is exactly how a
10^9-row scan should arrive (constant memory; measured shape from
day one). `verify` works — they expose metadata.

What is genuinely different is the WRITE POSTURE, and it gets a
spec section instead of a module: row INSERTs are wrong in this
class (cost and performance both); loading is BULK — stage a file
(the object-store put), COPY/LOAD it, under a LOAD ID that the
warehouse dedups (Snowflake per-file load history, BigQuery job
ids) — **WithKey at batch granularity**. A load retried after a
crash lands once because the far end recognizes the id: the
Durable recovery story, scaled up to files. Transactions: batch
atomicity is what these engines offer and all this posture uses.
Cost stated once: a warehouse query bills; budget/warehouse
selection is config (specs/conf.md), and the honest guidance is
that OLAP handles POINT NOTHING — it scans; point reads belong to
OLTP or a cache. Filed: `jdbc-bulk-load`.

### Data lakes (S3/Parquet, Iceberg, Delta) — read now via an engine, write via offload; no hand-rolled commit protocols

- READ, the pragmatic truth: lake tables are queried through
  engines — DuckDB (embedded, reads Parquet/Iceberg directly) and
  Trino both speak JDBC, so a lake read is `Jdbc.rows` against
  DuckDB pointed at the files: ZERO new machinery, verify and
  streaming included. A native columnar codec (a Parquet algebra
  over Schema, the Spark-encoder move again) is real work with a
  real payoff — staged for when a consumer hurts through the
  DuckDB path, not before.
- WRITE, own posture: okay-persist's stage 3 already names segment
  offload to object storage — our log's cold tail IS our lake, in
  our format, readable by the same recovery scan. Writing
  Iceberg/Delta TABLES (their commit protocols, manifest lists,
  optimistic snapshot commits) is explicitly an interop we adopt a
  library for or defer: table-format commit protocols are subtle,
  versioned, and exactly the kind of thing this stack refuses to
  approximate (the Redlock decision again).
- FOREIGN lake with restricted rights = the read path only, which
  is the DuckDB/Trino route unchanged.
- **Delta Lake, specifically** — worth naming because its
  architecture is OURS: `_delta_log/` is an append-only journal of
  actions (add/remove file, metadata, protocol) with Parquet
  CHECKPOINTS bounding the replay — the journal-foundation +
  snapshot-optimization decision of specs/persist.md, arrived at
  independently by another project at another scale. Concretely,
  three roads, in order: (1) READ through the existing JDBC seam —
  DuckDB's delta extension reads Delta tables today, zero new
  machinery; (2) READ/WRITE without Spark via **Delta Kernel**, the
  Delta project's own JVM library for exactly this — adopting their
  kernel is the honest form of the no-hand-rolled-commit-protocols
  rule, since the optimistic log-entry commit is theirs to version;
  (3) WRITE at scale through the okay-spark bridge that already
  exists — Spark is Delta's first-class writer, so the Aggregator
  bridge makes Delta output available NOW, before any kernel
  interop lands. Iceberg mirrors the shape (`iceberg-core` as its
  kernel-equivalent) when a deployment names it. Filed:
  `lake-delta`.

### Vector stores (pgvector, Qdrant, Milvus, ...) — the seam shipped with okay-rag

`VectorStore` (upsert/search/delete/size) is the seam; the memory
engine with CBOR persistence shipped, honest to ~10^5 brute-force.
The first real adapter is chosen for maximal reuse: **pgvector is a
VectorStore implemented entirely over specs/jdbc.md** — SQL, typed
edges, transact, their unique constraints; no new protocol in the
codebase. Server-side vector DBs (Qdrant-class) join as adapters
when a deployment names one. OWN posture note: the index is a
materialization of a topic of (segment, embedding) — okay-rag's
`Persist` already serializes exactly this, so rebuild = refold.
Filed: `rag-pgvector`.

### Heavy compute (Spark, Flink) — settled

The bridges exist (external-systems.md): any `Aggregator` exports
(zero, seqOp, combOp) to their APIs; Schema folds to Spark
encoders. Their place in this taxonomy: they are not storage — they
are how a FOREIGN-scale computation runs over the classes above,
and the merge contract is the same one okay-cluster distributes.
Nothing new filed. The CALL-shaped sibling of this class — a
statistical function over a frame, answered and journaled — is R,
specced separately (specs/r.md: operations not embeddings,
subprocess and Rserve engines behind one handler).

### Queues (RabbitMQ/AMQP, SQS, NATS, Pulsar, MQTT) — the ack shape, bridged not mirrored

The shape the log deliberately is not: per-message acknowledgement,
redelivery, competing consumers, no offsets — DELIVERY machinery,
not history. Two roads, and pointedly no new seam:

- **Ingress**: consume their queue, append into a persist topic,
  ack AFTER the append — at-least-once, with dedup one hop
  downstream (the record carries the broker's message id; topic
  consumers dedup by it — WithKey's shape again). The queue
  becomes an entry ramp to the log, and everything it could not do
  — replay, audit, fan-out to late consumers — is restored one hop
  in.
- **Egress**: a topic consumer publishes outward, its journaled
  offset making the publisher resumable; a broker that dedups on
  message id gets exactly-once OUTCOME, the rest get at-least-once
  said out loud.

A native `Queue` seam is REJECTED: it would mirror a lossy shape
into the core to save one hop, and every consumer of it would
rebuild the log's properties on top. Task distribution INSIDE the
stack is the cluster's worker model and consumer groups over
partitions — not a broker. Filed: `queue-shape` (the two bridges;
engine adapters as deployments name them).

### Named and deferred, so the list is complete

- **Search engines** (Elastic/OpenSearch): the inverted-index
  shape; okay-rag's Keyword side covers the in-process case; a
  foreign adapter is a seam implementation when named.
- **Time-series** (Prometheus/Influx): our stats-as-values plus a
  metrics topic cover the own posture; a foreign TSDB is a
  consumer of those values (an exporter), not a new seam.
- **Graph databases**: no seam until a consumer names a workload;
  refusing to guess an API for a class nobody here uses yet.
- **"Semi-SQL"** engines (CQL, ClickHouse dialects, PartiQL):
  where a JDBC driver exists (most), they are the JDBC seam with
  the OLAP or Docs posture as fits; where only a native protocol
  exists, they are a `Docs` adapter. The dialect differences live
  in the SQL strings, which stay visible — bind-don't-model
  already decided that.

## Scaling and reliability, the summary the ask deserves

- FOREIGN systems scale themselves; our contract is to consume
  them at constant memory (chunked streams), resume from offsets/
  watermarks (replayable sources, journaled positions), and write
  idempotently against their constraint machinery — so OUR crash
  never doubles THEIR data, which is the reliability property a
  client actually audits.
- OWN systems scale by the persist staging: partitions now,
  replication with epochs at stage 2, engine interop at stage 3,
  consensus honestly at stage 4 — and every materialized view
  (document, vector, cache) rebuilds by refold, so reliability
  reduces to the log's, once, instead of once per engine.

## Behavior

(umbrella-level acceptance; each class's claim carries its own full
list in its spec or spec section)

- [ ] `Docs` contract suite (the StoreSuite pattern): get/put/
      delete/query, `IfAbsent`/`IfVersion` semantics, consistency
      declared and granted — against the own-posture View engine
      and at least one foreign adapter
- [ ] a conditional write retried after a simulated lost ack lands
      once (CAS as WithKey), on the same two engines
- [ ] pgvector: the okay-rag store contract passes over specs/
      jdbc.md against Postgres; search agrees with the memory
      engine on a shared fixture
- [ ] a Parquet file on disk is read through the JDBC seam (DuckDB)
      with `verify` passing and constant-memory streaming asserted
- [ ] a bulk load with a load id, retried across a simulated crash,
      lands once (warehouse or its test double); row-INSERT paths
      refuse in the OLAP posture
- [ ] kafka-eos: a producer retry under the idempotent/
      transactional config does not duplicate (their test
      machinery), or the sink's at-least-once is asserted and
      documented where EOS is off
- [ ] every adapter exposes stats as a Schema value; no adapter
      logs or journals a credential (grep-able invariant from
      specs/conf.md asserted in tests where feasible)

## Out of scope

- multi-region anything; graph seams; search-engine adapters and
  TSDB adapters until a consumer names one
- hand-rolled Iceberg/Delta commit protocols, distributed locks,
  XA — refused above, not deferred
- a query federation layer over the seams — callers compose seams;
  a federator is a product, not a primitive

## Decisions

- **Taxonomy by access shape, not vendor** — shapes are stable
  (rows, events, documents, vectors, views, compute); vendors churn.
  Rejected: module-per-vendor (twenty APIs, one grave).
- **Vendor = seam implementation, never new surface** — the rule
  that keeps the codebase finite and the consumers portable across
  postures. Rejected: vendor-native APIs leaking upward.
- **One new seam only (`Docs`)** — every other class lands on an
  existing seam; the KV/document shape genuinely is not SQL, not a
  log, not a vector query. Rejected: per-engine NoSQL modules;
  also rejected: stretching `Topic` to cover point reads (a log
  with a hash on top is a worse `View`).
- **Conditional writes over multi-document transactions** — CAS is
  universal in this class, honest, and composes with Durable into
  sagas; the advertised multi-doc transactions are the exception
  that bills double or locks odd. Rejected: a transaction facade
  over engines that disagree about what one is.
- **Lake reads through DuckDB/Trino via JDBC first** — zero new
  machinery for the whole read side of the lake; the native
  columnar codec is staged behind demonstrated need. Rejected:
  Parquet-reader-first (months of codec work before the first
  query, replicating what an embedded engine does better).
- **pgvector as the first vector adapter** — it composes two
  existing specs (rag seam × jdbc seam) and exercises the foreign
  posture with transactions; a native vector-DB protocol adapter
  teaches nothing new. Rejected: Qdrant-first.
- **OLAP loads are files with ids, not rows** — the class's own
  idempotency machinery (load history) is the far end; WithKey
  scales to batch granularity unchanged. Rejected: row-DML into
  warehouses.
- **Own posture defaults to the log + materializations** — one
  reliability story, refold as the universal rebuild; engines join
  as views, so adopting or dropping one never moves the truth.
  Rejected: engine-per-class as the primary store in own mode.

## Results

(per class, as their claims land; this umbrella records
cross-class findings — the first being whether the Docs contract
truly fits Dynamo AND Mongo AND the View engine without growing
engine-shaped warts)
