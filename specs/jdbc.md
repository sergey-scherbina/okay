# okay-jdbc: someone else's database — typed rows, transactions, no DDL

## Overview

okay-jdbc today is the streaming interop (specs/external-systems.md):
a query as chunks at fetch-size, the connection under the Resource
region, writes a chunk per batch. This spec adds what a business
database actually demands and states the posture for the common
reality that database is usually in: it EXISTS, a DBA owns it, its
structure is not ours to change — no creating tables, no DDL, often
no rights beyond SELECT and maybe DML on a few tables. The module
must be fully useful under exactly those rights.

The posture, in one line: **the database's schema is authoritative —
we bind to it, we do not model it.** SQL stays the query language
(it is the DBA's language, the optimizer's input, and the only
honest description of what a query costs); okay adds TYPED EDGES to
it — parameters bound from a case class by `Schema`, rows decoded
into a case class by `Schema`, and drift between our types and their
schema caught loudly at the seam, at startup, naming the column.
This is the doobie/JDBI shape, not the ORM shape, and that is a
decision (below), not an omission.

**Seam update (specs/sql.md).** The CONTRACT below — typed rows,
verify, the transaction region, the no-DDL posture, the write
bridge — is unchanged, but it binds to the driver-agnostic `Sql`
seam rather than to `java.sql.Connection`: okay-jdbc is that seam's
FIRST driver (and stays the right one for warehouses, DuckDB and
everything with a good JVM driver), with the Postgres wire protocol
and an R2DBC hatch as the other roads in. The sketch below is
stated against the seam (audit fix, 2026-09-01); only the raw
`JdbcInterop` streaming keeps `Connection` in hand, deliberately.

## Typed rows: Schema at the flat edge

A result row is a FLAT product. The same derived `Schema[A]` that
serves JSON and CBOR serves rows, restricted to what a row can hold:

- primitives (Int/Long/Double/Boolean/String), bytes (`SBytes` —
  BLOBs travel as bytes, the okay-rag lesson), and `Option[_]` for
  nullable columns. Timestamps arrive as the JDBC epoch millis into
  a Long field v1 (a date type joins the algebra only if consumers
  hurt without it).
- field ↔ column by NAME: the field `userName` matches the column
  label `user_name` (camelCase → snake_case, the near-universal
  convention; an exact-label escape hatch per field when a legacy
  column defies it). Matching by label, not position, so `SELECT *`
  reordering does not silently shear the mapping.
- decode is TOTAL, damage is data: a NULL in a non-Option field, a
  type the field cannot hold, a missing column — each an error value
  naming the column and the row position, never a throw. Sums and
  nested products do not map to rows and are out of scope until a
  consumer names a need (a discriminator-column convention is the
  known future shape).

**`verify` — the fingerprint lesson at the database seam.** Schema
drift (a column dropped, renamed, retyped, made nullable) must not
be discovered one damaged row at a time in production. `verify[A]`
prepares the query and compares `ResultSetMetaData` against
`Schema[A]`: every mismatch reported as data — column, expected,
found — at STARTUP, the same move as Durable's fingerprint: the
drift stops the program loudly at the exact place, instead of
feeding values to the wrong field. A passing verify makes a later
per-row decode error what it actually is: an anomaly worth stopping
on, not a mapping bug.

**Parameters.** Bound positionally from a product by the same
Schema, always through `PreparedStatement` — there is no API that
interpolates a value into SQL text, so injection is unrepresentable,
not discouraged. Positional `?` in declared field order; a named-
parameter mini-syntax is rejected below.

## Transactions

The missing correctness piece, and it is a REGION, which the stack
already has the shape for (Resource: acquire/release in reverse, on
success, on handled aborts, on exceptions):

- `transact(conn, isolation)` opens a scope: autocommit off, the
  body runs, COMMIT on normal completion, ROLLBACK on an exception
  or a handled abort crossing the scope, autocommit restored either
  way. Streaming reads inside stay chunked (fetch-size is not
  invalidated by a transaction); batches inside are atomic with the
  rest of the scope.
- ISOLATION is a declared per-transaction decision (the `Ack`
  pattern one more time): ReadCommitted, RepeatableRead,
  Serializable. Declared and passed through — with the honest note
  that engines interpret levels differently (Oracle has no
  RepeatableRead, Postgres upgrades it); `transact` reads the level
  back from the connection and exposes what was actually granted,
  so a program that requires Serializable can refuse to run on less
  rather than assume.
- NESTED `transact` on the same connection refuses loudly v1. A
  savepoint API is a known future shape; silently flattening nested
  scopes (the common ORM behavior) is the bug we refuse to ship —
  a rollback that quietly does not roll back.

## Writing correctly into a database we do not own

No DDL rights means no outbox table, no offsets table, no advisory
anything — so the exactly-once question gets the same honest answer
`Durable` already wrote down: exactly-once EXECUTION between their
database and any journal of ours is impossible; what exists is
exactly-once OUTCOME, achieved at the far end. And here the far end
is a relational database — **whose unique constraints are the best
idempotency machinery in the business**. The Durable policies map
onto it directly, per statement, where the statement is declared:

- `WithKey` — the retry carries the same natural key, and their
  UNIQUE constraint deduplicates: `INSERT … ON CONFLICT DO NOTHING`
  (or the engine's MERGE) is WithKey spelled in SQL.
- `Reconcile` — a SELECT by that key IS the reconciliation: ask the
  database what happened, settle the journal.
- `Redo` — naturally idempotent statements (an UPDATE setting an
  absolute value, an idempotent UPSERT).

A write journaled in okay-persist and applied to their DB under one
of these policies survives a crash between journal and commit — the
recovery path re-applies under the policy and their constraints
answer "already happened". That is the correct bridge, and it needs
NOTHING created on their side beyond the constraints a sane schema
already has. (When a natural key does not exist, that is not a
library gap — it is a data-model conversation with the DBA, and the
spec says so rather than inventing a probabilistic dedup.)

The shipped shape (jdbc-write-bridge): `Writes(db, topic, run)` —
driver-agnostic, written against the `Sql` seam and a persist
`Topic`, housed in okay-jdbc only until a second driver wants it.
`write(sql, params, key)` journals `Intent(seq, sql, params, key)`
(Ack.Durable), executes, journals `Done(seq, count)` — intent
physically first, records through the persist `Typed` envelope.
`recover(policy)` refolds the run's records; every intent without
a completion is the crash window, resolved per key by the declared
`Policy`: `WithKey` re-executes the SAME statement with the SAME
key (the statement's contract is the idempotent form — MERGE /
ON CONFLICT — which is WithKey spelled in SQL), `Reconcile(select)`
asks by key and settles the journal without re-executing,
`Fail` reports `Unresolved` as DATA (a batch recovery answers per
entry; the caller decides — unlike Durable.tools, whose single
in-flight call rightly throws).

## Reading their data as it changes (three honest levels)

- **Snapshot** — any query, any time: the existing streaming read.
  Always available, always correct as-of its own transaction.
- **Incremental poll** — available exactly when their schema offers
  a monotone, commit-visible column (a sequence id, an updated_at
  they maintain): poll `WHERE col > watermark ORDER BY col`, the
  watermark journaled like any consumer offset. The caveat stated,
  not hidden: a transaction that commits LATE with a smaller value
  is invisible to a watermark that already passed it — mitigated by
  a lag window (`col <= now() - ε`), never eliminated. The spec
  refuses to call this CDC.

  The shipped shape (jdbc-poll-source): `Poll(db, offsets, group,
  source, start)` — the watermark IS a consumer offset, stored
  through persist `Offsets` (commit-as-record, refold-on-restart,
  nothing new to make durable). `poll[A](sql)(watermarkOf)` binds
  the current watermark as the statement's one parameter, decodes
  through the typed layer, and answers a `Batch`: the decoded
  PREFIX up to the first damaged row (the torn-tail doctrine — the
  watermark advances only over what decoded, a damaged row stops
  the advance and surfaces), plus the new watermark. The lag
  window lives in the CALLER's SQL text (`and col <= ...`), where
  the DBA can read it — the API adds no second language for it.
  At-least-once by construction: the commit follows the hand-over.
- **True CDC** — their WAL (Debezium-class). An interop question,
  out of scope here; if a deployment has it, its stream is just a
  source that appends into an okay-persist topic, and everything
  downstream is the ordinary log story.

## Config and secrets

Connection configs follow specs/conf.md: `url` (address only),
`user`, `password: Secret` — separate fields, resolved at the edge,
handed to `connection(url, user, password)` which already has the
right signature. Nothing here stores or logs a credential; the
JDBC URL never carries one (conf invariant 2).

## Own relational databases: migrations (the audit's gap, closed)

The no-DDL posture is for THEIR databases. Our own — the
materialization targets of specs/data.md's own posture — need the
opposite discipline, and it is the industry's settled one, adopted
rather than reinvented (the Flyway model): VERSIONED SQL scripts,
applied in order, each recorded with its CHECKSUM in a
schema-version table in the SAME database — transactional with the
DDL where the engine allows it (Postgres does), so the database
itself answers "what am I". A changed checksum on an
already-applied script REFUSES loudly (the fingerprint rule at yet
another seam); the applied set is also appended to an ops topic
for the audit trail. Auto-generated diff migrations are rejected:
a migration is authored SQL, reviewed like the code it is. Filed:
`own-db-migrations`.

## Interface (sketch — the shape, not the final word)

```scala
// the typed layer, once, against the Sql driver seam (specs/sql.md)
object Typed:
  /** startup drift check via the driver's describe: mismatches as data */
  def verify[A: Schema](db: Sql, sql: String): Vector[Drift] ! Async

  /** typed streaming read; per-row damage is data */
  def rows[A: Schema](db: Sql, sql: String, params: Product = EmptyTuple)
  : Chunk[Either[Bad, A]] ! (Produce + Async)

  /** typed write; params from the product, always prepared */
  def update[P: Schema](db: Sql, sql: String)(p: P): Long ! Async
  def batchOf[P: Schema](db: Sql, sql: String)(rows: Chunk[P]): Long ! Async

  /** the transaction region over the driver's begin/commit/rollback */
  def transact[A](db: Sql, isolation: Isolation = Isolation.ReadCommitted)
                 (body: => A ! (Resource + Async)): A ! (Resource + Async)
```

Module facts: stays `okay-jdbc` (JVM, honestly — JDBC is JVM); gains
a dependency on `okay-codec`; H2 already in the test scope is the
test database, with the no-DDL posture tested by running the typed
layer against tables created by "someone else" (test setup) under a
user with no DDL rights.

## Behavior

- [x] a row decodes into a case class by column label (camel→snake),
      Option for NULL, bytes for BLOB; a NULL in a non-Option field
      is an error value naming the column, not a throw
- [x] `SELECT *` with reordered columns decodes identically (label,
      not position)
- [x] `verify` reports a dropped, renamed, retyped and
      nullability-changed column, each naming the column; a passing
      verify then decodes every row of a matching table
- [x] params bind positionally from a product through
      PreparedStatement; no API accepts a value into SQL text
- [x] `transact` commits on success; rolls back on an exception AND
      on a handled abort crossing the scope; autocommit restored in
      both paths
- [x] nested `transact` on one connection refuses loudly
- [x] requested isolation is passed through and the GRANTED level is
      exposed; a caller can refuse a downgrade
- [x] a streaming read inside a transaction stays chunked at
      fetch-size (constant memory over a large table)
- [x] the WithKey bridge: an insert with a natural key, retried
      after a simulated crash-between-journal-and-commit, lands
      once (unique constraint dedup, H2); Reconcile: the SELECT by
      key settles the journal without re-executing (TestWrites: the
      reconcile fixture is a PLAIN insert, so a re-execution would
      have thrown on the primary key — proven, not assumed; an
      empty Reconcile and Fail answer Unresolved as data, world
      untouched, entry left open)
- [ ] incremental poll by a monotone column resumes from a journaled
      watermark; the late-commit caveat demonstrated as a test that
      DOCUMENTS the miss and the lag-window mitigation
- [x] the typed layer works end-to-end as a user with no DDL rights
      (their schema, our types)

## Out of scope

- an ORM, a query DSL, entity graphs, lazy loading — rejected below,
  not deferred
- connection pooling — a pool is a Resource-shaped interop (HikariCP
  behind the existing `connection` seam) when a deployment needs
  one; not v1 machinery
- distributed transactions / XA — the WithKey/Reconcile bridge is
  the answer at this stack's scale; XA's complexity buys ambiguity
- compile-time SQL validation — needs a live schema at build time;
  `verify` at startup is the honest equivalent of the same promise
- sums and nested products in rows; date/time algebra types —
  future shapes, each waiting for a consumer that hurts

## Decisions

- **Bind, don't model** — their schema is authoritative and version
  -controlled by someone else; an ORM's model layer would be a
  second source of truth that drifts. SQL + typed edges + `verify`
  keeps one truth and one loud drift alarm. Rejected: query
  DSL/ORM (a second language for the thing the DBA already reviews
  as SQL; hides cost; and its migrations assume the DDL rights this
  spec exists to live without).
- **Schema is the row codec** — one algebra for JSON, CBOR, Spark
  encoders (external-systems.md) and now rows; a second
  row-mapper algebra would split the type story. Rejected: a
  dedicated RowMapper typeclass.
- **verify-at-startup over per-row hope** — the Durable fingerprint
  lesson applied to a schema we do not control: drift is caught at
  the seam, loudly, before data flows. Rejected: discovering drift
  as production decode errors.
- **Damage is data even here** — per-row decode errors are values;
  after a passing verify they mean the world changed mid-run, and
  the CALLER decides to stop or skip. Rejected: throwing mappers.
- **Their constraints are the idempotency far end** — WithKey/
  Reconcile/Redo map to UNIQUE + ON CONFLICT + SELECT-by-key, which
  is machinery every serious schema already has; nothing to create,
  which is the constraint. Rejected: probabilistic dedup, XA.
- **Positional params from field order** — the order is visible in
  the case class next to the SQL; a named-param syntax is a parser
  and a new failure mode for one line of readability. Rejected:
  `:name` parameters v1.
- **Nested transact refuses** — flattening silently is the ORM bug
  where a rollback does not roll back; savepoints can arrive later
  as an explicit API. Rejected: implicit savepoint nesting v1.

## Results

Landed with sql-seam (2026-09-01): the typed layer lives in
okay-sql against the `Sql` trait (specs/sql.md, where the full
Results are recorded); okay-jdbc is the first driver. The whole
TestTyped battery runs on H2 AS the no-DDL `app` user against
tables the admin made — the posture proven, not simulated: 13
tests covering every checked box above. The two unchecked boxes
are their own filed slugs (jdbc-write-bridge, jdbc-poll-source).
One find escaped the module: rollback-on-exception exposed a
finalizer leak in core `Resource.run` (a continuation throwing
after a forwarded effect), fixed and pinned in the core suite.
jdbc-write-bridge followed (same day): `Writes(db, topic, run)` —
intent-first records through the persist Typed envelope, recover
by refold, five tests including both crash windows (executed with
the ack lost; never executed) and seq continuity over restart.
