# Outbox, inbox and dead-letter: the log and a database that is ours

## Overview

Three shapes every service that owns a relational database and
talks to a log eventually writes by hand, and the microservices
audit (2026-09-09) found none of them here:

- **Outbox.** A business transaction commits rows AND must announce
  an event. Appending to the log inside the transaction is not
  atomic with it (two systems, no 2PC — specs/jdbc.md and
  specs/data.md refuse XA), so a crash between commit and append
  loses the event, or between append and commit announces a rollback.
  The settled answer: write the event as a ROW in the same
  transaction, and let a relay carry rows into the log afterwards.
- **Inbox.** A consumer that acts on the log with database effects
  must not act twice on a redelivered record (the relay is
  at-least-once by construction; so is `Queues.ingress`). The
  settled answer: record the message id in a table inside the
  consumer's own transaction; a duplicate is refused by the unique
  constraint — "the best idempotency machinery in the business"
  (specs/jdbc.md).
- **Dead-letter.** A record the handler cannot process must not
  block its partition for ever. The settled answer: retry a bounded
  number of times, then park the record WITH its error in a
  dead-letter topic, commit past it, and let a person replay it.

specs/jdbc.md refuses an outbox for THEIR database (no DDL rights),
and that refusal stands; this module is for OUR databases — the
ones `Migrate` already versions — and its DDL is a value handed to
`Migrate` like any other script. `Queues.ingress`/`egress` bridge a
foreign BROKER to the log; this bridges a foreign TRANSACTION to it.
Nothing new is minted at the seams: the outbox and the inbox are
tables over `okay.sql.Sql`, the relay appends to a `okay.persist
.Topic`, the dead-letter topic is a `Typed[Dead]`.

## Interface

```scala
package okay.outbox

/** the column type bytes take, which is the one thing SQL never agreed on */
enum Dialect:
  case H2, Postgres, Sqlite

final case class Entry(id: String, topic: String, part: Int,
                       msgKey: Array[Byte], msgValue: Array[Byte],
                       createdAt: Long) derives Schema

final class Outbox(table: String = "okay_outbox",
                   clock: () => Long = wall, ids: () => String = uuid):
  /** the DDL, for our own database's migration scripts */
  def ddl(dialect: Dialect): String
  /** INSIDE the caller's transaction: the event as a row; answers its id */
  def enqueue(db: Sql, topic: String, value: Array[Byte],
              key: Array[Byte] = Array.empty, part: Int = 0): String ! Async
  /** one relay pass: unpublished rows in creation order → the topic,
    * each marked published after its append; at-least-once, the row id
    * is the record KEY so a downstream inbox dedups on it */
  def relayOnce(db: Sql, store: Store, batch: Int = 256): Int ! Async
  /** the relay as a loop, every `everyMillis`, until cancelled */
  def relay(db: Sql, store: Store, everyMillis: Long, batch: Int = 256)(using Timer): Nothing ! Async
  def pending(db: Sql): Long ! Async

final class Inbox(table: String = "okay_inbox", clock: () => Long = wall):
  def ddl(dialect: Dialect): String
  /** INSIDE the caller's transaction: true the first time an id is
    * seen (the row is inserted), false when it is already there */
  def first(db: Sql, id: String): Boolean ! Async
  /** the whole shape: a transaction that records the id and runs the
    * body; a duplicate runs nothing and answers None */
  def once[A](db: Sql, id: String)(body: => A ! Async): Option[A] ! Async

object DeadLetter:
  final case class Dead(topic: String, part: Int, offset: Long,
                        key: Array[Byte], value: Array[Byte],
                        attempts: Int, error: String, at: Long) derives Schema
  /** one pass from the group's committed offset: each record handled,
    * a failure retried up to `attempts` times, then parked in `dlq`
    * with its error; the offset committed after every record so a
    * poison record is passed, never re-met */
  def consume(topic: Topic, part: Int, group: String, offsets: Offsets,
              dlq: Typed[Dead], attempts: Int = 3, max: Int = 256,
              clock: () => Long = wall)
             (handle: Record => Unit ! Async)(using Scheduler): Int ! Async
  /** a dead record back onto its topic, for another try */
  def replay(dlq: Typed[Dead], store: Store, from: Long, max: Int = 256): Long ! Async
```

## Behavior

- [x] outbox: a row written inside a `Typed.transact` body is
      relayed after the commit; a body that ABORTS leaves no row and
      relays nothing — the event and the rows share one fate
- [x] outbox: the relay appends in creation order with the row id as
      the record key, marks each row published, and answers the
      count; a second pass with nothing pending appends nothing
- [x] outbox: a relay that appended but died before marking (forced:
      a store whose append succeeds and a mark that throws) appends
      the same key again on the next pass — at-least-once, and the
      duplicate carries the SAME key, which is what the inbox needs
- [x] outbox: `pending` counts unpublished rows; `ddl` renders per
      dialect and H2's is what the tests create the table with
- [x] inbox: `first` answers true once per id and false after, inside
      a transaction; `once` runs the body the first time (answering
      Some) and not the second (None); a body that fails leaves the
      id UNRECORDED (the transaction rolled back), so the record is
      tried again
- [x] inbox: the relay's duplicate (the test above) is collapsed by
      `once` on the consumer side: two records, one effect
- [x] dead-letter: a handler that fails on one record is retried
      `attempts` times on that record, the record is then parked in
      the dlq as a `Dead` carrying topic, partition, offset, key,
      value, attempts and the error, the offset is committed past
      it, and the records after it are handled — the partition is
      not blocked
- [x] dead-letter: a handler that succeeds on the retry parks nothing
- [x] dead-letter: `replay` appends a dead record back to its topic
      with its key and value, and the consumer meets it again from
      its committed offset
- [x] docs: docs/modules/okay-outbox.md, indexed

## Out of scope

- An outbox in THEIR database — specs/jdbc.md's refusal stands; the
  posture there is idempotent writes keyed by their constraints.
- A relay that is exactly-once. It is at-least-once by construction
  and says so; exactly-once OUTCOME is the inbox's, downstream.
- Concurrent relays over one table (they would double-publish; still
  at-least-once). Run one relay per table; a lease for it is the
  persist election's shape, not this module's.
- A monotone sequence column. Creation order is `created_at, id`;
  rows within one millisecond are ordered by id, which is not their
  insertion order. A consumer that needs total order needs a
  partition of its own, which is the log's job.
- Automatic DLQ replay, back-off between retries beyond the policy
  stream `okay.Retry` already supplies — a person decides.

## Design

**Tables, not a new seam.** The outbox and the inbox are two tables
over `Sql`; their DDL is a value per dialect (bytes are the one
column type SQL never agreed on: `varbinary` / `bytea` / `blob`),
handed to `Migrate` as a script by the application. Column names
avoid every reserved word that bit someone (`part`, `msg_key`,
`msg_value`).

**The unique constraint is the inbox's machinery.** `first` selects
then inserts, inside the caller's transaction. Two consumers racing
on one id both pass the select; the second INSERT fails on the
primary key, its transaction is cancelled by the region's brake, and
its body's effects go with it — correctness lives in the constraint,
the select is only the fast path.

**The dead-letter topic is a typed topic.** `Dead` has a Schema, so
the dlq is readable by every tool that reads the log, `/metrics` can
count it (`Store.stats`), and a replay is a read plus an append.

**Failure is observed with `Async.attempt`.** The consumer's handler
runs under the core's own attempt (one fiber per record, a
`Scheduler` in scope), so a throw anywhere in the handler is data.

## Decisions

- **A module, not okay-persist or okay-sql** — the outbox needs both
  seams and neither module should depend on the other; the module
  is cross-built because both are.
- **The row id is the record key** — `Queues.ingress` made the same
  choice for the broker id: the dedup handle rides where every
  consumer can see it without decoding the value.
- **`select` then `insert` for the inbox** — not "insert and catch":
  the unique-violation exception is driver-shaped (SQLSTATE 23505 on
  pg, an H2 class, a SQLite message) and the region cancels on any
  throw anyway; the fast path avoids the exception in the common
  case and the slow path is still correct.

## Results

**Landed (outbox, 2026-09-09).** Module `okay-outbox`, cross-built
JVM/JS/Native, JVM tests on H2 + MemoryStore: `TestOutboxInbox` (5)
and `TestDeadLetter` (3). What the tests pinned: one fate (an
aborted `Typed.transact` body leaves neither the order row nor the
outbox row); creation order across batches with the row id as the
record key; the forced crash window (a `Store` decorator whose
append succeeds and then throws) re-appends the SAME key on the next
pass and `Inbox.once` collapses the pair to one effect; a failing
`once` body rolls its id back so the record is tried again; a poison
record is tried exactly `attempts` times, parked as a `Dead` with
its error, the offset is committed past it and the records after it
are handled; a retry that succeeds parks nothing; `replay` puts a
dead record back and the consumer meets it from its committed
offset. `Rows.all` is the module's own small collector over
`Typed.rows` — a damaged row in OUR table is a failure, not data.
