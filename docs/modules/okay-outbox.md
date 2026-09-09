# okay-outbox

> The log and a database that is OURS (specs/outbox.md): an event
> written as a row in the business transaction and relayed into a
> topic afterwards, a consumer that records message ids inside its
> own transaction so the primary key refuses a duplicate, and a
> dead-letter topic for the record a handler cannot process. Two
> seams (`okay.sql.Sql`, `okay.persist.Topic`), nothing new minted.

Depends on: `okay-sql`, `okay-persist`, `okay-codec`. Cross-built
JVM/JS/Native; the H2 tests are JVM.

Not for THEIR database: specs/jdbc.md refuses an outbox where we
have no DDL rights, and that stands — there the posture is
idempotent writes keyed by their constraints. This module's tables
are ours, versioned by `Migrate` like any other script.

## Guide

**Outbox.** In the business transaction, beside the business rows:

```scala
val outbox = Outbox()                       // table okay_outbox
// once, in a migration: Script(3, "outbox", outbox.ddl(Dialect.Postgres))

Typed.transact[Unit, Async](db) { _ =>
  for
    _  <- widen(Typed.update[Order](db, "insert into orders ...")(order))
    id <- widen(outbox.enqueue(db, topic = "orders", value = Cbor.write(OrderPlaced(order))))
  yield ()
}
```

A relay carries rows into the log: `outbox.relayOnce(db, store)` is
one pass (unpublished rows in creation order, each appended to its
topic with the ROW ID as the record key, then marked published);
`outbox.relay(db, store, everyMillis = 500)` is the loop, run on a
fiber until cancelled. At-least-once, said out loud: a crash between
the append and the mark re-appends the same key next pass. Run one
relay per table. `outbox.pending(db)` is the number to watch.

**Inbox.** On the consuming side, when the effect is in the database:

```scala
val inbox = Inbox()                         // table okay_inbox
inbox.once(db, id = new String(record.key, "UTF-8")) {
  Typed.update[Shipment](db, "insert into shipments ...")(shipment)
}                                           // Some(rows) the first time, None after
```

`once` is a transaction that inserts the id and runs the body; the
first time it answers `Some`, a duplicate answers `None` and runs
nothing; a body that fails rolls the id back with it, so the record
is tried again. Two consumers racing on one id both pass the select
and the second insert fails on the key — correctness is the
constraint's, the select only the fast path. `first(db, id)` is the
same check for a transaction you already hold.

**Dead-letter.** For a consumer loop over a partition:

```scala
val dlq = store.topic("orders.dlq").of[DeadLetter.Dead]()   // okay.persist.of
DeadLetter.consume(orders, part = 0, group = "shipping", offsets, dlq, attempts = 3) { r =>
  ship(r)
}
```

Each record is handled in order; a failure is retried up to
`attempts` times, then the record is parked in the dlq as a `Dead`
(topic, partition, offset, key, value, attempts, the error) and the
offset is committed past it — a poison record never blocks the
partition. `DeadLetter.replay(dlq, store, from)` appends dead records
back onto their topics for another try, and answers where to
continue from. `Dead` has a Schema, so the dlq reads like any topic
and `/metrics` counts it as one.

## Gotchas

- The bytes column is the one type SQL never agreed on:
  `Dialect.H2` / `Postgres` / `Sqlite` pick `varbinary` / `bytea` /
  `blob`. Column names avoid `key`, `value` and `partition`, which
  are reserved somewhere.
- Creation order is `created_at, id`; rows within one millisecond
  are ordered by id. A consumer that needs total order needs a
  partition of its own.
- The relay's duplicate carries the SAME key; a consumer without an
  inbox (a pure projection, say) dedups with `Queues.dedup` or
  tolerates the repeat.
