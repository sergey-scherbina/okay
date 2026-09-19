# okay-outbox

The log and a database that is OURS (specs/outbox.md): an event written as a row in the business transaction and relayed into a topic afterwards, a consumer that records message ids inside its own transaction so the primary key refuses a duplicate, and a dead-letter topic for the record a handler cannot process. Two seams (`okay.sql.Sql`, `okay.persist.Topic`), nothing new minted.

**Depends on:** `okay-sql`, `okay-persist`, `okay-codec`. Cross-built

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-outbox.md`](../docs/modules/okay-outbox.md) | what it is, and the reasoning |
| [`specs/outbox.md`](../specs/outbox.md) | the design and its decisions |
