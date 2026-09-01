# Sprint

## Doing
- conf-topic — managed config as a compacted keyed topic: Configs
  (put/latest/at/history) as a thin convenience over Store in
  okay-persist, next to Snapshots/Offsets where the pattern lives;
  audit and rollback for free (specs/conf.md stage 2)

## Queue
(next candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
