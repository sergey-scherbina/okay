# Sprint

## Doing
- persist-backup — the DOCTOR (recovery scan against a backup,
  offline: an independent reader of the documented segment format)
  and Backup.copy/restore over the blob seam; FileStore untouched
  (specs/persist.md, Backup and restore)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
