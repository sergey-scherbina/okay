# Sprint

## Doing
- blob-fs — the object-store seam, stage 0: trait Blob (cross) and
  the filesystem engine (jvm), the contract suite the S3 engine will
  re-run; keys never escape the root (specs/blob.md)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
