# Sprint

## Doing
- jdbc-bulk-load — the OLAP write posture: stage + COPY under a
  LOAD ID the history table dedups (WithKey at batch granularity),
  row DML refused in the posture; DuckDB as the test double
  (specs/data.md)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
