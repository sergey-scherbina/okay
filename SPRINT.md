# Sprint

## Doing
- py-worker — okay-py stage 1: the worker pool (N processes is the
  parallelism, the GIL is then irrelevant), the supervisor replacing
  dead workers cold, the two-engine acceptance (specs/py.md)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
