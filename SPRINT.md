# Sprint

## Doing
- timeout-masks-failure — BACKLOG (CORE): `Async.timeout` lets a
  failing contender lose the race silently, so an immediate failure
  comes out as `None` after the whole ms. Law first ("a failure ends a
  timeout at once"), test on Async.timeout, then change timeout, not
  race. Claimed 2026-09-09 (.work/active/timeout-masks-failure.claim).
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
