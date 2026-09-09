# Sprint

## Doing
- sql-pool — BACKLOG "persistence-audit", lane 5 of 7: okay.sql.Pool,
  driver-neutral, cancel-safe hand-off, the brake on return. Claim:
  sql-pool. Then: persist-saga, docs-dynamo.
- resilience-faults — specs/resilience.md stage 2: seeded Faults.http, the composite under a plan (claim: resilience-faults)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
