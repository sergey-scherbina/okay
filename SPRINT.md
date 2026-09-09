# Sprint

## Doing
- handler-fusion-eff — specs/handler-fusion.md "Stage B": the composite
  `!>` for a row over Eff, assembled inline, product accumulator in the
  answer type; bar 1.5x over Fused.stateWriter's right-nested 13.8 µs
  (post-stage-A). Claimed 2026-09-09 (.work/active/handler-fusion-eff.claim).
- sql-temporal-types — BACKLOG "persistence-audit", lane 3 of 7:
  timestamp/date/time/uuid/json as SqlValue cases with Schema givens,
  both drivers, verify. Claim: sql-temporal-types. Then:
  sql-readonly-region, sql-pool, persist-saga, docs-dynamo.
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
