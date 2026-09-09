# Sprint

## Doing
- resilience-faults — specs/resilience.md stage 2: seeded Faults.http, the composite under a plan (claim: resilience-faults)
- handler-fusion-eff — specs/handler-fusion.md "Stage B": the composite
  `!>` for a row over Eff, assembled inline, product accumulator in the
  answer type; bar 1.5x over Fused.stateWriter's right-nested 13.8 µs
  (post-stage-A). Claimed 2026-09-09 (.work/active/handler-fusion-eff.claim).
- sql-readonly-region — BACKLOG "persistence-audit", lane 4 of 7:
  transact(readOnly = true) on pg and JDBC; JdbcSql restores the
  isolation level beside autocommit. Claim: sql-readonly-region. Then:
  sql-pool, persist-saga, docs-dynamo.
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
