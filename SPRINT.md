# Sprint

## Doing
- sql-commit-tag — BACKLOG "persistence-audit", first of seven lanes
  (the operator's go, 2026-09-09): COMMIT on an aborted pg transaction
  answers ROLLBACK and the region reports success. Test first, then the
  tag check. Claim: sql-commit-tag. The six that follow, in order:
  sql-serialization-retry, sql-temporal-types, sql-readonly-region,
  sql-pool, persist-saga, docs-dynamo.
- split-without-either — specs/handler-fusion.md "Stage A": the row
  split without an Either/Option per operation, measured on
  Fused.stateWriter then the shipping runners. Claimed 2026-09-09
  (.work/active/split-without-either.claim). Next in the same arc:
  handler-fusion-eff (Stage B), claimed when A lands.
- resilience — specs/resilience.md stage 0, the five handlers (claim: resilience)
- ui-vocab — specs/frontend.md stage 0, the vocabulary (claim: ui-vocab)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
