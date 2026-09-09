# Sprint

## Doing
- resilience-http — specs/resilience.md stage 1: Resilient.http/route + okay-ops rows (claim: resilience-http)
- sql-temporal-types — BACKLOG "persistence-audit", lane 3 of 7:
  timestamp/date/time/uuid/json as SqlValue cases with Schema givens,
  both drivers, verify. Claim: sql-temporal-types. Then:
  sql-readonly-region, sql-pool, persist-saga, docs-dynamo.
- split-without-either — specs/handler-fusion.md "Stage A": the row
  split without an Either/Option per operation, measured on
  Fused.stateWriter then the shipping runners. Claimed 2026-09-09
  (.work/active/split-without-either.claim). Next in the same arc:
  handler-fusion-eff (Stage B), claimed when A lands.
- ui-vocab — specs/frontend.md stage 0, the vocabulary (claim: ui-vocab)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
