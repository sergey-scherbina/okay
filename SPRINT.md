# Sprint

## Doing
- [ ] direct-macro — v1 of the scoped direct-style macro:
  direct[F] { flat block with m.? / m.reflect } rewritten into
  Cont binds over Monadic (specs/monadic-reflection.md is the
  semantic floor). Scope: linear val/expr, if/match; reflect under
  a lambda = compile error with a clear message; try/while and
  auto-coloring (Conversion trick) deferred to v2
  (user-directed, 2026-09-01)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
