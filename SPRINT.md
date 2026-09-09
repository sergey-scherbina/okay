# Sprint

## Doing
- lexer-state-allocation — BACKLOG: ~180 B per input CHARACTER on both
  lexing paths; Json's `S.copy(buf = s.buf + c)` is a fresh String per
  char, quadratic in token length. Carry the start offset, slice the
  input once at `finish`. B/op first, quiet box for time, lands only
  on the numbers. Claimed 2026-09-09
  (.work/active/lexer-state-allocation.claim).
- deploy-stop-grace — BACKLOG deploy-termination-grace: the manifests wait for the drain (claim: deploy-stop-grace)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
