# Sprint

## Doing
- deploy-stop-grace — BACKLOG deploy-termination-grace: the manifests wait for the drain (claim: deploy-stop-grace)
- obs-log — specs/obs.md: log lines as values, correlated to the trace by the handler (claim: obs-log)
- chunked-lexer-bookkeeping — BACKLOG (§10): chunked lexing 58.4 vs
  element-wise 49.3, the residual named as per-chunk bookkeeping.
  A/B in one run (the old loop kept benchmark-local), B/op first;
  lands only if it closes a real part of the gap, else the numbers
  close the item. Claimed 2026-09-09
  (.work/active/chunked-lexer-bookkeeping.claim).
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
