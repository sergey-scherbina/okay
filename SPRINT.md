# Sprint

## Doing
- obs-tracing — okay-obs: spans as values on a trace topic, W3C
  traceparent as the one propagation vocabulary, the tracing handler
  wrapping any Handler; export stays a consumer (specs/obs.md)

## Queue
- match-stage0 — specs/match.md stage 0: the model, Registry/Facts/
  Match effects, memory handler + rag embeddings, MCP tools, replay
  idempotence. The user-directed priority lane.

(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
