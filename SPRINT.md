# Sprint

## Doing
- wire-tls — the one transport seam: sslmode vocabulary
  (verify-full default), Tls.client/server over the blocking-socket
  transport our own wires use, private keys as Secret refs; live
  handshake tests against a generated CA (specs/tls.md)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
