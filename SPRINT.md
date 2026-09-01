# Sprint

## Doing
- cache-redis — the minimal RESP client (four commands do not
  justify a dependency), the Redis engine with CBOR values and
  server-side expiry, and the cross-node invalidation topic where
  a replaying node converges (specs/cache.md stage 2)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
