# Sprint

## Doing
- match-vec-cache — SqlMatch embeds every candidate profile's summary
  on EVERY candidates() call, and every live attribute's text on every
  registrySearch(). MemoryMatch has cached summaries since stage 0
  (`summaries`, invalidated on assert/supersede/link/reset); the SQL
  engine, which is the one a deployment runs, has nothing. Free when
  `embed` was Vectors.hashing (the arithmetic it was written against),
  one model inference per candidate per query with a real encoder.
  Measured downstream in okay-chat on a real multilingual encoder,
  50 profiles, 1 vCPU: 4.4s cold, 1.5s warm, ~80ms per profile.
  Spec: specs/match.md, match-vec-cache.

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
