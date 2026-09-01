# Sprint

## Doing
- [ ] direct-auto-coloring — v2 of the direct macro: no marks at
  all. The block becomes DirectCtx[F] ?=> A; a Conversion[G[A], A]
  gated on (DirectCtx[F], Effect[G]) lets F[A]-as-A typecheck ONLY
  inside direct blocks and ONLY for marker-registered types; the
  macro rewrites the conversion calls with the existing machinery.
  Explicit marks stay the default (specs/direct-macro.md, Out of
  scope -> this task; user-directed 2026-09-01)
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
