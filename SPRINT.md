# Sprint

## Doing
- **direct-flatmap-emission** (claimed): the direct macro's compilation
  target moves from `Cont[T, F[A], F[A]]`+reify to plain `F[T]` flatMaps
  — the bench-direct lane priced the Cont layer at 3.3x (§1b,
  docs/benchmarks.md) and filed this; the reflect encoding
  (`shift(k => m.flatMap(k))`) adds nothing semantically, so the whole
  target retires, not just sequential fragments. Spec: the Decisions
  section of specs/direct-macro.md. Gate: all TestDirect* suites
  unchanged and green; quiet-box §1b rerun recorded.
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
