# Sprint

## Doing
- **direct-tail-fusion** (claimed): loop bodies compile against an
  explicit tail so the sequencing bind merges into the body's last
  bind — while's measured 2.0x (two binds per iteration, §1b) goes
  to ~1x. Spec: the Decisions section of specs/direct-macro.md.
  Gate: every TestDirect* suite green unchanged; quiet-box §1b
  while row re-measured.
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
