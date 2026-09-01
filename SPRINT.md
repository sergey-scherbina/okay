# Sprint

## Doing
- [ ] direct-mark-retire — remove the symbolic .? mark from Direct
      and Monadic (three strikes recorded: .!, .!?, .?); .reflect is
      the one spelling; sweep tests/docs/specs; the Ambiguous-
      extension-methods error class disappears by construction
      (user-directed 2026-09-01)
- [ ] error-messages — the compile-error quality pass:
      @implicitNotFound on core typeclasses and capabilities with
      actionable text, audit every errorAndAbort/compiletime.error
      to what-why-fix, compileErrors tests asserting message QUALITY
      so wording cannot degrade silently (user directive:
      "obligatory")
## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
