# Sprint

## Doing
- own-db-migrations — Migrate over the Sql seam (okay-jdbc): the
  Flyway model adopted, checksummed versioned scripts, the
  schema-version table in the same database, changed checksums
  refuse loudly; ops-topic hook (specs/jdbc.md, Own relational
  databases)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
