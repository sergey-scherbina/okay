# Sprint

## Doing
- sql-seam — okay-sql: the Sql driver trait (SqlValue/Col, Async) and
  the typed layer (rows/params/verify/transact) written ONCE against
  it, cross-built as the structural no-java.sql proof; okay-jdbc
  becomes the seam's first driver and passes the jdbc.md behavior
  list on H2 with a no-DDL user (specs/sql.md + specs/jdbc.md)

## Queue
(next candidates from BACKLOG.md: sql-seam, conf-impl,
 persist-wire — the seams the most filed work binds to; ui-durable
 and mcp-resumable-sse can now bind to stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
