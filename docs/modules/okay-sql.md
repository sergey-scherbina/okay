# okay-sql

> The relational seam: SQL without a mandatory JDBC. One small
> driver trait (`Sql`) moves statements, values and row frames;
> everything smart — typing, totality, verify, the transaction
> region — lives above it, written once, cross-platform
> (specs/sql.md, specs/jdbc.md).

Depends on: the core (streams, Async, Resource) and `okay-codec`
(Schema is the row and parameter codec). No java.sql anywhere —
asserted structurally: the module cross-builds for JS and Native.

## Guide

**Bind, don't model.** The database's schema is authoritative; SQL
stays the query language. This module adds typed edges: parameters
bound from a case class by Schema (positionally, always through
the driver's prepared path — injection is unrepresentable), rows
decoded into a case class by Schema (by column LABEL, camelCase →
snake_case, so `SELECT *` reordering cannot shear the mapping),
and `Typed.verify` catching drift at startup as `Drift` values
naming the column — the Durable fingerprint lesson at the
database seam.

**Damage is data.** A row that does not decode is
`Bad(column, error, row)` in the stream, never a throw; after a
passing verify it means the world changed mid-run, and the caller
decides.

**The transaction region.** `Typed.transact` is a Resource scope
over the driver's begin/commit/rollback: commit on normal
completion; on an exception or a handled abort crossing the scope
the finalizer pulls `Sql.cancel()` — the one deliberately
synchronous method, because finalizers are sync by design (a wire
driver may just close the connection; servers roll back abandoned
transactions). Isolation is declared per transaction and `Granted`
answers with what the engine actually gave, so a caller can refuse
a downgrade. Nested transact refuses loudly.

**Drivers.** okay-jdbc's `JdbcSql` is the first (JVM, blocking
behind `Async.Run` on virtual threads — the honest default for H2,
DuckDB, warehouses); okay-pg (the Postgres v3 wire, cross-platform)
and an R2DBC hatch are filed. One typed program, many roads in —
the okay-http/okay-ui/okay-persist shape, told for SQL.
