# okay-r2dbc

> The R2DBC hatch of the `Sql` seam: any `io.r2dbc.spi.Connection`
> behind the same trait, so the typed layer (rows/params/verify/
> transact) runs over MSSQL, Oracle, MySQL — any engine with a
> maintained reactive driver — unchanged (specs/sql.md).

Depends on: `okay`, `okay-sql` (JVM), `r2dbc-spi`. Tests: H2 through
`r2dbc-h2`; Postgres through `r2dbc-postgresql` against the dockerized
server (skips where absent).

## Guide

**Connect.** Build the driver's `ConnectionFactory` as its docs say,
take a `Connection` from it, wrap it: `R2dbcSql(conn)`. Parameters are
positional — `$1..$n` in the SQL (`Placeholders.numbered` turns a
`?`-written program into that).

**What it buys, honestly.** On virtual threads a JDBC call parked
behind `Async.Run` costs what a reactive callback costs, so this is
not a speed road: the publishers are PULLED — a demand-driven
subscriber requests `fetchSize` rows and parks until they arrive.
What you get is the driver: the engines whose wire protocol okay will
not write.

**Where the SPI is thinner than JDBC.** R2DBC exposes column metadata
only with a row. `describe` therefore runs the statement asking for one
row and reads that row's metadata; a statement with parameters, or one
answering no rows, describes as EMPTY, and `Typed.verify` then names
every column missing. Verify against a populated table, or verify on
the JDBC driver of the same engine.

**Nullability is the driver's word.** H2's driver states it;
r2dbc-postgresql answers UNKNOWN for every column, so `verify` names
each non-Option field "nullable" on that engine. For Postgres with a
catalog-backed describe, use okay-pg.
