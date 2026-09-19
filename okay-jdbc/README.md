# okay-jdbc

Result sets as chunked async streams, constant memory at any size; connections under the Resource region — closed on the scope's end, handled aborts included. And, since sql-seam: the first DRIVER of the `Sql` seam (docs/modules/okay-sql.md) — `JdbcSql(conn)` puts the typed layer (rows/params/verify/transact) on top of any JDBC connection, exercised on H2 as a user with no DDL rights.

**Depends on:** `okay`, `okay-sql` (JVM); tests run on H2.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-jdbc.md`](../docs/modules/okay-jdbc.md) | what it is, and the reasoning |
| [`specs/sql.md`](../specs/sql.md) | the design and its decisions |
