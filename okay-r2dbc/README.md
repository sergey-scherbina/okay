# okay-r2dbc

The R2DBC hatch of the `Sql` seam: any `io.r2dbc.spi.Connection` behind the same trait, so the typed layer (rows/params/verify/ transact) runs over MSSQL, Oracle, MySQL — any engine with a maintained reactive driver — unchanged (specs/sql.md).

**Depends on:** `okay`, `okay-sql` (JVM), `r2dbc-spi`. Tests: H2 through

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-r2dbc.md`](../docs/modules/okay-r2dbc.md) | what it is, and the reasoning |
| [`specs/sql.md`](../specs/sql.md) | the design and its decisions |
