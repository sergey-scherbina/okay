# okay-pg

The Postgres v3 wire, natively: the direct road the Sql seam exists for — no java.sql, no driver dependency, the protocol itself behind the same trait, so the typed layer (rows/verify/params/transact) runs over it unchanged (specs/sql.md).

**Depends on:** the core and `okay-sql`. JVM leg (a blocking socket

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-pg.md`](../docs/modules/okay-pg.md) | what it is, and the reasoning |
| [`specs/sql.md`](../specs/sql.md) | the design and its decisions |
