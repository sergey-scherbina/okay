## okay2-jdbc-migrate - Migrate, BulkLoad and JdbcInterop on okay2

First lane of okay2-jdbc-tails (operator: "do everything needed for
okay2, all at once"). The three okay-jdbc pieces that need nothing of
okay-persist, ported to `okay2-jdbc` with their suites:

- `Migrate`: versioned scripts with sha-256 checksums in a version table;
  refuses duplicates, disorder, a CHANGED or VANISHED applied script; a
  failing script leaves no row (TestMigrate, 7 tests, H2).
- `BulkLoad.load` (load id + COPY in one transaction, `AlreadyLoaded` on
  the retry, a failing COPY rolls its claim back) and `BulkLoad.olap`
  (row DML refused by name) — TestBulkLoad, 3 tests, embedded DuckDB
  (`duckdb_jdbc` added at Test scope).
- `JdbcInterop`: a connection under the Resource region, a query as a
  chunked stream, a chunk per batch (TestJdbcInterop, 2 tests).

okay2-jdbc now depends on okay2-platform at compile scope: Migrate and
BulkLoad run a statement to its answer inside one Async operation, as
the Scala 3 originals do, and that parks on the platform's `CanBlock`.
Spec: specs/okay2.md stage 42, "okay2-jdbc-tails, lane 1"; docs §34.

Left (backlog okay2-jdbc-tails): Writes, Poll and SqlStore (they need an
okay2-persist log, whose typed view waits on okay2-codec's CBOR), okay-pg
and the Live pg suites.
