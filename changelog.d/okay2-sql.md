## okay2-sql - the relational seam and its JDBC driver for okay2

Third of the modules the operator asked for (json, xml, sql, http):
okay-sql as `okay2-sql` (JVM, Scala.js, Scala Native) and okay-jdbc's
`JdbcSql` as `okay2-jdbc` (JVM). Spec stage 42, docs §34.

What is ported:
- the seam: `SqlValue`, `SqlType`, `Col`, `Isolation`, `Granted`,
  `Drift`, `Bad`, `Sql`;
- `Placeholders` and `Temporal`;
- `Typed`: row shapes, decode by label, `verify`, `rows`/`rowsOf`,
  `update`, `batchOf`, `transact`, `transactRetry`, and the
  `Db[Tx.No]`/`Db[Tx.Yes]` region;
- the `Tx` typestate over `Prog`;
- `Column`/`Row` over `HMap`;
- `Query`;
- `Pool`.

okay2-jdbc's suites run on H2 (as a user with no DDL rights) and SQLite:
TestTyped, TestSqlite, TestQuerySqlite (the law: the engine's rows for
every predicate are the in-memory ones), TestPool and TestRetry. The
okay2-sql suites are TestSqlPure, TestQueryPure, TestRow and TestTx.
Totals: 35 JVM results for okay2-jdbc, 27 per platform for okay2-sql.

Found along the way, in okay2-codec: a tuple was not derived, because
everything under `scala.` was skipped. The derivation macro now derives
`scala.TupleN` as Scala 3's Mirror does. The rest of okay-jdbc (Writes,
Migrate, Poll, SqlStore, BulkLoad, JdbcInterop) and the pg wire driver
are filed as `okay2-jdbc-tails`.
