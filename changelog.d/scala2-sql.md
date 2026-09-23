## scala2-sql - okay-sql from Scala 2.13: Db, with rows, statements, verify and transactions as Eff

The third of the five areas (HTTP, SQL, codecs, agents, UI).

- Probed from scalac 2.13.18: okay-sql's DATA is readable (`SqlValue`,
  `Bad`, `Drift`, `Isolation`, the `Sql` trait, okay-jdbc's `JdbcSql`).
  Every OPERATION answers a program, so none of them is usable from
  Scala 2.
- The new module okay-scala2-sql adds `Db`: `Db.jdbc(connection)` and
  `Db(sql)`, with
  - `rows`/`rowsOf` as a `Source[Either[Bad, A]]`;
  - `all`/`allOf` as `Eff[Async with Throws[Bad], Vector[A]]`, where the
    first undecodable row is a typed failure;
  - `update`/`updateOf`, `verify`, and `transaction` (okay-sql's
    `Typed.transact` under `Resource`).

  Each is one call into okay-sql's `Typed`. Rows are decoded by column
  label, and parameters are always bound.
- `TestSqlFromScala2` runs on in-memory H2 (now a test dependency of the
  2.13 probe): parameters and case-class binding, a `Bad` row both ways,
  commit and rollback, and `verify`. The probe has 60 tests. The two
  first-run failures were the test's own assumptions: H2 upper-cases
  names, and `verify` rightly flags a nullable column behind a
  non-Option field.
- The first full matrix went red, with "No suitable driver found" for
  H2, although the suite had passed alone. `DriverManager` scans once
  per JVM, and okay-jdbc's unforked suite had registered H2 from its
  own class loader first. The suite now connects through
  `org.h2.Driver` directly, and docs/scala2.md says why.
- Docs: section 8c of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia, and spec stage 8.
