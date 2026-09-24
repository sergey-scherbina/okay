# okay-scala2-sql

okay-sql for **Scala 2.13**. okay-sql's data types (`SqlValue`, `Bad`,
`Drift`, `Isolation`, the `Sql` driver trait, okay-jdbc's `JdbcSql`) are
readable from Scala 2 and used directly. Its operations answer programs,
which Scala 2 cannot compose, so this module provides them:

| | |
|---|---|
| `Db.jdbc(connection)` / `Db(sql)` | a database over JDBC, or over any okay-sql driver |
| `rows` / `rowsOf` | every row as a `Source[Either[Bad, A]]`, decoded by column label |
| `all` / `allOf` | every row as `Vector[A] ! (Async + Throws[Bad])` |
| `update` / `updateOf` | a statement, with `SqlValue` parameters or a case class's fields |
| `verify` | the `Drift` between a query's columns and `A` |
| `transaction` | commit on completion, roll back on failure |

The walkthrough is section 8c of
[okay from Scala 2.13](../scala2.md#8c-sql-queries-and-transactions), and
the signatures are in [okay-scala2](okay-scala2.md#api-reference).
