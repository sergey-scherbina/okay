# okay-sql — the relational seam, without a mandatory JDBC

One small driver trait (`Sql`) moves statements, values and row
frames. Everything smart — typing, totality, verify, the transaction
region — lives ABOVE it, written once and cross-platform. There is no
`java.sql` anywhere, and that is asserted structurally: the module
cross-builds for JS and Native.

**Bind, don't model.** The database's schema is authoritative and SQL
stays the query language; what this adds is typed edges. Parameters
are bound from a case class by `Schema`, positionally, always through
the driver's prepared path — injection is unrepresentable. Rows are
decoded into a case class by `Schema` by column LABEL, camelCase to
snake_case, so a `SELECT *` whose columns get reordered cannot shear
the mapping.

**Damage is data.** A row that does not decode is `Bad(column, error,
row)` in the stream, never a throw.

## The pieces

| | |
|---|---|
| `trait Sql` | the driver seam: `query(sql, params)` moving frames |
| `Typed.rows[A]` / `Typed.rowsOf[A, P]` | a query as a stream of `A`, parameters bound from `P` |
| `Typed.transact` | the transaction as a `Resource` scope: commit on normal completion, `Sql.cancel()` from the finalizer otherwise; isolation declared, and `Granted` says what the engine actually gave |
| `verify` | the query's shape checked against the engine before a run, so a mismatch is not a surprise mid-stream |

## Reading rows into a case class

```scala
import okay.sql.*

final case class Person(name: String, age: Int)
given Schema[Person] = Schema.derived

Typed.rows[Person](db, "select name, age from people where age > ?",
  Vector(SqlValue.I32(18)))
```

A driver is chosen at the edge and nothing above it changes:
[`okay-jdbc`](../okay-jdbc) for H2, DuckDB and warehouses,
[`okay-pg`](../okay-pg) speaking the Postgres v3 wire cross-platform,
[`okay-r2dbc`](../okay-r2dbc) for the reactive drivers.

## Further

| | |
|---|---|
| [`docs/modules/okay-sql.md`](../docs/modules/okay-sql.md) | the guide: binding, totality, the transaction region |
| [`specs/sql.md`](../specs/sql.md) | the design and its decisions |
