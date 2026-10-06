# okay-semantic-sql

An optional SQL interpreter for validated `okay-semantic` plans. Depends on the
semantic core and `okay-sql`; no JDBC or database vendor jar is required.

`Render(plan, Binding(table, dimensionColumns, measureColumns))` produces a
parameterized `Statement`. `Statement.execute(using Sql)` returns a program over
Async, with either named decode errors or the result. The caller chooses the
existing JDBC, PostgreSQL or other Sql backend.

Bindings are explicit ASCII identifiers and structured column references, quoted
in generated SQL. Schema-qualified
names, expressions and missing bindings are refused. Filter values are bound parameters; comparisons, membership and explicit null
filters have the same semantics as memory. SQL computes sums, row counts,
non-null counts, min/max and distinct counts. The shared semantic finalizer computes averages and
ratios, so integer division and averaging averages cannot change their meaning.

Result order depends on the backend; compare groups by their keys. Exact totals
must arrive as SqlValue.Num, I32 or I64; floating values are refused. Database
numeric precision still bounds the SQL aggregation. SQL columns must faithfully
represent the model's extractors; this is an application binding contract.

Explicit lookup bindings must match the model's declared relations and
cardinalities. Duplicate-key preflights reject fanout before LEFT JOIN. Use a
repeatable-read transaction or stable source snapshot when concurrent mutations
could occur between checks and aggregation. Having, ordering and pagination run
in the shared finalizer. Fixed time buckets derive from dimension metadata;
civil calendars need explicitly materialized columns or typed SQL row execution.
No arbitrary SQL fragments are accepted. Execution
collects aggregate result rows in memory; this adapter does not stream individual
result groups to its caller. See [the specification](../../specs/semantic.md).
