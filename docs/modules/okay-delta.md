# okay-delta

> Delta Lake without Spark: the Delta project's own kernel writes and
> scans tables from the seam's `SqlValue` rows, so the optimistic log
> commit is theirs to version (specs/data.md, lake-delta). Reads at
> scale stay the JDBC road — DuckDB's delta extension over the same
> files.

Depends on: `okay`, `okay-sql`, `delta-kernel-api` + `delta-kernel-defaults`
(the latter brings the Hadoop client for local files and object stores).

## Guide

**Write.** `Delta.create(path, columns)` makes an empty table (version 0)
from columns in the seam's vocabulary — Bool/I32/I64/F64/Num/Text/Bytes,
`nullable` per column; arrays and composites are refused by name (v1).
`Delta.append(path, rows)` is a blind append of `Chunk[Vector[SqlValue]]`
and answers the committed version.

**Land once.** `Delta.append(path, rows, loadId = Some(("my-app", n)))`
rides Delta's transaction identifier: the same `(app, version)` cannot
land twice, so a retried load is a refusal, not a duplicate — the
bulk-load posture's dedup in Delta's own words.

**Read.** `Delta.snapshot(path)` — the latest version and its schema as
`Col`s; `Delta.rows(path)` — the kernel's own full scan, for a reader
with no engine at hand. For queries, aggregation and streaming at
fetch-size, read the same table through the JDBC seam with DuckDB:

```scala
st.execute("INSTALL delta"); st.execute("LOAD delta")
Typed.rows[Reading](JdbcSql(conn), s"select ... from delta_scan('$path')")
```
