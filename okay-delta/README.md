# okay-delta

Delta Lake without Spark: the Delta project's own kernel writes and scans tables from the seam's `SqlValue` rows, so the optimistic log commit is theirs to version (specs/data.md, lake-delta). Reads at scale stay the JDBC road — DuckDB's delta extension over the same files.

**Depends on:** `okay`, `okay-sql`, `delta-kernel-api` + `delta-kernel-defaults`

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-delta.md`](../docs/modules/okay-delta.md) | what it is, and the reasoning |
| [`specs/data.md`](../specs/data.md) | the design and its decisions |
