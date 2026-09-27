## bulk-parquet — a Parquet file as a `Bulk` source, no Spark reader

`Bulk.read(path, format)`: any file format read by its SPLITS, spread
with `of` and read by `flatMap` wherever a split lands — every `Bulk`
instance gets it by default, so a Spark executor reads its own row
groups. `Bulk.Format` is the serializable format. okay-parquet's
`ParquetFormat` / `ParquetFile.rows[A]`: row groups as splits, a group
decoded as `A` by its Schema and pruned at the reader to A's fields
(okay-parquet now depends on okay-stream). okay-arrow's `Rows` reads a
timestamp, duration or date column into a `Long` field (its raw value).
The NYC taxi demo (TestTaxiAlgebra, Live) now reads its month through
`Bulk.read` on Spark AND in one JVM on `localBulk`, equal per hour; its
2 318 848 trips equal pyarrow's count. TestParquetBulk (rows as
written, replayable, timestamps, pruning measured, a bad type refused
with file and group named); pruning mutant caught. specs/bulk.md,
docs/modules/okay-parquet.md.
