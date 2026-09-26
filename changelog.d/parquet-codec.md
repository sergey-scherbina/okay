## parquet-codec — Parquet without Spark or Hadoop

New module okay-parquet (JVM, JS, Native): our Parquet codec over
okay-arrow's `Table` — flat schemas; data pages v1 and v2, dictionary
pages, PLAIN, the dictionary encodings and RLE booleans; Snappy, ZSTD or
none through `Compression` — read row group by row group from a
`ReadAt` (size and byte ranges: an array, a file, an S3 object) and
written by a writer that holds one row group. Behind a `ParquetCodec`
facade: ours the default, parquet-java behind `ParquetJava.given` over
an optional dependency, `Parquets.byName`; each reads the other's files
(TestParquetJava), pyarrow reads ours and ours reads pyarrow's across
page versions and codecs (TestParquetPyArrow). specs/parquet.md,
docs/modules/okay-parquet.md. Part of engine-object-store-io.
